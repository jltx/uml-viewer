// Command goscan reports one Go module's packages, imports, and top-level
// declarations as a single EDN map on stdout. Run it with the module root as
// the working directory.
package main

import (
	"bytes"
	"encoding/json"
	"errors"
	"flag"
	"fmt"
	"go/ast"
	"go/parser"
	"go/token"
	"io"
	"os"
	"os/exec"
	"path"
	"path/filepath"
	"sort"
	"strconv"
	"strings"
	"unicode"
	"unicode/utf8"
)

type moduleReport struct {
	Module   string
	Goos     string
	Packages []packageReport
}

type packageReport struct {
	ImportPath string
	Name       string
	Dir        string
	Files      []string
	Imports    []string
	Decls      []declReport
}

type declReport struct {
	Name     string
	Kind     string
	File     string
	Line     int
	Exported bool
	offset   int
}

type listedPackage struct {
	ImportPath string
	Name       string
	Dir        string
	GoFiles    []string
	CgoFiles   []string
	Imports    []string
	Module     *struct {
		Path string
		Dir  string
	}
	Error *struct {
		Err string
	}
}

func main() {
	goos := flag.String("goos", "", "target GOOS; empty inherits the environment")
	flag.Parse()

	report, err := scanModule(".", *goos)
	if err != nil {
		fmt.Fprintln(os.Stderr, err)
		os.Exit(1)
	}
	fmt.Print(formatEDN(report))
}

func scanModule(moduleDir, goos string) (moduleReport, error) {
	resolvedGoos, err := effectiveGoos(moduleDir, goos)
	if err != nil {
		return moduleReport{}, err
	}
	listed, err := listPackages(moduleDir, goos)
	if err != nil {
		return moduleReport{}, err
	}

	var problems []error
	for _, pkg := range listed {
		if pkg.Error != nil {
			problems = append(problems, fmt.Errorf("%s: %s", pkg.ImportPath, strings.TrimSpace(pkg.Error.Err)))
		}
	}
	if len(problems) > 0 {
		return moduleReport{}, errors.Join(problems...)
	}

	report := moduleReport{Goos: resolvedGoos}
	for _, pkg := range listed {
		if pkg.Module == nil {
			return moduleReport{}, fmt.Errorf("%s: package is not in a module", pkg.ImportPath)
		}
		report.Module = pkg.Module.Path
		sourceFiles := append(append([]string{}, pkg.GoFiles...), pkg.CgoFiles...)
		if len(sourceFiles) == 0 {
			continue
		}
		moduleRelativeDir, err := moduleRelativeSlashPath(pkg.Module.Dir, pkg.Dir)
		if err != nil {
			return moduleReport{}, err
		}
		scanned := packageReport{
			ImportPath: pkg.ImportPath,
			Name:       pkg.Name,
			Dir:        moduleRelativeDir,
			Imports:    importsWithoutCgoPseudoPackage(pkg.Imports),
		}
		for _, fileName := range sourceFiles {
			scanned.Files = append(scanned.Files, path.Join(moduleRelativeDir, fileName))
		}
		sort.Strings(scanned.Files)
		for _, relativeFile := range scanned.Files {
			decls, err := parseDecls(pkg.Module.Dir, relativeFile)
			if err != nil {
				problems = append(problems, err)
				continue
			}
			scanned.Decls = append(scanned.Decls, decls...)
		}
		sortDecls(scanned.Decls)
		report.Packages = append(report.Packages, scanned)
	}
	if len(problems) > 0 {
		return moduleReport{}, errors.Join(problems...)
	}
	if report.Module == "" {
		return moduleReport{}, errors.New("go list reported no packages")
	}
	sort.Slice(report.Packages, func(left, right int) bool {
		return report.Packages[left].ImportPath < report.Packages[right].ImportPath
	})
	return report, nil
}

func runGo(moduleDir, goos string, args ...string) ([]byte, error) {
	command := exec.Command("go", args...)
	command.Dir = moduleDir
	command.Env = os.Environ()
	if goos != "" {
		command.Env = append(command.Env, "GOOS="+goos)
	}
	var stdout, stderr bytes.Buffer
	command.Stdout, command.Stderr = &stdout, &stderr
	if err := command.Run(); err != nil {
		return nil, fmt.Errorf("go %s: %w\n%s", strings.Join(args, " "), err, strings.TrimSpace(stderr.String()))
	}
	return stdout.Bytes(), nil
}

func effectiveGoos(moduleDir, goos string) (string, error) {
	output, err := runGo(moduleDir, goos, "env", "GOOS")
	if err != nil {
		return "", err
	}
	return strings.TrimSpace(string(output)), nil
}

func listPackages(moduleDir, goos string) ([]listedPackage, error) {
	output, err := runGo(moduleDir, goos, "list", "-e", "-json", "./...")
	if err != nil {
		return nil, err
	}
	var packages []listedPackage
	decoder := json.NewDecoder(bytes.NewReader(output))
	for {
		var pkg listedPackage
		err := decoder.Decode(&pkg)
		if errors.Is(err, io.EOF) {
			return packages, nil
		}
		if err != nil {
			return nil, fmt.Errorf("decoding go list output: %w", err)
		}
		packages = append(packages, pkg)
	}
}

func moduleRelativeSlashPath(moduleDir, packageDir string) (string, error) {
	relative, err := filepath.Rel(moduleDir, packageDir)
	if err != nil {
		return "", err
	}
	return filepath.ToSlash(relative), nil
}

func importsWithoutCgoPseudoPackage(imports []string) []string {
	kept := []string{}
	for _, imported := range imports {
		if imported != "C" {
			kept = append(kept, imported)
		}
	}
	sort.Strings(kept)
	return kept
}

func parseDecls(moduleDir, relativeFile string) ([]declReport, error) {
	source, err := os.ReadFile(filepath.Join(moduleDir, filepath.FromSlash(relativeFile)))
	if err != nil {
		return nil, err
	}
	fileSet := token.NewFileSet()
	parsed, err := parser.ParseFile(fileSet, relativeFile, source, parser.SkipObjectResolution)
	if err != nil {
		return nil, err
	}

	var decls []declReport
	emit := func(name, kind string, identifier *ast.Ident) {
		// physical position: //line directives in generated code name another file and line
		position := fileSet.PositionFor(identifier.Pos(), false)
		decls = append(decls, declReport{
			Name:     name,
			Kind:     kind,
			File:     relativeFile,
			Line:     position.Line,
			Exported: startsWithUpperCase(lastIdentifier(name)),
			offset:   position.Offset,
		})
	}
	for _, declaration := range parsed.Decls {
		switch typed := declaration.(type) {
		case *ast.FuncDecl:
			if typed.Name.Name == "_" {
				continue
			}
			if typed.Recv == nil || len(typed.Recv.List) == 0 {
				emit(typed.Name.Name, "func", typed.Name)
				continue
			}
			emit(receiverTypeName(typed.Recv.List[0].Type)+"."+typed.Name.Name, "method", typed.Name)
		case *ast.GenDecl:
			if typed.Tok != token.TYPE {
				continue
			}
			for _, spec := range typed.Specs {
				typeSpec := spec.(*ast.TypeSpec)
				emit(typeSpec.Name.Name, "type", typeSpec.Name)
			}
		}
	}
	return decls, nil
}

func receiverTypeName(expression ast.Expr) string {
	for {
		switch typed := expression.(type) {
		case *ast.StarExpr:
			expression = typed.X
		case *ast.ParenExpr:
			expression = typed.X
		case *ast.IndexExpr:
			expression = typed.X
		case *ast.IndexListExpr:
			expression = typed.X
		case *ast.Ident:
			return typed.Name
		default:
			return ""
		}
	}
}

func lastIdentifier(name string) string {
	return name[strings.LastIndex(name, ".")+1:]
}

func startsWithUpperCase(identifier string) bool {
	first, _ := utf8.DecodeRuneInString(identifier)
	return unicode.IsUpper(first)
}

func sortDecls(decls []declReport) {
	sort.Slice(decls, func(left, right int) bool {
		a, b := decls[left], decls[right]
		if a.File != b.File {
			return a.File < b.File
		}
		return a.offset < b.offset
	})
}

func formatEDN(report moduleReport) string {
	var out strings.Builder
	out.WriteString("{:module " + ednString(report.Module) + "\n")
	out.WriteString(" :goos " + ednString(report.Goos) + "\n")
	out.WriteString(" :packages\n [")
	for index, pkg := range report.Packages {
		if index > 0 {
			out.WriteString("\n  ")
		}
		writePackage(&out, pkg)
	}
	out.WriteString("]}\n")
	return out.String()
}

func writePackage(out *strings.Builder, pkg packageReport) {
	out.WriteString("{:import-path " + ednString(pkg.ImportPath) + "\n")
	out.WriteString("   :name " + ednString(pkg.Name) + "\n")
	out.WriteString("   :dir " + ednString(pkg.Dir) + "\n")
	out.WriteString("   :files " + ednStringVector(pkg.Files) + "\n")
	out.WriteString("   :imports " + ednStringVector(pkg.Imports) + "\n")
	out.WriteString("   :decls [")
	for index, decl := range pkg.Decls {
		if index > 0 {
			out.WriteString("\n           ")
		}
		out.WriteString("{:name " + ednString(decl.Name) +
			" :kind :" + decl.Kind +
			" :file " + ednString(decl.File) +
			" :line " + strconv.Itoa(decl.Line) +
			" :exported " + strconv.FormatBool(decl.Exported) + "}")
	}
	out.WriteString("]}")
}

func ednStringVector(values []string) string {
	quoted := make([]string, len(values))
	for index, value := range values {
		quoted[index] = ednString(value)
	}
	return "[" + strings.Join(quoted, " ") + "]"
}

func ednString(value string) string {
	escaped := strings.ReplaceAll(value, `\`, `\\`)
	escaped = strings.ReplaceAll(escaped, `"`, `\"`)
	return `"` + escaped + `"`
}
