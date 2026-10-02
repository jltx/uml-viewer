package main

import (
	"bytes"
	"errors"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"
)

const fixtureRelativePath = "../../../../spec/fixtures/go/demo"

func fixtureDir(t *testing.T) string {
	t.Helper()
	absolute, err := filepath.Abs(fixtureRelativePath)
	if err != nil {
		t.Fatal(err)
	}
	return absolute
}

func copyFixtureToTemp(t *testing.T) string {
	t.Helper()
	source := fixtureDir(t)
	destination := t.TempDir()
	err := filepath.WalkDir(source, func(path string, entry os.DirEntry, walkErr error) error {
		if walkErr != nil {
			return walkErr
		}
		relative, err := filepath.Rel(source, path)
		if err != nil {
			return err
		}
		target := filepath.Join(destination, relative)
		if entry.IsDir() {
			return os.MkdirAll(target, 0o755)
		}
		content, err := os.ReadFile(path)
		if err != nil {
			return err
		}
		return os.WriteFile(target, content, 0o644)
	})
	if err != nil {
		t.Fatal(err)
	}
	return destination
}

func scanFixture(t *testing.T, goos string) moduleReport {
	t.Helper()
	report, err := scanModule(fixtureDir(t), goos, "")
	if err != nil {
		t.Fatalf("scanModule(%q): %v", goos, err)
	}
	return report
}

func findPackage(t *testing.T, report moduleReport, importPath string) packageReport {
	t.Helper()
	for _, candidate := range report.Packages {
		if candidate.ImportPath == importPath {
			return candidate
		}
	}
	t.Fatalf("package %q not found", importPath)
	return packageReport{}
}

func findDecls(pkg packageReport, name string) []declReport {
	var found []declReport
	for _, decl := range pkg.Decls {
		if decl.Name == name {
			found = append(found, decl)
		}
	}
	return found
}

func findDecl(t *testing.T, pkg packageReport, name string) declReport {
	t.Helper()
	found := findDecls(pkg, name)
	if len(found) != 1 {
		t.Fatalf("want exactly one decl %q in %s, got %d", name, pkg.ImportPath, len(found))
	}
	return found[0]
}

func equalStrings(left, right []string) bool {
	return strings.Join(left, "\x00") == strings.Join(right, "\x00")
}

func writeFile(t *testing.T, path, content string) {
	t.Helper()
	if err := os.MkdirAll(filepath.Dir(path), 0o755); err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(path, []byte(content), 0o644); err != nil {
		t.Fatal(err)
	}
}

func appendToFile(t *testing.T, path, content string) {
	t.Helper()
	existing, err := os.ReadFile(path)
	if err != nil {
		t.Fatal(err)
	}
	writeFile(t, path, string(existing)+content)
}

func TestLinuxStorePackageFilesAndDecls(t *testing.T) {
	report := scanFixture(t, "linux")
	if report.Goos != "linux" || report.Module != "example.com/demo" {
		t.Fatalf("unexpected header: %q %q", report.Module, report.Goos)
	}
	store := findPackage(t, report, "example.com/demo/store")
	wantFiles := []string{"store/open.go", "store/query.go", "store/store_linux.go"}
	if !equalStrings(store.Files, wantFiles) {
		t.Fatalf("files = %v, want %v", store.Files, wantFiles)
	}
	if store.Name != "store" || store.Dir != "store" {
		t.Fatalf("name/dir = %q %q", store.Name, store.Dir)
	}
	wantImports := []string{"example.com/demo/internal/util", "strings"}
	if !equalStrings(store.Imports, wantImports) {
		t.Fatalf("imports = %v, want %v", store.Imports, wantImports)
	}
	for _, absent := range []string{"windowsOnly", "TestOpen"} {
		if len(findDecls(store, absent)) != 0 {
			t.Fatalf("decl %q must be absent on linux", absent)
		}
	}
	if platform := findDecl(t, store, "platformName"); platform.File != "store/store_linux.go" {
		t.Fatalf("platformName file = %q", platform.File)
	}
}

func TestWindowsStorePackageIncludesWindowsFile(t *testing.T) {
	store := findPackage(t, scanFixture(t, "windows"), "example.com/demo/store")
	wantFiles := []string{"store/open.go", "store/query.go", "store/store_windows.go"}
	if !equalStrings(store.Files, wantFiles) {
		t.Fatalf("files = %v, want %v", store.Files, wantFiles)
	}
	findDecl(t, store, "windowsOnly")
}

func TestEmptyGoosInheritsEnvironment(t *testing.T) {
	t.Setenv("GOOS", "darwin")
	if got := scanFixture(t, "").Goos; got != "darwin" {
		t.Fatalf("goos = %q, want darwin", got)
	}
}

func TestDeclKindsNamesAndExportedFlags(t *testing.T) {
	store := findPackage(t, scanFixture(t, "linux"), "example.com/demo/store")

	storeString := findDecl(t, store, "Store.String")
	rowString := findDecl(t, store, "Row.String")
	if storeString.File != "store/query.go" || rowString.File != "store/query.go" {
		t.Fatalf("String files = %q %q", storeString.File, rowString.File)
	}
	if storeString.Line == rowString.Line {
		t.Fatalf("Store.String and Row.String share line %d", storeString.Line)
	}

	cacheGet := findDecl(t, store, "Cache.Get")
	if cacheGet.Kind != "method" || !cacheGet.Exported {
		t.Fatalf("Cache.Get = %+v", cacheGet)
	}
	if closeMethod := findDecl(t, store, "Store.Close"); closeMethod.Kind != "method" {
		t.Fatalf("Store.Close kind = %q", closeMethod.Kind)
	}
	if normalize := findDecl(t, store, "normalize"); normalize.Exported || normalize.Kind != "func" {
		t.Fatalf("normalize = %+v", normalize)
	}
	if cache := findDecl(t, store, "Cache"); cache.Kind != "type" || !cache.Exported {
		t.Fatalf("Cache = %+v", cache)
	}
	if open := findDecl(t, store, "Open"); open.Kind != "func" || open.File != "store/open.go" || open.Line != 13 {
		t.Fatalf("Open = %+v, want a func on line 13 of store/open.go", open)
	}
}

func TestRootPackageAndDuplicateUtilNames(t *testing.T) {
	report := scanFixture(t, "linux")
	root := findPackage(t, report, "example.com/demo")
	if root.Dir != "." || !equalStrings(root.Files, []string{"demo.go"}) {
		t.Fatalf("root = %+v", root)
	}
	findPackage(t, report, "example.com/demo/internal/util")
	pkgUtil := findPackage(t, report, "example.com/demo/pkg/util")
	if !equalStrings(pkgUtil.Imports, []string{"net/http"}) {
		t.Fatalf("pkg/util imports = %v", pkgUtil.Imports)
	}
	for _, pkg := range report.Packages {
		if strings.Contains(pkg.ImportPath, "e2e") {
			t.Fatalf("test-only package %q must be omitted", pkg.ImportPath)
		}
	}
}

func TestPackagesAreSortedByImportPath(t *testing.T) {
	report := scanFixture(t, "linux")
	for index := 1; index < len(report.Packages); index++ {
		if report.Packages[index-1].ImportPath >= report.Packages[index].ImportPath {
			t.Fatalf("packages out of order at %d: %q >= %q", index,
				report.Packages[index-1].ImportPath, report.Packages[index].ImportPath)
		}
	}
}

func TestDeclsAreSortedByFileThenLine(t *testing.T) {
	store := findPackage(t, scanFixture(t, "linux"), "example.com/demo/store")
	for index := 1; index < len(store.Decls); index++ {
		previous, current := store.Decls[index-1], store.Decls[index]
		if previous.File > current.File || (previous.File == current.File && previous.Line > current.Line) {
			t.Fatalf("decls out of order: %+v before %+v", previous, current)
		}
	}
}

func TestDeclsOnTheSameLineKeepSourceOrder(t *testing.T) {
	moduleDir := copyFixtureToTemp(t)
	writeFile(t, filepath.Join(moduleDir, "demo_same_line.go"),
		"package demo\n\nfunc Zeta() {}; func Alpha() {}\n")
	root := findPackage(t, mustScan(t, moduleDir, "linux"), "example.com/demo")
	var names []string
	for _, decl := range root.Decls {
		if decl.File == "demo_same_line.go" {
			names = append(names, decl.Name)
		}
	}
	if !equalStrings(names, []string{"Zeta", "Alpha"}) {
		t.Fatalf("same-line decl order = %v", names)
	}
}

func TestLineDirectivesDoNotMoveDeclarations(t *testing.T) {
	moduleDir := copyFixtureToTemp(t)
	writeFile(t, filepath.Join(moduleDir, "demo_generated.go"),
		"package demo\n\nfunc First() {}\n\n//line grammar.y:900\nfunc Second() {}\n\n//line grammar.y:5\nfunc Third() {}\n")
	root := findPackage(t, mustScan(t, moduleDir, "linux"), "example.com/demo")
	var names []string
	var lines []int
	for _, decl := range root.Decls {
		if decl.File == "demo_generated.go" {
			names = append(names, decl.Name)
			lines = append(lines, decl.Line)
		}
	}
	if !equalStrings(names, []string{"First", "Second", "Third"}) {
		t.Fatalf("decls under the file's real path = %v", names)
	}
	if lines[0] != 3 || lines[1] != 6 || lines[2] != 9 {
		t.Fatalf("lines = %v, want the physical lines [3 6 9]", lines)
	}
}

func TestPackageWhoseFilesAreAllExcludedIsOmitted(t *testing.T) {
	moduleDir := copyFixtureToTemp(t)
	writeFile(t, filepath.Join(moduleDir, "extra", "extra_windows.go"),
		"//go:build windows\n\npackage extra\n\nfunc Extra() {}\n")
	for _, pkg := range mustScan(t, moduleDir, "linux").Packages {
		if strings.HasSuffix(pkg.ImportPath, "/extra") {
			t.Fatalf("package %q has no files for linux and must be omitted", pkg.ImportPath)
		}
	}
}

func TestBlankFunctionsAreNotEmitted(t *testing.T) {
	moduleDir := copyFixtureToTemp(t)
	writeFile(t, filepath.Join(moduleDir, "demo_blank.go"),
		"package demo\n\nfunc _() {}\n\ntype Blank struct{}\n\nfunc (Blank) _() {}\n")
	root := findPackage(t, mustScan(t, moduleDir, "linux"), "example.com/demo")
	for _, decl := range root.Decls {
		if decl.Name == "_" || strings.HasSuffix(decl.Name, "._") {
			t.Fatalf("blank decl emitted: %+v", decl)
		}
	}
	findDecl(t, root, "Blank")
}

func TestPseudoImportCIsOmitted(t *testing.T) {
	moduleDir := copyFixtureToTemp(t)
	writeFile(t, filepath.Join(moduleDir, "demo_cgo.go"),
		"package demo\n\n/*\nint answer(void) { return 42; }\n*/\nimport \"C\"\n\nfunc Answer() int { return int(C.answer()) }\n")
	t.Setenv("CGO_ENABLED", "1")
	root := findPackage(t, mustScan(t, moduleDir, "linux"), "example.com/demo")
	if !equalStrings(root.Files, []string{"demo.go", "demo_cgo.go"}) {
		t.Fatalf("cgo file missing from files: %v", root.Files)
	}
	for _, imported := range root.Imports {
		if imported == "C" {
			t.Fatal(`pseudo-import "C" must be omitted`)
		}
	}
}

func TestFunctionBodySyntaxErrorFailsWithFileName(t *testing.T) {
	moduleDir := copyFixtureToTemp(t)
	appendToFile(t, filepath.Join(moduleDir, "store", "query.go"), "\nfunc broken() {\n\tx := \n}\n")
	_, err := scanModule(moduleDir, "linux", "")
	if err == nil {
		t.Fatal("want an error for a syntax error in a function body")
	}
	if !strings.Contains(err.Error(), "store/query.go") {
		t.Fatalf("error does not name the file: %v", err)
	}
}

func TestImportSyntaxErrorFails(t *testing.T) {
	moduleDir := copyFixtureToTemp(t)
	writeFile(t, filepath.Join(moduleDir, "demo_bad.go"), "package demo\n\nimport (\n")
	if _, err := scanModule(moduleDir, "linux", ""); err == nil {
		t.Fatal("want an error for a syntax error in the import section")
	}
}

func TestDirectoryWithoutGoModFails(t *testing.T) {
	emptyDir := t.TempDir()
	writeFile(t, filepath.Join(emptyDir, "main.go"), "package main\n\nfunc main() {}\n")
	if _, err := scanModule(emptyDir, "linux", ""); err == nil {
		t.Fatal("want an error when the directory has no go.mod")
	}
}

func resolvedFixtureDir(t *testing.T) string {
	t.Helper()
	resolved, err := filepath.EvalSymlinks(fixtureDir(t))
	if err != nil {
		t.Fatal(err)
	}
	return resolved
}

func TestScanInModuleSubdirectoryFailsNamingTheModuleRoot(t *testing.T) {
	moduleRoot := resolvedFixtureDir(t)
	_, err := scanModule(filepath.Join(fixtureDir(t), "store"), "linux", "")
	if err == nil {
		t.Fatal("want an error when the directory is a subdirectory of the module")
	}
	if !strings.Contains(strings.ToLower(err.Error()), strings.ToLower(moduleRoot)) {
		t.Fatalf("error does not name the module root %q: %v", moduleRoot, err)
	}
}

func TestProgramInModuleSubdirectoryExitsOneNamingTheModuleRoot(t *testing.T) {
	moduleRoot := resolvedFixtureDir(t)
	stdout, stderr, err := runProgram(t, filepath.Join(fixtureDir(t), "store"), "-goos", "linux")
	var exitErr *exec.ExitError
	if !errors.As(err, &exitErr) || exitErr.ExitCode() != 1 {
		t.Fatalf("want exit code 1, got %v", err)
	}
	if len(stdout) != 0 {
		t.Fatalf("stdout must be empty on failure, got %q", stdout)
	}
	if !strings.Contains(strings.ToLower(string(stderr)), strings.ToLower(moduleRoot)) {
		t.Fatalf("stderr does not name the module root %q: %s", moduleRoot, stderr)
	}
}

func TestCanonicalDirFallsBackToTheCleanedAbsolutePathWhenSymlinksCannotBeResolved(t *testing.T) {
	missing := filepath.Join(t.TempDir(), "no-such-dir")
	got, err := canonicalDir(missing + string(filepath.Separator) + ".")
	if err != nil {
		t.Fatalf("canonicalDir(%q): %v", missing, err)
	}
	if got != missing {
		t.Fatalf("canonicalDir = %q, want %q", got, missing)
	}
}

func mustScan(t *testing.T, moduleDir, goos string) moduleReport {
	t.Helper()
	report, err := scanModule(moduleDir, goos, "")
	if err != nil {
		t.Fatalf("scanModule(%q): %v", moduleDir, err)
	}
	return report
}

func TestEDNEscapesQuotesAndBackslashes(t *testing.T) {
	report := moduleReport{
		Module: `ex"ample\x`,
		Goos:   "linux",
		Packages: []packageReport{{
			ImportPath: "a/b", Name: "b", Dir: "b",
			Files:   []string{"b/b.go"},
			Imports: []string{"fmt"},
			Decls:   []declReport{{Name: "F", Kind: "func", File: "b/b.go", Line: 3, Exported: true}},
		}},
	}
	want := "{:module \"ex\\\"ample\\\\x\"\n" +
		" :goos \"linux\"\n" +
		" :packages\n" +
		" [{:import-path \"a/b\"\n" +
		"   :name \"b\"\n" +
		"   :dir \"b\"\n" +
		"   :files [\"b/b.go\"]\n" +
		"   :imports [\"fmt\"]\n" +
		"   :decls [{:name \"F\" :kind :func :file \"b/b.go\" :line 3 :exported true}]}]}\n"
	if got := formatEDN(report); got != want {
		t.Fatalf("formatEDN mismatch\n got: %q\nwant: %q", got, want)
	}
}

func TestEDNPrintsEmptyListsAsEmptyVectors(t *testing.T) {
	report := moduleReport{Module: "m", Goos: "linux", Packages: []packageReport{{
		ImportPath: "m", Name: "m", Dir: ".", Files: []string{"m.go"},
	}}}
	got := formatEDN(report)
	if !strings.Contains(got, ":imports []") || !strings.Contains(got, ":decls []") {
		t.Fatalf("empty lists must print as []: %s", got)
	}
}

func runProgram(t *testing.T, workingDir string, args ...string) (stdout, stderr []byte, err error) {
	t.Helper()
	mainPath, pathErr := filepath.Abs("main.go")
	if pathErr != nil {
		t.Fatal(pathErr)
	}
	command := exec.Command("go", append([]string{"run", mainPath}, args...)...)
	command.Dir = workingDir
	var stdoutBuffer, stderrBuffer bytes.Buffer
	command.Stdout, command.Stderr = &stdoutBuffer, &stderrBuffer
	err = command.Run()
	return stdoutBuffer.Bytes(), stderrBuffer.Bytes(), err
}

func TestProgramOutputIsByteIdenticalAcrossRuns(t *testing.T) {
	first, firstStderr, err := runProgram(t, fixtureDir(t), "-goos", "linux")
	if err != nil {
		t.Fatalf("first run: %v\n%s", err, firstStderr)
	}
	second, _, err := runProgram(t, fixtureDir(t), "-goos", "linux")
	if err != nil {
		t.Fatalf("second run: %v", err)
	}
	if !bytes.Equal(first, second) {
		t.Fatal("two runs produced different stdout")
	}
	if !bytes.HasPrefix(first, []byte("{:module \"example.com/demo\"\n :goos \"linux\"\n")) {
		t.Fatalf("unexpected output start: %.80s", first)
	}
}

func architectureModule(t *testing.T) string {
	t.Helper()
	moduleDir := t.TempDir()
	writeFile(t, filepath.Join(moduleDir, "go.mod"), "module example.com/archdemo\n\ngo 1.21\n")
	writeFile(t, filepath.Join(moduleDir, "x_amd64.go"), "package archdemo\n\nfunc OnAmd64() {}\n")
	writeFile(t, filepath.Join(moduleDir, "x_arm64.go"), "package archdemo\n\nfunc OnArm64() {}\n")
	return moduleDir
}

func TestProgramGoarchFlagSelectsArchitectureFiles(t *testing.T) {
	moduleDir := architectureModule(t)
	for goarch, wantDecl := range map[string]string{"arm64": "OnArm64", "amd64": "OnAmd64"} {
		stdout, stderr, err := runProgram(t, moduleDir, "-goos", "linux", "-goarch", goarch)
		if err != nil {
			t.Fatalf("-goarch %s: %v\n%s", goarch, err, stderr)
		}
		wantDecls := `:decls [{:name "` + wantDecl + `" :kind :func :file "x_` + goarch + `.go" :line 3 :exported true}]}`
		if !strings.Contains(string(stdout), wantDecls) {
			t.Fatalf("-goarch %s: want only %s, got:\n%s", goarch, wantDecl, stdout)
		}
	}
}

func TestEmptyGoarchInheritsEnvironment(t *testing.T) {
	moduleDir := architectureModule(t)
	t.Setenv("GOARCH", "arm64")
	report, err := scanModule(moduleDir, "linux", "")
	if err != nil {
		t.Fatal(err)
	}
	archdemo := findPackage(t, report, "example.com/archdemo")
	if !equalStrings(archdemo.Files, []string{"x_arm64.go"}) {
		t.Fatalf("files = %v, want the arm64 file of the environment's GOARCH", archdemo.Files)
	}
}

func TestProgramExitsOneAndPrintsProblemToStderr(t *testing.T) {
	moduleDir := copyFixtureToTemp(t)
	appendToFile(t, filepath.Join(moduleDir, "store", "query.go"), "\nfunc broken() {\n\tx := \n}\n")
	stdout, stderr, err := runProgram(t, moduleDir, "-goos", "linux")
	var exitErr *exec.ExitError
	if !errors.As(err, &exitErr) || exitErr.ExitCode() != 1 {
		t.Fatalf("want exit code 1, got %v", err)
	}
	if len(stdout) != 0 {
		t.Fatalf("stdout must be empty on failure, got %q", stdout)
	}
	if !strings.Contains(string(stderr), "store/query.go") {
		t.Fatalf("stderr does not name the file: %s", stderr)
	}
}
