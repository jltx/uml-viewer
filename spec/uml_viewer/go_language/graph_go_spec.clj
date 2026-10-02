(ns uml-viewer.go-language.graph-go-spec
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [speclj.core :refer :all]
            [uml-viewer.application.ir-generator :as ir-generator]
            [uml-viewer.go-language.graph-go :as graph-go]
            [uml-viewer.go-language.go-spec-support :as support :refer [fixture-root temp-copy-of-fixture]]
            [uml-viewer.graph :as graph]))

(defmacro ^:private with-go [& body]
  `(support/when-go-installed "go not found; Go scan skipped" ~@body))

(defn- temp-module
  "A new directory holding the Go module `module-path`: its go.mod and
  `files`, a map of module-relative path to content."
  [module-path files]
  (let [module-root (io/file (System/getProperty "java.io.tmpdir")
                             (str "uml-go-" (System/nanoTime)))]
    (doseq [[path content] (assoc files "go.mod" (str "module " module-path "\n\ngo 1.21\n"))]
      (io/make-parents (io/file module-root path))
      (spit (io/file module-root path) content))
    module-root))

(defn- listed-as-cgo-file?
  "Does `go list`, with cgo switched on, report `file-name` among the CgoFiles
  of the module at `module-root`?"
  [module-root file-name]
  (let [builder (-> (ProcessBuilder. ["go" "list" "-json" "./..."])
                    (.directory module-root)
                    (.redirectError java.lang.ProcessBuilder$Redirect/DISCARD))]
    (.put (.environment builder) "CGO_ENABLED" "1")
    (boolean (re-find (re-pattern (str "\"CgoFiles\":\\s*\\[[^\\]]*\""
                                       (java.util.regex.Pattern/quote file-name)
                                       "\""))
                      (slurp (.getInputStream (.start builder)))))))

(defn- thrown-by [scan-thunk]
  (try (scan-thunk)
       nil
       (catch clojure.lang.ExceptionInfo failure failure)))

(def ^:private helper-process @#'graph-go/helper-process)

(defn- with-helper-environment
  "The result of `scan-thunk` when the helper process is started from an
  environment that also holds `extra-variables`."
  [extra-variables scan-thunk]
  (with-redefs-fn {#'graph-go/helper-process
                   (fn [go-opts environment]
                     (helper-process go-opts (merge environment extra-variables)))}
    scan-thunk))

(def ^:private non-host-goos
  (if (str/starts-with? (System/getProperty "os.name") "Windows") "linux" "windows"))

(describe "go scan facts"
  (it "reports the module's packages, each with its dotted namespace"
    (with-go
      (let [facts (graph-go/scan-facts fixture-root {:goos "linux"})]
        (should= "example.com/demo" (:module facts))
        (should= "linux" (:goos facts))
        (should= ["example.com.demo"
                  "example.com.demo.cmd.demo"
                  "example.com.demo.internal.util"
                  "example.com.demo.pkg.util"
                  "example.com.demo.store"]
                 (map :ns (:packages facts)))
        (should= ["store/open.go" "store/query.go" "store/store_linux.go"]
                 (:files (last (:packages facts)))))))

  (it "inherits GOOS from the environment when no go opts are given"
    (with-go
      (let [facts (graph-go/scan-facts fixture-root nil)]
        (should (seq (:goos facts)))
        (should= 5 (count (:packages facts))))))

  (it "scans for a GOOS set in the environment when no go opts are given"
    (with-go
      (let [facts (with-helper-environment {"GOOS" non-host-goos "GOARCH" "arm64"}
                    #(graph-go/scan-facts fixture-root nil))]
        (should= non-host-goos (:goos facts))
        (should-contain (str "store/store_" non-host-goos ".go")
                        (:files (last (:packages facts)))))))

  (it "builds the helper for the host and names the target GOOS and GOARCH as flags"
    (let [environment {"GOOS" "linux" "GOARCH" "arm64" "GOFLAGS" "-mod=mod"}
          target-flags #(drop 3 (:command (helper-process %1 %2)))]
      (should= {"GOFLAGS" "-mod=mod"} (:environment (helper-process nil environment)))
      (should= ["-goos" "linux" "-goarch" "arm64"] (target-flags nil environment))
      (should= ["-goos" "plan9" "-goarch" "arm64"] (target-flags {:goos "plan9"} environment))
      (should= [] (target-flags nil {"GOFLAGS" "-mod=mod"}))))

  (it "scans the files of a GOARCH set in the environment"
    (with-go
      (let [module-root (temp-module "example.com/archdemo"
                                     {"x_amd64.go" "package archdemo\n\nfunc OnAmd64() {}\n"
                                      "x_arm64.go" "package archdemo\n\nfunc OnArm64() {}\n"})
            archdemo-package #(-> (with-helper-environment {"GOARCH" %}
                                    (fn [] (graph-go/scan-facts module-root {:goos "linux"})))
                                  :packages
                                  first)]
        (should= ["x_arm64.go"] (:files (archdemo-package "arm64")))
        (should= ["OnArm64"] (map :name (:decls (archdemo-package "arm64"))))
        (should= ["x_amd64.go"] (:files (archdemo-package "amd64"))))))

  (it "throws naming the Go toolchain when the go program cannot be started"
    (let [failure (with-bindings {#'graph-go/*go-executable* "no-such-go-program"}
                    (thrown-by #(graph-go/scan-facts fixture-root nil)))]
      (should-not-be-nil failure)
      (should-contain "Go toolchain not found on PATH" (ex-message failure))
      (should-contain "no-such-go-program" (ex-message failure))
      (should (instance? java.io.IOException (ex-cause failure)))))

  (it "throws naming the module root when it is not a directory"
    (let [missing-root (io/file (System/getProperty "java.io.tmpdir")
                                (str "uml-go-no-such-module-" (System/nanoTime)))
          failure (thrown-by #(graph-go/scan-facts missing-root nil))]
      (should-not-be-nil failure)
      (should-contain "Go module root not found" (ex-message failure))
      (should-contain (str missing-root) (ex-message failure))
      (should-not-contain "toolchain" (ex-message failure))))

  (it "throws naming the module root when the root is a subdirectory of the module"
    (with-go
      (let [failure (thrown-by #(graph-go/scan-facts (io/file fixture-root "store") {:goos "linux"}))
            module-root (.getCanonicalPath (io/file fixture-root))]
        (should-not-be-nil failure)
        (should-contain "is not the module root" (ex-message failure))
        (should-contain (str/lower-case module-root) (str/lower-case (ex-message failure))))))

  (it "throws with the helper's stderr when the helper fails"
    (with-go
      (let [broken-root (temp-copy-of-fixture)]
        (spit (io/file broken-root "store/query.go") "package store\n\nfunc broken( {\n")
        (let [failure (thrown-by #(graph-go/scan-facts broken-root {:goos "linux"}))]
          (should-not-be-nil failure)
          (should-contain "store/query.go" (ex-message failure)))))))

(describe "temp copy of a module"
  (it "leaves out the .metrics and target directories that tool runs leave behind"
    (let [source-root (temp-module "example.com/demo"
                                   {"demo.go" "package demo
"
                                    "store/store.go" "package store
"
                                    ".metrics/crap.edn" "{}"
                                    ".metrics/mutate/store.edn" "{}"
                                    "target/leftover.txt" "x"
                                    "store/target/leftover.txt" "x"})
          copy-root (support/temp-copy-of source-root)
          copied-files (->> (file-seq copy-root)
                            (filter #(.isFile %))
                            (map #(str (.relativize (.toPath copy-root) (.toPath %))))
                            (map #(str/replace % "\\" "/"))
                            sort)]
      (should= ["demo.go" "go.mod" "store/store.go"] copied-files))))

(def ^:private linux-opts {:prefix "example.com.demo" :go {:goos "linux"}})

(def ^:private linux-scan
  (delay (graph/scan graph-go/impl fixture-root linux-opts)))

(defn- classes-by-id [scan]
  (into {} (map (juxt :id identity)) (:classes scan)))

(defn- edge-pairs [scan]
  (mapv (juxt :from :to) (:edges scan)))

(defn- op-named [scan class-id op-name]
  (first (filter #(= op-name (:name %)) (:ops ((classes-by-id scan) class-id)))))

(defn- line-starting-with [file declaration]
  (->> (str/split-lines (slurp file))
       (keep-indexed (fn [index line]
                       (when (str/starts-with? line declaration) (inc index))))
       first))

(describe "go graph"
  (it "registers as the :go scanner"
    (should= graph-go/impl (graph/lookup :go)))

  (it "makes one class per package, named by its package clause"
    (with-go
      (let [by-id (classes-by-id @linux-scan)]
        (should= #{:demo :store :internal.util :pkg.util :cmd.demo}
                 (set (map :id (remove :foreign (:classes @linux-scan)))))
        (should= {:id :demo
                  :name "demo"
                  :ns "example.com.demo"
                  :lang :go
                  :file "spec/fixtures/go/demo/demo.go"}
                 (dissoc (by-id :demo) :ops))
        (should= "main" (:name (by-id :cmd.demo)))
        (should= "example.com.demo.cmd.demo" (:ns (by-id :cmd.demo)))
        (should= "spec/fixtures/go/demo/store/open.go" (:file (by-id :store))))))

  (it "keeps two packages with the same name apart"
    (with-go
      (let [by-id (classes-by-id @linux-scan)]
        (should= "util" (:name (by-id :internal.util)))
        (should= "util" (:name (by-id :pkg.util)))
        (should-not= (:ns (by-id :internal.util)) (:ns (by-id :pkg.util))))))

  (it "makes a foreign class for each standard-library import"
    (with-go
      (let [foreign (filter :foreign (:classes @linux-scan))]
        (should= [{:id :std.net.http :name "net/http" :ns "std.net.http" :foreign true}
                  {:id :std.strings :name "strings" :ns "std.strings" :foreign true}]
                 foreign)
        (should-not (some #(str/includes? (:name %) "example.com/demo") foreign)))))

  (it "ids an import from another module by its dotted path"
    (with-go
      (let [copy-root (temp-copy-of-fixture)]
        (spit (io/file copy-root "go.mod")
              (str "module example.com/demo\n\ngo 1.21\n\n"
                   "require github.com/acme/lib v0.0.0\n\n"
                   "replace github.com/acme/lib => ./third_party/lib\n"))
        (io/make-parents (io/file copy-root "third_party/lib/go.mod"))
        (spit (io/file copy-root "third_party/lib/go.mod")
              "module github.com/acme/lib\n\ngo 1.21\n")
        (spit (io/file copy-root "third_party/lib/lib.go")
              "package lib\n\nfunc Polish() {}\n")
        (spit (io/file copy-root "store/polish.go")
              (str "package store\n\nimport \"github.com/acme/lib\"\n\n"
                   "func Polish() { lib.Polish() }\n"))
        (let [scan (graph/scan graph-go/impl copy-root linux-opts)]
          (should= {:id :github.com.acme.lib
                    :name "github.com/acme/lib"
                    :ns "github.com.acme.lib"
                    :foreign true}
                   ((classes-by-id scan) :github.com.acme.lib))
          (should-contain [:store :github.com.acme.lib] (edge-pairs scan))))))

  (it "makes one dependency edge per import, sorted by from and to"
    (with-go
      (should= [[:cmd.demo :pkg.util]
                [:cmd.demo :store]
                [:internal.util :std.strings]
                [:pkg.util :std.net.http]
                [:store :internal.util]
                [:store :std.strings]]
               (edge-pairs @linux-scan))
      (should= #{:dependency} (set (map :kind (:edges @linux-scan))))))

  (it "lists each declaration as an op with its file and line"
    (with-go
      (let [row-string (op-named @linux-scan :store "Row.String")]
        (should= ["Store" "Open" "Store.Close" "Row" "Cache" "Query" "Store.String"
                  "normalize" "Row.String" "Cache.Get" "platformName"]
                 (map :name (:ops ((classes-by-id @linux-scan) :store))))
        (should= "Row.String" (:text row-string))
        (should= "spec/fixtures/go/demo/store/query.go" (:file row-string))
        (should= (line-starting-with (io/file fixture-root "store/query.go")
                                     "func (r Row) String()")
                 (:line row-string)))))

  (it "marks unexported declarations private and spells types as types"
    (with-go
      (should= true (:private (op-named @linux-scan :store "normalize")))
      (should-not (contains? (op-named @linux-scan :store "Open") :private))
      (should= "type Store" (:text (op-named @linux-scan :store "Store")))))

  (it "follows an import that is removed and then restored"
    (with-go
      (let [copy-root (temp-copy-of-fixture)
            open-file (io/file copy-root "store/open.go")
            original (slurp open-file)
            first-scan (graph/scan graph-go/impl copy-root linux-opts)]
        (spit open-file (-> original
                            (str/replace "\"example.com/demo/internal/util\"" "")
                            (str/replace "util.Trim(strings.ToLower(name))"
                                         "strings.ToLower(name)")))
        (let [scan-without-import (graph/scan graph-go/impl copy-root linux-opts)]
          (should-contain [:store :internal.util] (edge-pairs first-scan))
          (should-not-contain [:store :internal.util] (edge-pairs scan-without-import)))
        (spit open-file original)
        (should= first-scan (graph/scan graph-go/impl copy-root linux-opts)))))

  (it "scans the files of the configured GOOS"
    (with-go
      (let [windows-scan (graph/scan graph-go/impl fixture-root
                                     (assoc linux-opts :go {:goos "windows"}))]
        (should-be-nil (op-named @linux-scan :store "windowsOnly"))
        (should= true (:private (op-named windows-scan :store "windowsOnly"))))))

  (it "lists the init function of each file as its own op"
    (with-go
      (let [module-root (temp-module "example.com/demo"
                                     {"boot.go" "package demo\n\nfunc init() {}\n"
                                      "wire.go" (str "package demo\n\nvar wired bool\n\n"
                                                     "func init() { wired = true }\n")})
            scan (graph/scan graph-go/impl module-root linux-opts)
            init-ops (filter #(= "init" (:name %)) (:ops ((classes-by-id scan) :demo)))]
        (should= ["boot.go" "wire.go"] (map #(.getName (io/file (:file %))) init-ops))
        (should= [3 5] (map :line init-ops)))))

  (it "lists a cgo file and makes nothing of its pseudo-import C"
    (with-go
      (let [module-root (temp-module "example.com/cgodemo"
                                     {"answer.go" (str "package cgodemo\n\n"
                                                       "/*\nint answer(void) { return 42; }\n*/\n"
                                                       "import \"C\"\n\n"
                                                       "func Answer() int { return int(C.answer()) }\n")})]
        (if (listed-as-cgo-file? module-root "answer.go")
          (let [cgodemo-package (-> (with-helper-environment {"CGO_ENABLED" "1"}
                                      #(graph-go/scan-facts module-root nil))
                                    :packages
                                    first)
                scan (with-helper-environment {"CGO_ENABLED" "1"}
                       #(graph/scan graph-go/impl module-root {:prefix "example.com.cgodemo"}))]
            (should= ["answer.go"] (:files cgodemo-package))
            (should= [] (:imports cgodemo-package))
            (should= [:cgodemo] (map :id (:classes scan)))
            (should= ["Answer"] (map :name (:ops (first (:classes scan)))))
            (should= [] (:edges scan)))
          (println "go list reports no cgo files here; cgo scan skipped")))))

  (it "throws naming the id and both packages when two packages share a class id"
    (with-go
      (let [module-root (temp-module "example.com/demo"
                                     {"demo.go" "package demo\n\nfunc Outer() {}\n"
                                      "demo/demo.go" "package demo\n\nfunc Inner() {}\n"})
            failure (thrown-by #(graph/scan graph-go/impl module-root linux-opts))]
        (should-not-be-nil failure)
        (should-contain "example.com/demo and example.com/demo/demo" (ex-message failure))
        (should-contain ":demo" (ex-message failure))
        (should= {:id :demo :import-paths ["example.com/demo" "example.com/demo/demo"]}
                 (ex-data failure)))))

  (it "throws naming the file that does not parse"
    (with-go
      (let [broken-root (temp-copy-of-fixture)]
        (spit (io/file broken-root "store/query.go") "package store\n\nfunc broken( {\n")
        (let [failure (thrown-by #(graph/scan graph-go/impl broken-root linux-opts))]
          (should-not-be-nil failure)
          (should-contain "store/query.go" (ex-message failure)))))))

(describe "go policy"
  (it "documents a module as a namespace tree with the standard library collapsed"
    (with-go
      (let [doc (ir-generator/document graph-go/impl
                                       {:lang :go
                                        :src fixture-root
                                        :prefix "example.com.demo"
                                        :go {:goos "linux"}
                                        :foreign ['std]})
            project-classes (remove :foreign (:classes doc))
            ops (mapcat :ops project-classes)]
        (should (:hierarchical doc))
        (should= [:std] (map :id (filter :foreign (:classes doc))))
        (should= #{:demo :store :internal.util :pkg.util :cmd.demo}
                 (set (map :id project-classes)))
        (should (every? #(seq (:ops %)) project-classes))
        (should (every? #(str/ends-with? (:file %) ".go") ops))
        (should (every? #(pos? (:line %)) ops))
        (should-contain "spec/fixtures/go/demo/store/store_linux.go" (map :file ops))
        (should-not-contain "windowsOnly" (map :name ops))))))
