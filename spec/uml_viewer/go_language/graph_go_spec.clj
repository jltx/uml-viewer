(ns uml-viewer.go-language.graph-go-spec
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [speclj.core :refer :all]
            [uml-viewer.application.ir-generator :as ir-generator]
            [uml-viewer.go-language.graph-go :as graph-go]
            [uml-viewer.graph :as graph]))

(def ^:private fixture-root "spec/fixtures/go/demo")

(def ^:private go-installed?
  (delay (try (zero? (-> (ProcessBuilder. ["go" "version"])
                         (.redirectOutput java.lang.ProcessBuilder$Redirect/DISCARD)
                         (.redirectError java.lang.ProcessBuilder$Redirect/DISCARD)
                         .start
                         .waitFor))
              (catch java.io.IOException _ false))))

(defmacro ^:private with-go [& body]
  `(if @go-installed?
     (do ~@body)
     (println "go not found; Go scan skipped")))

(defn- temp-copy-of-fixture []
  (let [fixture (io/file fixture-root)
        copy-root (io/file (System/getProperty "java.io.tmpdir")
                           (str "uml-go-" (System/nanoTime)))]
    (doseq [file (file-seq fixture)
            :when (.isFile file)]
      (let [copy (io/file copy-root (str (.relativize (.toPath fixture) (.toPath file))))]
        (io/make-parents copy)
        (io/copy file copy)))
    copy-root))

(defn- thrown-by [scan-thunk]
  (try (scan-thunk)
       nil
       (catch clojure.lang.ExceptionInfo failure failure)))

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

  (it "throws with the helper's stderr when the helper fails"
    (with-go
      (let [broken-root (temp-copy-of-fixture)]
        (spit (io/file broken-root "store/query.go") "package store\n\nfunc broken( {\n")
        (let [failure (thrown-by #(graph-go/scan-facts broken-root {:goos "linux"}))]
          (should-not-be-nil failure)
          (should-contain "store/query.go" (ex-message failure)))))))

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
