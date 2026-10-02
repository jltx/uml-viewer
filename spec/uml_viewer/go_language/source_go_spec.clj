(ns uml-viewer.go-language.source-go-spec
  (:require [clojure.string :as str]
            [speclj.core :refer :all]
            [uml-viewer.application.detail :as detail]
            [uml-viewer.go-language.graph-go :as graph-go]
            [uml-viewer.go-language.source-go :as source-go]
            [uml-viewer.graph :as graph]
            [uml-viewer.source :as source]))

(def ^:private fixture-root "spec/fixtures/go/demo")
(def ^:private query-file (str fixture-root "/store/query.go"))

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
     (println "go not found; Go source navigation skipped")))

(defn- line-starting-with [file declaration]
  (->> (str/split-lines (slurp file))
       (keep-indexed (fn [index line]
                       (when (str/starts-with? line declaration) (inc index))))
       first))

(describe "go source"
  (it "registers as the :go source"
    (should= source-go/impl (source/lookup :go)))

  (it "opens the file named on the ident at the ident's line"
    (let [line (line-starting-with query-file "func Query")
          found (source/member-source :go {:name "Query"
                                           :file query-file
                                           :line line
                                           :lang :go})]
      (should (str/ends-with? (:file found) "store/query.go"))
      (should= line (:line found))
      (should= :go (:lang found))
      (should (str/starts-with? (:body found) "package store"))))

  (it "accepts backslash separators in the ident's file"
    (let [found (source/member-source :go {:name "Query"
                                           :file (str/replace query-file "/" "\\")
                                           :line 13
                                           :lang :go})]
      (should (str/ends-with? (:file found) "store/query.go"))))

  (it "returns nil when the ident's file does not exist"
    (should-be-nil (source/member-source :go {:name "Query"
                                              :file (str fixture-root "/store/missing.go")
                                              :line 13
                                              :lang :go})))

  (it "returns nil for a named member without a line"
    (should-be-nil (source/member-source :go {:name "Query" :file query-file :lang :go})))

  (it "returns nil for a line past the end of the file"
    (should-be-nil (source/member-source :go {:name "Query"
                                              :file query-file
                                              :line 9999
                                              :lang :go})))

  (it "opens the class-level file with no line for a module row"
    (let [found (source/member-source :go {:ns "example.com.demo.store"
                                           :file query-file
                                           :lang :go})]
      (should (str/ends-with? (:file found) "store/query.go"))
      (should-be-nil (:line found))))

  (it "titles a member by its file and name"
    (should= (str query-file "/Query")
             (source/title source-go/impl {:file query-file :name "Query"}))
    (should= "example.com.demo.store/Query"
             (source/title source-go/impl {:ns "example.com.demo.store" :name "Query"}))))

(def ^:private store-class
  (delay (->> (graph/scan graph-go/impl fixture-root {:prefix "example.com.demo"
                                                      :go {:goos "linux"}})
              :classes
              (filter #(= :store (:id %)))
              first)))

(defn- clicked-member-source [member-name]
  (let [class-card @store-class]
    (source/member-source
     (detail/member-ident {:class class-card
                           :ns (:ns class-card)
                           :lang (:lang class-card)
                           :file (:file class-card)}
                          member-name))))

(describe "go member click path"
  (it "opens a member from the package's second file at its own declaration"
    (with-go
      (let [found (clicked-member-source "Query")]
        (should= (str fixture-root "/store/open.go") (:file @store-class))
        (should (str/ends-with? (:file found) "store/query.go"))
        (should= (line-starting-with query-file "func Query") (:line found)))))

  (it "opens two methods named String at their own declarations"
    (with-go
      (let [store-string (clicked-member-source "Store.String")
            row-string (clicked-member-source "Row.String")]
        (should= (line-starting-with query-file "func (s Store) String") (:line store-string))
        (should= (line-starting-with query-file "func (r Row) String") (:line row-string))
        (should-not= (:line store-string) (:line row-string)))))

  (it "opens the class-level file for the package row"
    (with-go
      (let [found (clicked-member-source nil)]
        (should (str/ends-with? (:file found) "store/open.go"))
        (should-be-nil (:line found))))))
