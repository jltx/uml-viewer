(ns uml-viewer.go-language.source-go-spec
  (:require [clojure.string :as str]
            [speclj.core :refer :all]
            [uml-viewer.go-language.source-go :as source-go]
            [uml-viewer.source :as source]))

(def ^:private fixture-root "spec/fixtures/go/demo")
(def ^:private query-file (str fixture-root "/store/query.go"))

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
