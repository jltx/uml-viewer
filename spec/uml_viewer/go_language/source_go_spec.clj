(ns uml-viewer.go-language.source-go-spec
  (:require [clojure.string :as str]
            [speclj.core :refer :all]
            [uml-viewer.application.detail :as detail]
            [uml-viewer.go-language.graph-go :as graph-go]
            [uml-viewer.go-language.go-spec-support :as support :refer [fixture-root]]
            [uml-viewer.go-language.source-go :as source-go]
            [uml-viewer.graph :as graph]
            [uml-viewer.source :as source]))

(def ^:private query-file (str fixture-root "/store/query.go"))

(defmacro ^:private with-go [& body]
  `(support/when-go-installed "go not found; Go source navigation skipped" ~@body))

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

  (it "finds a named member without a line by its declaration"
    (should= (line-starting-with query-file "func Query")
             (:line (source/member-source :go {:name "Query" :file query-file :lang :go}))))

  (it "finds a named member whose line is past the end of the file"
    (should= (line-starting-with query-file "func Query")
             (:line (source/member-source :go {:name "Query"
                                               :file query-file
                                               :line 9999
                                               :lang :go}))))

  (it "returns nil for an undeclared member whose line is past the end of the file"
    (should-be-nil (source/member-source :go {:name "Missing"
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

(defn- source-of [lines]
  (str/join "\n" lines))

(defn- line-of [lines declaration]
  (inc (.indexOf ^java.util.List lines declaration)))

(defn- found-line [lines ident]
  (source/start-line source-go/impl (source-of lines) ident))

(def ^:private two-string-methods
  ["package store"
   ""
   "func (s Store) String() string { return s.name }"
   ""
   "func (r Row) String() string { return r.text }"])

(def ^:private row-string-line-before-edit
  (line-of two-string-methods "func (r Row) String() string { return r.text }"))

(def ^:private declaration-forms
  ["package store"
   ""
   "type RowSet []Row"
   ""
   "type ("
   "\t// Row is one result."
   "\tRow struct {"
   "\t\tStore Store"
   "\t}"
   "\tStore struct{}"
   ")"
   ""
   "type Cache[K comparable, V any] struct {"
   "\tentries map[K]V"
   "}"
   ""
   "type Alias = Row"
   ""
   "func OpenAll() {}"
   ""
   "func Open(name string) *Store { return nil }"
   ""
   "func Map[T any](items []T) []T { return items }"
   ""
   "func (s *Store) Close() error { return nil }"
   ""
   "func (Row) Kind() string { return \"row\" }"
   ""
   "func (*Row) Reset() {}"
   ""
   "func (set RowSet) Kind() string { return \"set\" }"
   ""
   "func (c *Cache[K, V]) Get(key K) (V, bool) {"
   "\tvalue, found := c.entries[key]"
   "\treturn value, found"
   "}"
   ""
   "func init() {}"
   ""
   "func init() {}"])

(defn- searched-line [member-name]
  (found-line declaration-forms {:name member-name}))

(describe "go declaration line"
  (it "follows a declaration that moved below its recorded line"
    (let [edited (concat (take 4 two-string-methods)
                         ["// String joins the row's" "// values with commas."]
                         (drop 4 two-string-methods))]
      (should= "// String joins the row's" (nth edited (dec row-string-line-before-edit)))
      (should= (+ 2 row-string-line-before-edit)
               (found-line edited {:name "Row.String"
                                   :line row-string-line-before-edit}))))

  (it "does not take another receiver's method for the member"
    (let [edited (into ["package store" "" "import \"strings\"" ""]
                       (drop 2 two-string-methods))]
      (should= "func (s Store) String() string { return s.name }"
               (nth edited (dec row-string-line-before-edit)))
      (should= (+ 2 row-string-line-before-edit)
               (found-line edited {:name "Row.String"
                                   :line row-string-line-before-edit}))))

  (it "keeps a recorded line that declares the member"
    (let [second-init (+ 2 (line-of declaration-forms "func init() {}"))]
      (should= second-init (found-line declaration-forms {:name "init" :line second-init}))
      (should= (- second-init 2) (found-line declaration-forms {:name "init" :line 1}))))

  (it "finds a method on a pointer receiver"
    (should= (line-of declaration-forms "func (s *Store) Close() error { return nil }")
             (searched-line "Store.Close")))

  (it "finds a method whose receiver has no variable name"
    (should= (line-of declaration-forms "func (Row) Kind() string { return \"row\" }")
             (searched-line "Row.Kind"))
    (should= (line-of declaration-forms "func (*Row) Reset() {}")
             (searched-line "Row.Reset")))

  (it "finds a method on a generic receiver"
    (should= (line-of declaration-forms "func (c *Cache[K, V]) Get(key K) (V, bool) {")
             (searched-line "Cache.Get")))

  (it "finds a function, with or without type parameters"
    (should= (line-of declaration-forms "func Open(name string) *Store { return nil }")
             (searched-line "Open"))
    (should= (line-of declaration-forms "func Map[T any](items []T) []T { return items }")
             (searched-line "Map")))

  (it "finds a type, a generic type, and an alias"
    (should= (line-of declaration-forms "type RowSet []Row") (searched-line "RowSet"))
    (should= (line-of declaration-forms "type Cache[K comparable, V any] struct {")
             (searched-line "Cache"))
    (should= (line-of declaration-forms "type Alias = Row") (searched-line "Alias")))

  (it "finds a type declared inside a type block, past a field of the same name"
    (should= (line-of declaration-forms "\tRow struct {") (searched-line "Row"))
    (should= (line-of declaration-forms "\tStore struct{}") (searched-line "Store")))

  (it "finds a type block entry below a comment with an unmatched bracket"
    (let [commented-block ["package store"
                           ""
                           "type ("
                           "\t// 1) a row holds values"
                           "\tRow struct{}"
                           "\t// 2) a store holds rows"
                           "\tStore struct{}"
                           ")"]]
      (should= 7 (found-line commented-block {:name "Store"}))))

  (it "does not take a longer receiver name for the member's receiver"
    (should= (line-of declaration-forms "func (set RowSet) Kind() string { return \"set\" }")
             (searched-line "RowSet.Kind")))

  (it "finds the same lines in a source with CRLF line endings"
    (let [crlf-line #(source/start-line source-go/impl
                                        (str/join "\r\n" declaration-forms)
                                        {:name %})]
      (should= (searched-line "Store") (crlf-line "Store"))
      (should= (searched-line "Open") (crlf-line "Open"))
      (should= (searched-line "Cache.Get") (crlf-line "Cache.Get"))
      (should= (line-of declaration-forms "\tStore struct{}") (crlf-line "Store"))))

  (it "is nil for a member the source does not declare"
    (should-be-nil (searched-line "Missing"))
    (should-be-nil (searched-line "Store.Kind"))
    (should-be-nil (searched-line "entries")))

  (it "keeps the recorded line of a declaration form the search does not know"
    (let [unusual-forms ["package store"
                         ""
                         "var _ = 0; func Late() {}"
                         ""
                         "func ("
                         "\tr Row,"
                         ") Wide() {}"
                         ""
                         "type ( Alias = int )"]]
      (should= 3 (found-line unusual-forms {:name "Late" :line 3}))
      (should= 7 (found-line unusual-forms {:name "Row.Wide" :line 7}))
      (should= 9 (found-line unusual-forms {:name "Alias" :line 9}))
      (should-be-nil (found-line unusual-forms {:name "Late"}))))

  (it "is nil for an undeclared member whose recorded line is past the end of the source"
    (should-be-nil (found-line declaration-forms
                               {:name "Missing" :line (inc (count declaration-forms))}))
    (should-be-nil (found-line declaration-forms {:name "Missing" :line 0})))

  (it "leaves the line of an ident without a name alone"
    (should= 7 (found-line declaration-forms {:ns "example.com.demo.store" :line 7}))
    (should-be-nil (found-line declaration-forms {:ns "example.com.demo.store"}))))

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
