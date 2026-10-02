(ns uml-viewer.go-language.metrics-go-spec
  (:require [clojure.string :as str]
            [speclj.core :refer :all]
            [uml-viewer.application.overlay :as overlay]
            [uml-viewer.go-language.graph-go :as graph-go]
            [uml-viewer.go-language.metrics-go :as metrics-go]
            [uml-viewer.graph :as graph]))

(def ^:private crap-report
  (str/join
    "\n"
    ["ok  \texample.com/demo/store\t0.412s\tcoverage: 71.4% of statements"
     "CRAP Report"
     "==========="
     "Function                       Package                               CC    Cov%     CRAP"
     "----------------------------------------------------------------------------------------"
     "LongReceiverTypeName.LongMethodName store                              6   33.3%     16.7"
     "Store.Close                    store                                  2  100.0%      2.0"
     "platformName                   store                                  1  100.0%      1.0"
     "platformName                   store                                  1    N/A       N/A"
     ""]))

(describe "go crap report"
  (it "reads each measured row as numbers, however wide the name"
    (should= [{:name "LongReceiverTypeName.LongMethodName"
               :package "store"
               :complexity 6
               :coverage 33.3
               :crap 16.7}
              {:name "Store.Close" :package "store" :complexity 2 :coverage 100.0 :crap 2.0}
              {:name "platformName" :package "store" :complexity 1 :coverage 100.0 :crap 1.0}]
             (metrics-go/parse-crap-report crap-report)))

  (it "reads nothing from output that holds no report"
    (should= [] (metrics-go/parse-crap-report
                  "FAIL\texample.com/demo/store [build failed]\n"))))

(def ^:private store-package
  {:import-path "example.com/demo/store"
   :ns "example.com.demo.store"
   :name "store"
   :dir "store"
   :files ["store/open.go" "store/query.go" "store/store_linux.go"]
   :decls [{:name "Store" :kind :type :file "store/open.go" :line 9 :exported true}
           {:name "Store.Close" :kind :method :file "store/open.go" :line 17 :exported true}
           {:name "Store.String" :kind :method :file "store/query.go" :line 17 :exported true}
           {:name "normalize" :kind :func :file "store/query.go" :line 21 :exported false}
           {:name "Row.String" :kind :method :file "store/query.go" :line 25 :exported true}
           {:name "platformName" :kind :func :file "store/store_linux.go" :line 5
            :exported false}]})

(defn- crap-row [function-name package-name complexity coverage crap]
  {:name function-name
   :package package-name
   :complexity complexity
   :coverage coverage
   :crap crap})

(describe "go crap entries"
  (it "gives each method of a package its own numbers under the package namespace"
    (should= [{:namespace "example.com.demo.store"
               :name "Store.String"
               :complexity 3
               :coverage 50.0
               :crap 4.1}
              {:namespace "example.com.demo.store"
               :name "Row.String"
               :complexity 1
               :coverage 100.0
               :crap 1.0}]
             (metrics-go/crap-entries [(crap-row "Store.String" "store" 3 50.0 4.1)
                                       (crap-row "Row.String" "store" 1 100.0 1.0)]
                                      store-package)))

  (it "drops a row whose name the package does not declare as a function"
    (should= [] (metrics-go/crap-entries [(crap-row "windowsOnly" "store" 1 0.0 2.0)
                                          (crap-row "Store" "store" 1 0.0 2.0)]
                                         store-package)))

  (it "drops a row of another package that shares a function name"
    (should= [] (metrics-go/crap-entries [(crap-row "normalize" "util" 1 100.0 1.0)]
                                         store-package)))

  (it "drops every row of a name that is measured twice"
    (should= ["Store.Close"]
             (map :name
                  (metrics-go/crap-entries [(crap-row "platformName" "store" 1 100.0 1.0)
                                            (crap-row "Store.Close" "store" 2 100.0 2.0)
                                            (crap-row "platformName" "store" 1 0.0 2.0)]
                                           store-package)))))

(def ^:private mutation-report
  (str/join
    "\n"
    ["ok  \texample.com/demo/store\t0.336s\tcoverage: 60.0% of statements"
     "Mutation run: store/query.go"
     "Total mutation sites: 9"
     "Covered mutation sites: 5"
     "Uncovered mutation sites: 4"
     "Changed mutation sites: 8"
     "Manifest exists: false"
     "Selected mutation sites: 5"
     "Uncovered mutations:"
     "  line 3 > -> >= "
     "  line 22 == -> != func/normalize"
     "  line 22 0 -> 1 func/normalize"
     "  line 26 false -> true func/Row.String"
     "ok  \texample.com/demo/store\t0.271s"
     "ok  \texample.com/demo/store\t0.288s"
     "[1/5] survived line 18 >= -> >: func/Store.Close"
     "--- FAIL: TestOpen (0.00s)"
     "    store_test.go:12: unexpected store string: \"store\""
     "FAIL"
     "FAIL\texample.com/demo/store\t0.284s"
     "FAIL"
     "[2/5] killed line 18 0 -> 1: func/Store.Close"
     "[3/5] timeout line 19 true -> false: func/Store.Close"
     "[4/5] killed line 19 != -> ==: func/Store.Close"
     "[5/5] killed line 3 > -> >=: "
     ""
     "Mutation Report"
     "==============="
     "Killed: 4"
     "Survived: 1"
     "Uncovered: 4"
     ""
     "Survivors:"
     "  line 18 >= -> > func/Store.Close"
     ""]))

(describe "go mutation report"
  (it "counts killed, survived, and uncovered sites per function amid test output"
    (should= [{:name "Row.String" :killed 0 :survived 0 :uncovered 1}
              {:name "Store.Close" :killed 3 :survived 1 :uncovered 0}
              {:name "normalize" :killed 0 :survived 0 :uncovered 2}]
             (metrics-go/parse-mutation-report mutation-report)))

  (it "counts a timeout as killed"
    (should= [{:name "Store.Close" :killed 1 :survived 0 :uncovered 0}]
             (metrics-go/parse-mutation-report
               "[1/1] timeout line 19 true -> false: func/Store.Close\n")))

  (it "takes uncovered sites from the uncovered block only"
    (should= [{:name "Store.Close" :killed 0 :survived 1 :uncovered 0}]
             (metrics-go/parse-mutation-report
               (str/join "\n" ["[1/1] survived line 18 >= -> >: func/Store.Close"
                               ""
                               "Survivors:"
                               "  line 18 >= -> > func/Store.Close"
                               ""])))))

(describe "go mutation forms"
  (it "ids an exported method as public and an unexported function as private"
    (should= [{:id "defn/Store.Close" :killed 3 :survived 1 :uncovered 0 :sites 4}
              {:id "defn-/normalize" :killed 0 :survived 0 :uncovered 2 :sites 2}]
             (metrics-go/mutation-forms
               [{:name "Store.Close" :killed 3 :survived 1 :uncovered 0}
                {:name "normalize" :killed 0 :survived 0 :uncovered 2}])))

  (it "ids an unexported method of an exported type as private"
    (should= ["defn-/Store.reset"]
             (map :id (metrics-go/mutation-forms
                        [{:name "Store.reset" :killed 1 :survived 0 :uncovered 0}]))))

  (it "ids a name that starts with an underscore as private"
    (should= ["defn-/_helper"]
             (map :id (metrics-go/mutation-forms
                        [{:name "_helper" :killed 1 :survived 0 :uncovered 0}])))))

(def ^:private root-package
  {:import-path "example.com/demo"
   :ns "example.com.demo"
   :name "demo"
   :dir "."
   :files ["demo.go"]
   :decls [{:name "Version" :kind :func :file "demo.go" :line 3 :exported true}]})

(def ^:private facts
  {:module "example.com/demo" :goos "linux" :packages [root-package store-package]})

(describe "go package selection"
  (it "selects every package when no changed files are given"
    (should= [root-package store-package] (metrics-go/select-packages facts nil)))

  (it "selects only the packages that hold a changed file"
    (should= [store-package] (metrics-go/select-packages facts ["store/query.go"])))

  (it "selects no package when nothing changed"
    (should= [] (metrics-go/select-packages facts []))))

(defn- crap-entry [namespace-name function-name crap]
  {:namespace namespace-name :name function-name :complexity 1 :coverage 100.0 :crap crap})

(describe "go crap merge"
  (it "replaces the entries of a measured namespace and keeps the others, sorted"
    (should= [(crap-entry "example.com.demo" "Version" 1.0)
              (crap-entry "example.com.demo.store" "Open" 1.0)
              (crap-entry "example.com.demo.store" "Store.Close" 2.0)]
             (metrics-go/merge-crap [(crap-entry "example.com.demo.store" "Store.Close" 6.0)
                                     (crap-entry "example.com.demo.store" "normalize" 1.0)
                                     (crap-entry "example.com.demo" "Version" 1.0)]
                                    [(crap-entry "example.com.demo.store" "Store.Close" 2.0)
                                     (crap-entry "example.com.demo.store" "Open" 1.0)]
                                    ["example.com.demo.store"])))

  (it "empties a measured namespace that has no new entries"
    (should= [(crap-entry "example.com.demo" "Version" 1.0)]
             (metrics-go/merge-crap [(crap-entry "example.com.demo.store" "Store.Close" 6.0)
                                     (crap-entry "example.com.demo" "Version" 1.0)]
                                    []
                                    ["example.com.demo.store"])))

  (it "keeps the entries of a namespace that was not measured"
    (should= [(crap-entry "example.com.demo" "Version" 1.0)
              (crap-entry "example.com.demo.store" "Store.Close" 6.0)]
             (metrics-go/merge-crap [(crap-entry "example.com.demo.store" "Store.Close" 6.0)]
                                    [(crap-entry "example.com.demo" "Version" 1.0)]
                                    ["example.com.demo"]))))

(defn- mutation-form [id killed survived]
  {:id id :killed killed :survived survived :uncovered 0 :sites (+ killed survived)})

(describe "go mutation form merge"
  (it "replaces the forms of a measured file and keeps the others, sorted"
    (should= [(mutation-form "defn-/normalize" 2 0)
              (mutation-form "defn/Store.Close" 1 2)
              (mutation-form "defn/Store.String" 3 0)]
             (metrics-go/merge-forms [(mutation-form "defn/Store.String" 1 0)
                                      (mutation-form "defn/Store.Close" 1 2)]
                                     [(mutation-form "defn/Store.String" 3 0)
                                      (mutation-form "defn-/normalize" 2 0)]
                                     store-package
                                     ["store/query.go"])))

  (it "drops a stale form whose function the package no longer declares"
    (should= [(mutation-form "defn/Store.Close" 1 2)]
             (metrics-go/merge-forms [(mutation-form "defn/Deleted" 4 0)
                                      (mutation-form "defn/Store.Close" 1 2)]
                                     []
                                     store-package
                                     [])))

  (it "removes the form of a function in a measured file that has no site in this run"
    (should= [(mutation-form "defn/Store.String" 1 0)]
             (metrics-go/merge-forms [(mutation-form "defn/Store.String" 1 0)
                                      (mutation-form "defn/Store.Close" 1 2)]
                                     []
                                     store-package
                                     ["store/open.go"])))

  (it "keeps the form of a function in a file that was not measured"
    (should= [(mutation-form "defn/Store.Close" 1 2)
              (mutation-form "defn/Store.String" 3 0)]
             (metrics-go/merge-forms [(mutation-form "defn/Store.Close" 1 2)]
                                     [(mutation-form "defn/Store.String" 3 0)]
                                     store-package
                                     ["store/query.go"]))))

(def ^:private tool-package
  {:import-path "example.com/demo/cmd/tool"
   :ns "example.com.demo.cmd.tool"
   :name "main"
   :dir "cmd/tool"
   :files ["cmd/tool/flags.go" "cmd/tool/render.go"]
   :decls [{:name "init" :kind :func :file "cmd/tool/flags.go" :line 5 :exported false}
           {:name "parseFlags" :kind :func :file "cmd/tool/flags.go" :line 9 :exported false}
           {:name "init" :kind :func :file "cmd/tool/render.go" :line 5 :exported false}
           {:name "render" :kind :func :file "cmd/tool/render.go" :line 9 :exported false}]})

(def ^:private flags-file-forms
  [(mutation-form "defn-/init" 1 0) (mutation-form "defn-/parseFlags" 2 1)])

(def ^:private render-file-forms
  [(mutation-form "defn-/init" 0 3) (mutation-form "defn-/render" 4 0)])

(def ^:private forms-of-uniquely-named-functions
  [(mutation-form "defn-/parseFlags" 2 1) (mutation-form "defn-/render" 4 0)])

(describe "go mutation form merge of a name declared in several files"
  (it "leaves the name out of a run over every file"
    (should= forms-of-uniquely-named-functions
             (metrics-go/merge-forms []
                                     (concat flags-file-forms render-file-forms)
                                     tool-package
                                     (:files tool-package))))

  (it "gives the same forms when one file is measured again, however often"
    (let [after-full-run (metrics-go/merge-forms []
                                                 (concat flags-file-forms render-file-forms)
                                                 tool-package
                                                 (:files tool-package))
          measure-flags-file-again #(metrics-go/merge-forms %
                                                            flags-file-forms
                                                            tool-package
                                                            ["cmd/tool/flags.go"])
          after-one-partial-run (measure-flags-file-again after-full-run)]
      (should= forms-of-uniquely-named-functions after-one-partial-run)
      (should= forms-of-uniquely-named-functions
               (measure-flags-file-again after-one-partial-run))))

  (it "removes a form of the name that an earlier snapshot holds"
    (should= [(mutation-form "defn-/render" 4 0)]
             (metrics-go/merge-forms render-file-forms [] tool-package []))))

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
     (println "go not found; Go metrics overlay skipped")))

(def ^:private string-methods-crap-report
  (str/join
    "\n"
    ["CRAP Report"
     "==========="
     "Function                       Package                               CC    Cov%     CRAP"
     "----------------------------------------------------------------------------------------"
     "Store.String                   store                                  3   50.0%      4.1"
     "Row.String                     store                                  1  100.0%      1.0"
     ""]))

(def ^:private string-methods-mutation-report
  (str/join
    "\n"
    ["Uncovered mutations:"
     "  line 26 false -> true func/Row.String"
     "[1/2] killed line 18 + -> -: func/Store.String"
     "[2/2] survived line 18 0 -> 1: func/Store.String"
     ""]))

(defn- named [function-name items]
  (first (filter #(= function-name (:name %)) items)))

(describe "go metrics on the overlay"
  (it "paints each method of the scanned store class with its own numbers"
    (with-go
      (let [go-opts {:goos "linux"}
            scanned-package (named "store" (:packages (graph-go/scan-facts fixture-root go-opts)))
            scan (graph/scan graph-go/impl fixture-root
                             {:prefix "example.com.demo" :go go-opts})
            scanned-class (first (filter #(= :store (:id %)) (:classes scan)))
            crap-entries (metrics-go/crap-entries
                           (metrics-go/parse-crap-report string-methods-crap-report)
                           scanned-package)
            forms (metrics-go/merge-forms
                    []
                    (metrics-go/mutation-forms
                      (metrics-go/parse-mutation-report string-methods-mutation-report))
                    scanned-package
                    (:files scanned-package))
            package-namespace (:ns scanned-package)
            metrics {:crap (group-by :namespace crap-entries)
                     :mutate {package-namespace {:namespace package-namespace :forms forms}}}
            painted-class (first (:classes (overlay/apply-metrics
                                             {:hierarchical true
                                              :prefix "example.com.demo"
                                              :classes [scanned-class]}
                                             metrics)))
            store-string (named "Store.String" (:ops painted-class))
            row-string (named "Row.String" (:ops painted-class))]
        (should= {:cc 3 :crap 4.1 :coverage 0.5}
                 (select-keys store-string [:cc :crap :coverage]))
        (should= {:cc 1 :crap 1.0 :coverage 1.0}
                 (select-keys row-string [:cc :crap :coverage]))
        (should= {:killed 1 :survived 1 :uncovered 0 :sites 2}
                 (select-keys store-string [:killed :survived :uncovered :sites]))
        (should= {:killed 0 :survived 0 :uncovered 1 :sites 1}
                 (select-keys row-string [:killed :survived :uncovered :sites]))
        (should= (select-keys (named "Store.String" (:ops scanned-class)) [:file :line])
                 (select-keys store-string [:file :line]))
        (should= (select-keys (named "Row.String" (:ops scanned-class)) [:file :line])
                 (select-keys row-string [:file :line]))
        (should= "spec/fixtures/go/demo/store/query.go" (:file row-string))
        (should (pos? (:line row-string)))
        (should= (map :name (:ops scanned-class)) (map :name (:ops painted-class)))))))
