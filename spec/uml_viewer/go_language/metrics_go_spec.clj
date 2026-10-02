(ns uml-viewer.go-language.metrics-go-spec
  (:require [clojure.string :as str]
            [speclj.core :refer :all]
            [uml-viewer.go-language.metrics-go :as metrics-go]))

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
