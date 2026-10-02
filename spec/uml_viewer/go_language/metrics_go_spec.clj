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
