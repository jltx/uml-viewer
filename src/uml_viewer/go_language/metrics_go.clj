(ns uml-viewer.go-language.metrics-go
  "Turn crap4go and mutate4go text reports into `.metrics` snapshot data."
  (:require [clojure.string :as str]))

(def ^:private crap-header-columns ["Function" "Package" "CC" "Cov%" "CRAP"])

(def ^:private measured-crap-row
  #"(\S+)\s+(\S+)\s+(\d+)\s+(\d+(?:\.\d+)?)%\s+(\d+(?:\.\d+)?)")

(defn- columns [line]
  (str/split (str/trim line) #"\s+"))

(defn- crap-row [line]
  (when-let [[_ function-name package-name complexity coverage crap]
             (re-matches measured-crap-row (str/trim line))]
    {:name function-name
     :package package-name
     :complexity (parse-long complexity)
     :coverage (parse-double coverage)
     :crap (parse-double crap)}))

(defn parse-crap-report
  "Rows of the crap4go table that carry a coverage and a CRAP number."
  [text]
  (->> (str/split-lines text)
       (drop-while #(not= crap-header-columns (columns %)))
       (keep crap-row)
       vec))
