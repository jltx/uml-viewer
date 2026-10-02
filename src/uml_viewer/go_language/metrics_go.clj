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

(defn- function-names
  "Names of the functions and methods a scanned package declares."
  [package]
  (into #{}
        (comp (filter #(#{:func :method} (:kind %)))
              (map :name))
        (:decls package)))

(defn crap-entries
  "Snapshot entries for one scanned package. A name measured more than once
  is left out: the rows cannot be told apart."
  [rows package]
  (let [declared (function-names package)
        package-rows (filter #(and (= (:name package) (:package %))
                                   (declared (:name %)))
                             rows)
        row-count (frequencies (map :name package-rows))]
    (->> package-rows
         (filter #(= 1 (row-count (:name %))))
         (mapv (fn [row]
                 {:namespace (:ns package)
                  :name (:name row)
                  :complexity (:complexity row)
                  :coverage (:coverage row)
                  :crap (:crap row)})))))
