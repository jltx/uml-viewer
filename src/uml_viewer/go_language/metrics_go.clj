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

(defn- function-decls
  "The function and method declarations of a scanned package."
  [package]
  (filter #(#{:func :method} (:kind %)) (:decls package)))

(defn crap-entries
  "Snapshot entries for one scanned package. A name measured more than once
  is left out: the rows cannot be told apart."
  [rows package]
  (let [declared (set (map :name (function-decls package)))
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

(def ^:private mutant-progress-line
  #"\[\d+/\d+\] (killed|survived|timeout) line \d+ .*: func/(\S+)")

(def ^:private uncovered-site-line #"  line \d+ .* func/(\S+)")

(def ^:private outcome-of-status
  {"killed" :killed "timeout" :killed "survived" :survived})

(defn- tested-site [line]
  (when-let [[_ status function-name] (re-matches mutant-progress-line line)]
    [function-name (outcome-of-status status)]))

(defn- uncovered-site [line]
  (when-let [[_ function-name] (re-matches uncovered-site-line line)]
    [function-name :uncovered]))

(defn- uncovered-block
  "The site lines under the heading. The survivors list at the end of the
  report has the same line shape, so only this block may be read."
  [lines]
  (->> lines
       (drop-while #(not= "Uncovered mutations:" %))
       rest
       (take-while #(str/starts-with? % "  line "))))

(defn parse-mutation-report
  "Per-function site counts from the output of one mutate4go run."
  [text]
  (let [lines (str/split-lines text)
        sites (concat (keep tested-site lines)
                      (keep uncovered-site (uncovered-block lines)))
        counts-by-function (reduce (fn [counts [function-name outcome]]
                                     (update-in counts [function-name outcome] (fnil inc 0)))
                                   {}
                                   sites)]
    (->> counts-by-function
         (map (fn [[function-name counts]]
                (merge {:name function-name :killed 0 :survived 0 :uncovered 0} counts)))
         (sort-by :name)
         vec)))

(defn- private-name? [member-name]
  (let [last-identifier (last (str/split member-name #"\."))]
    (not (Character/isUpperCase (char (first last-identifier))))))

(defn mutation-forms
  "Snapshot forms for per-function site counts. The overlay reads the
  function name and its visibility from the `defn/` or `defn-/` id."
  [results]
  (mapv (fn [{:keys [killed survived uncovered] function-name :name}]
          {:id (str (if (private-name? function-name) "defn-/" "defn/") function-name)
           :killed killed
           :survived survived
           :uncovered uncovered
           :sites (+ killed survived uncovered)})
        results))

(defn select-packages
  "The scanned packages to measure: all of them when `changed-files` is nil,
  otherwise those that hold one of the changed files."
  [facts changed-files]
  (if (nil? changed-files)
    (:packages facts)
    (let [changed (set changed-files)]
      (filterv #(some changed (:files %)) (:packages facts)))))

(defn merge-crap
  "The crap snapshot entries after a run: `new-entries` stand in for every
  earlier entry of `measured-namespaces`; other namespaces keep theirs."
  [existing-entries new-entries measured-namespaces]
  (let [measured (set measured-namespaces)]
    (->> existing-entries
         (remove #(measured (:namespace %)))
         (concat new-entries)
         (sort-by (juxt :namespace :name))
         vec)))

(defn- form-function-name [form]
  (str/replace (:id form) #"^defn-?/" ""))

(defn merge-forms
  "The mutation snapshot forms of `package` after a run of `measured-files`:
  `new-forms` stand in for every earlier form of a function declared in a
  measured file, forms of functions in other files stay, and forms of
  functions the package no longer declares are left out. A name declared
  more than once (every file may hold an `init`) is left out as well: its
  forms cannot be told apart."
  [existing-forms new-forms package measured-files]
  (let [measured (set measured-files)
        decls-of-function (group-by :name (function-decls package))
        declared-more-than-once? (fn [form]
                                   (< 1 (count (decls-of-function (form-function-name form)))))
        in-unmeasured-file? (fn [form]
                              (when-let [[decl] (decls-of-function (form-function-name form))]
                                (not (measured (:file decl)))))]
    (->> existing-forms
         (filter in-unmeasured-file?)
         (concat new-forms)
         (remove declared-more-than-once?)
         (sort-by :id)
         vec)))
