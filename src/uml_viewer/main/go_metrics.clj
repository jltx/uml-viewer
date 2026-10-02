(ns uml-viewer.main.go-metrics
  "Write `.metrics` snapshots for a Go module from crap4go and mutate4go."
  (:refer-clojure :exclude [run!])
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [uml-viewer.application.ir-generator :as ir-generator]
            [uml-viewer.go-language.graph-go :as graph-go]
            [uml-viewer.go-language.metrics-go :as metrics-go])
  (:gen-class))

(def ^:private ^:dynamic *executables*
  {:sh "sh" :crap4go "crap4go" :mutate4go "mutate4go"})

(defn- run-process
  "Exit status and output of `command` run in `directory`. Stderr is merged
  into stdout so that one drained stream cannot leave the child blocked on
  the other."
  [directory command]
  (let [process (-> (ProcessBuilder. ^java.util.List command)
                    (.directory directory)
                    (.redirectErrorStream true)
                    .start)
        output (slurp (.getInputStream process))]
    {:exit (.waitFor process) :output output}))

(defn- sh-starts? [module-root]
  (try (run-process module-root [(:sh *executables*) "-c" "exit 0"])
       true
       (catch java.io.IOException _ false)))

(defn- run-tool
  "Output of one tool invocation in the module root, or nil when the tool
  exits non-zero. Prints the progress line of the invocation."
  [module-root label command]
  (let [started (System/nanoTime)
        {:keys [exit output]} (run-process module-root command)
        seconds (/ (- (System/nanoTime) started) 1e9)]
    (println (format "%s %.1fs%s" label seconds (if (zero? exit) "" " failed")))
    (when (zero? exit) output)))

(defn- test-command [package]
  (if (= "." (:dir package))
    "go test ."
    (str "go test ./" (:dir package))))

(defn- read-snapshot [file]
  (when (.isFile file)
    (edn/read-string (slurp file))))

(defn- write-snapshot [file snapshot]
  (io/make-parents file)
  (spit file (str (pr-str snapshot) "\n")))

(defn- crap-command [package]
  (cond-> [(:crap4go *executables*) "--test-command" (test-command package)]
    (not= "." (:dir package)) (conj (str (:dir package) "/"))))

(defn- crap-entries-of-package
  "Snapshot entries of `package`, or nil when crap4go fails on it."
  [module-root package]
  (when-let [report (run-tool module-root
                              (str "crap4go " (:import-path package))
                              (crap-command package))]
    (metrics-go/crap-entries (metrics-go/parse-crap-report report) package)))

(defn- measure-crap
  "Writes the crap snapshot of the module. Returns the packages that could
  not be measured; their earlier entries stay in the snapshot."
  [module-root packages]
  (let [runs (mapv (fn [package]
                     {:package package :entries (crap-entries-of-package module-root package)})
                   packages)
        measured (filter :entries runs)
        snapshot-file (io/file module-root ".metrics" "crap.edn")]
    (when (seq measured)
      (write-snapshot snapshot-file
                      {:entries (metrics-go/merge-crap (:entries (read-snapshot snapshot-file))
                                                       (mapcat :entries measured)
                                                       (map (comp :ns :package) measured))}))
    (map :package (remove :entries runs))))

(defn- mutation-forms-of-file
  "Snapshot forms of the functions in `file`, or nil when mutate4go fails on it."
  [module-root package file]
  (when-let [report (run-tool module-root
                              (str "mutate4go " file)
                              [(:mutate4go *executables*) file "--mutate-all"
                               "--test-command" (test-command package)])]
    (metrics-go/mutation-forms (metrics-go/parse-mutation-report report))))

(defn- write-mutation-snapshot
  "Writes the mutation snapshot of `package` from runs over `files`. Forms
  of a file mutate4go fails on stay as they were. True when every file was
  measured."
  [module-root package files]
  (let [runs (mapv (fn [file]
                     {:file file :forms (mutation-forms-of-file module-root package file)})
                   files)
        measured (filter :forms runs)
        snapshot-file (io/file module-root ".metrics" "mutate" (str (:import-path package) ".edn"))]
    (when (seq measured)
      (write-snapshot snapshot-file
                      {:namespace (:ns package)
                       :forms (metrics-go/merge-forms (:forms (read-snapshot snapshot-file))
                                                      (mapcat :forms measured)
                                                      package
                                                      (map :file measured))}))
    (= (count runs) (count measured))))

(defn- measure-mutations
  "Writes one mutation snapshot per package. Returns the packages that hold
  a file that could not be measured."
  [module-root packages]
  (->> packages
       (mapv (fn [package]
               {:package package
                :measured? (write-mutation-snapshot module-root package (:files package))}))
       (remove :measured?)
       (map :package)))

(def ^:private sh-missing-message
  "Go metrics need sh on PATH (run from Git Bash, or add Git's usr\\bin to PATH)")

(def ^:private usage
  "usage: clojure -M:go-metrics (crap|mutate) <policy.edn> [--since <git-ref>]")

(defn run!
  "Measure the Go module of the policy at `policy-path` and write its
  snapshot files. `command` is \"crap\" or \"mutate\"; `opts` may hold
  `:since`, a git ref. Returns the exit status."
  [command policy-path opts]
  (if-not (and (#{"crap" "mutate"} command) policy-path)
    (do (println usage)
        1)
    (let [policy (ir-generator/read-policy policy-path)
          module-root (io/file (or (:src policy) "."))]
      (if-not (sh-starts? module-root)
        (do (println sh-missing-message)
            1)
        (let [facts (graph-go/scan-facts module-root (:go policy))
              packages (metrics-go/select-packages facts nil)
              unmeasured-packages (case command
                                    "crap" (measure-crap module-root packages)
                                    "mutate" (measure-mutations module-root packages))]
          (doseq [package unmeasured-packages]
            (println (str "not measured: " (:import-path package))))
          (if (seq unmeasured-packages) 1 0))))))
