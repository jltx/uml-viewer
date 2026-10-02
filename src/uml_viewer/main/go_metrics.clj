(ns uml-viewer.main.go-metrics
  "Write `.metrics` snapshots for a Go module from crap4go and mutate4go."
  (:refer-clojure :exclude [run!])
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [uml-viewer.application.ir-generator :as ir-generator]
            [uml-viewer.go-language.graph-go :as graph-go]
            [uml-viewer.go-language.metrics-go :as metrics-go])
  (:gen-class))

(def ^:private ^:dynamic *executables*
  {:sh "sh" :crap4go "crap4go" :mutate4go "mutate4go"})

(defn- run-process
  "Exit status and combined stdout and stderr of `command` run in `directory`.
  The output goes to a file and stdin is closed at once: a pipe stays open
  for as long as any descendant of the child holds it, and mutate4go leaves
  the `go test` of a timed-out mutant running, so reading a pipe to its end
  would wait for that orphan. Throws, with `:missing-executable` in the
  data, when the program cannot be started."
  [directory command]
  (let [output-file (java.io.File/createTempFile "go-metrics" ".out")
        builder (-> (ProcessBuilder. ^java.util.List command)
                    (.directory directory)
                    (.redirectErrorStream true)
                    (.redirectOutput output-file))]
    (try
      (let [process (try (.start builder)
                         (catch java.io.IOException not-started
                           (throw (ex-info (str (first command) " not found on PATH")
                                           {:missing-executable (first command)}
                                           not-started))))]
        (.close (.getOutputStream process))
        (let [exit (.waitFor process)]
          {:exit exit :output (slurp output-file)}))
      (finally (.delete output-file)))))

(defn- sh-starts? [module-root]
  (try (run-process module-root [(:sh *executables*) "-c" "exit 0"])
       true
       (catch clojure.lang.ExceptionInfo _ false)))

(defn- run-tool
  "Output of one tool invocation in the module root, or nil when the tool
  exits non-zero. Prints the progress line of the invocation, after the
  tool's own output when it failed."
  [module-root label command]
  (let [started (System/nanoTime)
        {:keys [exit output]} (run-process module-root command)
        seconds (/ (- (System/nanoTime) started) 1e9)]
    (when-not (zero? exit)
      (println (str/trim-newline output)))
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

(defn- files-to-mutate [package changed-files]
  (if (nil? changed-files)
    (:files package)
    (filterv (set changed-files) (:files package))))

(defn- measure-mutations
  "Writes one mutation snapshot per package. Returns the packages that hold
  a file that could not be measured."
  [module-root packages changed-files]
  (->> packages
       (mapv (fn [package]
               {:package package
                :measured? (write-mutation-snapshot module-root
                                                    package
                                                    (files-to-mutate package changed-files))}))
       (remove :measured?)
       (map :package)))

(defn- measure
  "Runs `command` over the packages that hold `changed-files`, or over every
  package when `changed-files` is nil. Returns the exit status."
  [command module-root go-opts changed-files]
  (let [facts (graph-go/scan-facts module-root go-opts)
        packages (metrics-go/select-packages facts changed-files)
        unmeasured-packages (case command
                              "crap" (measure-crap module-root packages)
                              "mutate" (measure-mutations module-root packages changed-files))]
    (doseq [package unmeasured-packages]
      (println (str "not measured: " (:import-path package))))
    (if (seq unmeasured-packages) 1 0)))

(defn- changed-go-sources
  "The Go source files, tests left out, in the output of `git diff --name-only`."
  [git-diff-output]
  (filterv #(and (str/ends-with? % ".go")
                 (not (str/ends-with? % "_test.go")))
           (str/split-lines git-diff-output)))

(defn- measure-since
  "Runs `command` over what changed between `git-ref` and HEAD. Returns the
  exit status."
  [command module-root go-opts git-ref]
  (let [{:keys [exit output]} (run-process module-root
                                           ["git" "diff" "--name-only" "--relative"
                                            (str git-ref "...HEAD")])]
    (if (zero? exit)
      (measure command module-root go-opts (changed-go-sources output))
      (do (print output)
          (flush)
          1))))

(def ^:private sh-missing-message
  "Go metrics need sh on PATH (run from Git Bash, or add Git's usr\\bin to PATH)")

(def ^:private usage
  "usage: clojure -M:go-metrics (crap|mutate) <policy.edn> [--since <git-ref>]")

(defn run!
  "Measure the Go module of the policy at `policy-path` and write its
  snapshot files. `command` is \"crap\" or \"mutate\"; `opts` may hold
  `:since`, a git ref. Returns the exit status."
  [command policy-path opts]
  (cond
    (not (and (#{"crap" "mutate"} command) policy-path)) (do (println usage)
                                                             1)
    (not (.isFile (io/file policy-path))) (do (println (str "policy file not found: " policy-path))
                                              1)
    :else
    (let [policy (ir-generator/read-policy policy-path)
          module-root (io/file (or (:src policy) "."))
          go-opts (:go policy)]
      (try
        (cond
          ;; a process cannot start in a missing directory, which would read as a missing sh
          (not (.isDirectory module-root)) (do (println (str "Go module root not found: "
                                                             module-root))
                                               1)
          (not (sh-starts? module-root)) (do (println sh-missing-message)
                                             1)
          (:since opts) (measure-since command module-root go-opts (:since opts))
          :else (measure command module-root go-opts nil))
        ;; every ex-info raised in here is a printable startup or scan failure
        (catch clojure.lang.ExceptionInfo failure
          (println (ex-message failure))
          1)))))

(defn- parse-options
  "The options that the arguments after the policy path give, or nil when
  they are anything but nothing or `--since <git-ref>`."
  [option-arguments]
  (cond
    (empty? option-arguments) {}
    (and (= 2 (count option-arguments))
         (= "--since" (first option-arguments))) {:since (second option-arguments)}))

(defn- status-of-arguments
  "Exit status of a run with the command line `arguments`."
  [[command policy-path & option-arguments]]
  (if-let [opts (parse-options option-arguments)]
    (run! command policy-path opts)
    (do (println usage)
        1)))

(defn -main [& arguments]
  (let [status (status-of-arguments arguments)]
    (flush)
    (System/exit status)))
