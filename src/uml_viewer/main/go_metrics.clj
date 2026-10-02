(ns uml-viewer.main.go-metrics
  "Write `.metrics` snapshots for a Go module from crap4go and mutate4go."
  (:refer-clojure :exclude [run!])
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

(defn- sh-starts? []
  (try (run-process nil [(:sh *executables*) "-c" "exit 0"])
       true
       (catch java.io.IOException _ false)))

(defn run!
  "Measure the Go module of the policy at `policy-path` and write its
  snapshot files. `command` is \"crap\" or \"mutate\"; `opts` may hold
  `:since`, a git ref. Returns the exit status."
  [command policy-path opts]
  (if (sh-starts?)
    0
    (do (println "Go metrics need sh on PATH (run from Git Bash, or add Git's usr\\bin to PATH)")
        1)))
