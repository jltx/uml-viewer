(ns uml-viewer.main.go-metrics-spec
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [speclj.core :refer :all]
            [uml-viewer.main.go-metrics :as go-metrics]))

(def ^:private fixture-root "spec/fixtures/go/demo")

(defn- starts? [command]
  (try (-> (ProcessBuilder. ^java.util.List command)
           (.directory (io/file (System/getProperty "java.io.tmpdir")))
           (.redirectErrorStream true)
           (.redirectOutput java.lang.ProcessBuilder$Redirect/DISCARD)
           .start
           .waitFor)
       true
       (catch java.io.IOException _ false)))

(def ^:private tools-installed?
  (delay (every? starts? [["go" "version"]
                          ["sh" "-c" "exit 0"]
                          ["crap4go" "--help"]
                          ["mutate4go" "--help"]])))

(defmacro ^:private with-tools [& body]
  `(if @tools-installed?
     (do ~@body)
     (println "go, sh, crap4go, or mutate4go not found; Go metrics runner skipped")))

(defn- temp-copy-of-fixture []
  (let [fixture (io/file fixture-root)
        copy-root (io/file (System/getProperty "java.io.tmpdir")
                           (str "uml-go-metrics-" (System/nanoTime)))]
    (doseq [file (file-seq fixture)
            :when (.isFile file)]
      (let [copy (io/file copy-root (str (.relativize (.toPath fixture) (.toPath file))))]
        (io/make-parents copy)
        (io/copy file copy)))
    copy-root))

(defn- write-policy
  "Path of a policy file, written into `module-root`, whose `:src` is that directory."
  [module-root]
  (let [policy-file (io/file module-root "demo.policy.edn")]
    (spit policy-file (pr-str {:lang :go
                               :src (.getPath module-root)
                               :prefix "example.com.demo"
                               :go {:goos "linux"}}))
    (.getPath policy-file)))

(defn- run-captured
  "Status and printed output of one runner call."
  [command policy-path opts]
  (let [status (atom nil)
        output (with-out-str (reset! status (go-metrics/run! command policy-path opts)))]
    {:status @status :output output}))

(describe "go metrics preflight"
  (it "stops before any tool runs when sh cannot be started"
    (let [module-root (temp-copy-of-fixture)
          policy-path (write-policy module-root)
          {:keys [status output]}
          (with-bindings {#'go-metrics/*executables* {:sh "no-such-sh-program"
                                                      :crap4go "crap4go"
                                                      :mutate4go "mutate4go"}}
            (run-captured "crap" policy-path {}))]
      (should= 1 status)
      (should-contain
        "Go metrics need sh on PATH (run from Git Bash, or add Git's usr\\bin to PATH)"
        output)
      (should-not (.exists (io/file module-root ".metrics")))
      (should-not (.exists (io/file module-root "target"))))))

(defn- crap-snapshot-file [module-root]
  (io/file module-root ".metrics" "crap.edn"))

(describe "go crap snapshot"
  (it "writes the same entries for every package on each run"
    (with-tools
      (let [module-root (temp-copy-of-fixture)
            policy-path (write-policy module-root)
            first-run (run-captured "crap" policy-path {})
            first-snapshot (slurp (crap-snapshot-file module-root))
            second-run (run-captured "crap" policy-path {})
            second-snapshot (slurp (crap-snapshot-file module-root))
            entries (:entries (edn/read-string first-snapshot))
            measured-namespaces (set (map :namespace entries))]
        (should= 0 (:status first-run))
        (should= 0 (:status second-run))
        (should= first-snapshot second-snapshot)
        (should-contain "example.com.demo.store" measured-namespaces)
        (should-contain "example.com.demo" measured-namespaces)
        (should= 1 (count (filter #(= "platformName" (:name %)) entries)))))))
