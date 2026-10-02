(ns uml-viewer.main.go-metrics-spec
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
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

(defn- status-and-output
  "Status that `runner-call` returns and what it prints."
  [runner-call]
  (let [status (atom nil)
        output (with-out-str (reset! status (runner-call)))]
    {:status @status :output output}))

(defn- run-captured [command policy-path opts]
  (status-and-output #(go-metrics/run! command policy-path opts)))

(defn- command-line-captured [& arguments]
  (status-and-output #(#'go-metrics/status-of-arguments arguments)))

(def ^:private usage-line
  "usage: clojure -M:go-metrics (crap|mutate) <policy.edn> [--since <git-ref>]")

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
      (should-not (.exists (io/file module-root "target")))))

  (it "names a tool that cannot be started and measures nothing"
    (with-tools
      (let [module-root (temp-copy-of-fixture)
            policy-path (write-policy module-root)
            {:keys [status output]}
            (with-bindings {#'go-metrics/*executables* {:sh "sh"
                                                        :crap4go "no-such-crap4go-program"
                                                        :mutate4go "mutate4go"}}
              (run-captured "crap" policy-path {}))]
        (should= 1 status)
        (should= (str "no-such-crap4go-program not found on PATH" (System/lineSeparator))
                 output)
        (should-not (.exists (io/file module-root ".metrics")))))))

(defn- tool-leftovers-in-fixture
  "What a tool run inside the tracked fixture would leave behind. `target` is
  git-ignored, so `git status` alone would not show such a run."
  []
  (filterv #(.exists (io/file fixture-root %)) [".metrics" "target"]))

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
        (should= 1 (count (filter #(= "platformName" (:name %)) entries)))
        (should= [] (tool-leftovers-in-fixture)))))

  (it "keeps the earlier entries of a package whose tests fail and measures the others"
    (with-tools
      (let [module-root (temp-copy-of-fixture)
            policy-path (write-policy module-root)
            earlier-store-entry {:namespace "example.com.demo.store"
                                 :name "Store.Close"
                                 :complexity 7
                                 :coverage 12.5
                                 :crap 39.8}]
        (spit (io/file module-root "store" "store_test.go")
              "\nfunc TestAlwaysFails(t *testing.T) { t.Fatal(\"failing on purpose\") }\n"
              :append true)
        (io/make-parents (crap-snapshot-file module-root))
        (spit (crap-snapshot-file module-root) (pr-str {:entries [earlier-store-entry]}))
        (let [{:keys [status output]} (run-captured "crap" policy-path {})
              entries (:entries (edn/read-string (slurp (crap-snapshot-file module-root))))]
          (should= 1 status)
          (should-contain "--- FAIL: TestAlwaysFails" output)
          (should-contain "not measured: example.com/demo/store" (str/split-lines output))
          (should= [earlier-store-entry]
                   (filter #(= "example.com.demo.store" (:namespace %)) entries))
          (should= ["Version"]
                   (map :name (filter #(= "example.com.demo" (:namespace %)) entries)))
          (should-contain "example.com.demo.internal.util" (set (map :namespace entries))))))))

(defn- give-close-a-mutation-site
  "The fixture's Close has nothing mutate4go can mutate; this adds a comparison."
  [module-root]
  (let [open-file (io/file module-root "store" "open.go")]
    (spit open-file (str/replace (slurp open-file)
                                 "s.name = \"\""
                                 "if s.name == \"\" {\n\t\treturn nil\n\t}\n\ts.name = \"\""))))

(defn- form-with-id [forms id]
  (first (filter #(= id (:id %)) forms)))

(defn- add-drain-whose-mutant-never-returns
  "Adds a tested function to the store package. The mutant `<=` of its loop
  bound waits for a value that never comes, so mutate4go times it out; the
  orphaned test stays blocked until `go test` gives up after ten minutes."
  [module-root]
  (spit (io/file module-root "store" "drain.go")
        (str "package store\n\n"
             "func Drain(pending chan int, count int) int {\n"
             "\ttotal := 0\n"
             "\tfor received := 0; received < count; received++ {\n"
             "\t\ttotal += <-pending\n"
             "\t}\n"
             "\treturn total\n"
             "}\n"))
  (spit (io/file module-root "store" "store_test.go")
        (str "\nfunc TestDrain(t *testing.T) {\n"
             "\tpending := make(chan int, 2)\n"
             "\tpending <- 4\n"
             "\tpending <- 5\n"
             "\tif Drain(pending, 2) != 9 {\n"
             "\t\tt.Fatal(\"drain did not add the pending values\")\n"
             "\t}\n"
             "}\n")
        :append true))

(describe "go mutation snapshot"
  (it "writes the same per-function site counts of a package on each run"
    (with-tools
      (let [module-root (temp-copy-of-fixture)
            policy-path (write-policy module-root)
            store-snapshot-file (io/file module-root ".metrics/mutate/example.com/demo/store.edn")]
        (give-close-a-mutation-site module-root)
        (let [first-run (run-captured "mutate" policy-path {})
              first-snapshot (edn/read-string (slurp store-snapshot-file))
              second-run (run-captured "mutate" policy-path {})
              second-snapshot (edn/read-string (slurp store-snapshot-file))
              forms (:forms first-snapshot)]
          (should= 0 (:status first-run))
          (should= 0 (:status second-run))
          (should= "example.com.demo.store" (:namespace first-snapshot))
          (should (pos? (:sites (form-with-id forms "defn/Store.Close"))))
          (should= {:id "defn/Store.String" :killed 1 :survived 0 :uncovered 0 :sites 1}
                   (form-with-id forms "defn/Store.String"))
          (should= first-snapshot second-snapshot)
          (should= [] (tool-leftovers-in-fixture))))))

  (it "returns once mutate4go ends, though a timed-out mutant leaves its test running"
    (with-tools
      (let [module-root (temp-copy-of-fixture)
            policy-path (write-policy module-root)]
        (add-drain-whose-mutant-never-returns module-root)
        (let [started (System/nanoTime)
              {:keys [status]} (run-captured "mutate" policy-path {})
              elapsed-seconds (/ (- (System/nanoTime) started) 1e9)
              forms (:forms (edn/read-string
                              (slurp (io/file module-root
                                              ".metrics/mutate/example.com/demo/store.edn"))))]
          (should= 0 status)
          (should (< elapsed-seconds 180))
          (should= {:id "defn/Drain" :killed 3 :survived 0 :uncovered 0 :sites 3}
                   (form-with-id forms "defn/Drain")))))))

(describe "go metrics since a git ref"
  (it "reads the git ref that follows --since"
    (should= {:since "main"} (#'go-metrics/parse-options ["--since" "main"]))
    (should= {} (#'go-metrics/parse-options [])))

  (it "prints the usage line and measures nothing when --since has no git ref"
    (should= {:status 1 :output (str usage-line (System/lineSeparator))}
             (command-line-captured "crap" "no-policy.edn" "--since")))

  (it "prints the usage line and measures nothing for a flag it does not know"
    (should= {:status 1 :output (str usage-line (System/lineSeparator))}
             (command-line-captured "crap" "no-policy.edn" "--verbose")))

  (it "keeps the changed Go source files and leaves out tests and other files"
    (should= ["store/query.go" "demo.go"]
             (#'go-metrics/changed-go-sources
              "store/query.go\nstore/store_test.go\nREADME.md\ndemo.go\n")))

  (it "mutates only the changed files of a package"
    (let [package {:files ["store/open.go" "store/query.go"]}]
      (should= ["store/query.go"]
               (#'go-metrics/files-to-mutate package ["demo.go" "store/query.go"]))
      (should= ["store/open.go" "store/query.go"]
               (#'go-metrics/files-to-mutate package nil))))

  (it "measures nothing when git cannot list the changes"
    (let [module-root (temp-copy-of-fixture)
          {:keys [status]} (run-captured "crap" (write-policy module-root) {:since "main"})]
      (should= 1 status)
      (should-not (.exists (io/file module-root ".metrics"))))))

(describe "go metrics usage"
  (it "names the commands when the command is unknown"
    (let [{:keys [status output]} (run-captured "coverage" "no-policy.edn" {})]
      (should= 1 status)
      (should-contain usage-line output)))

  (it "names the module root when it is not a directory"
    (let [missing-root (io/file (System/getProperty "java.io.tmpdir")
                                (str "uml-go-missing-" (System/nanoTime)))
          policy-file (java.io.File/createTempFile "go-metrics" ".policy.edn")]
      (spit policy-file (pr-str {:lang :go :src (.getPath missing-root)}))
      (let [{:keys [status output]} (run-captured "crap" (.getPath policy-file) {})]
        (should= 1 status)
        (should-contain (str "Go module root not found: " (.getPath missing-root)) output)))))
