(ns uml-viewer.go-language.go-spec-support
  "Helpers shared by the specs that scan or measure the Go fixture module."
  (:require [clojure.java.io :as io]))

(def fixture-root "spec/fixtures/go/demo")

(def go-installed?
  (delay (try (zero? (-> (ProcessBuilder. ["go" "version"])
                         (.redirectOutput java.lang.ProcessBuilder$Redirect/DISCARD)
                         (.redirectError java.lang.ProcessBuilder$Redirect/DISCARD)
                         .start
                         .waitFor))
              (catch java.io.IOException _ false))))

(defmacro when-go-installed
  "Runs `body`, or prints `skip-message` when the go executable is missing."
  [skip-message & body]
  `(if @go-installed?
     (do ~@body)
     (println ~skip-message)))

(def ^:private tool-output-directories #{".metrics" "target"})

(defn- module-files
  "The files under `module-root`, without the output directories of crap4go and
  mutate4go: a tool run inside a module leaves them behind, git-ignored."
  [module-root]
  (->> (tree-seq #(and (.isDirectory %)
                       (or (= module-root %)
                           (not (tool-output-directories (.getName %)))))
                 #(.listFiles %)
                 module-root)
       (filter #(.isFile %))))

(defn temp-copy-of
  "A copy of the module at `module-root` in a new temp directory."
  [module-root]
  (let [copy-root (io/file (System/getProperty "java.io.tmpdir")
                           (str "uml-go-" (System/nanoTime)))]
    (doseq [file (module-files module-root)]
      (let [copy (io/file copy-root (str (.relativize (.toPath module-root) (.toPath file))))]
        (io/make-parents copy)
        (io/copy file copy)))
    copy-root))

(defn temp-copy-of-fixture []
  (temp-copy-of (io/file fixture-root)))
