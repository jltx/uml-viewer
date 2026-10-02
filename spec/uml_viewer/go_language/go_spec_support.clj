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

(defn temp-copy-of-fixture []
  (let [fixture (io/file fixture-root)
        copy-root (io/file (System/getProperty "java.io.tmpdir")
                           (str "uml-go-" (System/nanoTime)))]
    (doseq [file (file-seq fixture)
            :when (.isFile file)]
      (let [copy (io/file copy-root (str (.relativize (.toPath fixture) (.toPath file))))]
        (io/make-parents copy)
        (io/copy file copy)))
    copy-root))
