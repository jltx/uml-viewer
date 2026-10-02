(ns uml-viewer.go-language.graph-go
  "Go LanguageGraph: one class per package, import edges, top-level
  declarations as ops. The facts come from the goscan helper program."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]))

(defn- dotted [import-path]
  (str/replace import-path "/" "."))

(defn- helper-source-path []
  (.getPath (io/file (.toURI (io/resource "uml_viewer/go_language/goscan/main.go")))))

(defn- run-helper
  "Stdout of the goscan helper run in module `root`."
  [root go-opts]
  (let [command (cond-> ["go" "run" (helper-source-path)]
                  (:goos go-opts) (conj "-goos" (:goos go-opts)))
        ;; stderr goes to a file: an undrained stderr pipe blocks the child once it fills
        stderr-file (java.io.File/createTempFile "goscan" ".stderr")]
    (try
      (let [process (-> (ProcessBuilder. ^java.util.List command)
                        (.directory (io/file root))
                        (.redirectError stderr-file)
                        .start)
            stdout (slurp (.getInputStream process))
            exit-code (.waitFor process)]
        (when-not (zero? exit-code)
          (throw (ex-info (str "go scan of " root " failed:\n" (str/trim (slurp stderr-file)))
                          {:root (str root) :exit exit-code})))
        stdout)
      (finally (.delete stderr-file)))))

(defn scan-facts
  "The goscan helper's report for the Go module at `root`, each package with
  `:ns`, its dotted import path. `go-opts` is `{:goos \"linux\"}` or nil."
  [root go-opts]
  (update (edn/read-string (run-helper root go-opts))
          :packages
          (fn [packages]
            (mapv #(assoc % :ns (dotted (:import-path %))) packages))))
