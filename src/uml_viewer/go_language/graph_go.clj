(ns uml-viewer.go-language.graph-go
  "Go LanguageGraph: one class per package, import edges, top-level
  declarations as ops. The facts come from the goscan helper program."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [uml-viewer.graph :as graph]))

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

(defn- package-id [package prefix module]
  (if (= (:ns package) prefix)
    (keyword (last (str/split module #"/")))
    (graph/id-of (:ns package) prefix)))

(defn- foreign-id [import-path]
  (let [first-segment (first (str/split import-path #"/"))]
    (keyword (if (str/includes? first-segment ".")
               (dotted import-path)
               (str "std." (dotted import-path))))))

(defn- foreign-class [import-path]
  (let [id (foreign-id import-path)]
    {:id id :name import-path :ns (name id) :foreign true}))

(defn- working-directory-relative [root module-relative-file]
  (graph/relative-path (io/file root module-relative-file)))

(defn- op-of [root decl]
  (cond-> {:name (:name decl)
           :text (if (= :type (:kind decl))
                   (str "type " (:name decl))
                   (:name decl))
           :file (working-directory-relative root (:file decl))
           :line (:line decl)}
    (not (:exported decl)) (assoc :private true)))

(defn- package-class [root package]
  {:id (:id package)
   :name (:name package)
   :ns (:ns package)
   :lang :go
   :file (working-directory-relative root (first (:files package)))
   :ops (mapv #(op-of root %) (:decls package))})

(defrecord GoGraph []
  graph/LanguageGraph
  (scan [_ root opts]
    (let [facts (scan-facts root (:go opts))
          packages (mapv #(assoc % :id (package-id % (:prefix opts) (:module facts)))
                         (:packages facts))
          module-package-ids (into {} (map (juxt :import-path :id)) packages)
          foreign-imports (->> packages
                               (mapcat :imports)
                               (remove module-package-ids)
                               distinct
                               sort)]
      {:classes (into (mapv #(package-class root %) packages)
                      (map foreign-class)
                      foreign-imports)
       :edges (->> (for [package packages
                         imported (:imports package)]
                     {:from (:id package)
                      :to (or (module-package-ids imported) (foreign-id imported))
                      :kind :dependency})
                   (remove #(= (:from %) (:to %)))
                   distinct
                   (sort-by (juxt :from :to))
                   vec)})))

(def impl (->GoGraph))

(graph/register! :go impl)
