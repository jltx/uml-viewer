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

(def ^:private ^:dynamic *go-executable* "go")

(defn- helper-process
  "The command that runs the goscan helper and the environment it runs in,
  given the `environment` of this process. `go run` must build the helper for
  the host, so GOOS and GOARCH leave the environment and the target GOOS, from
  `go-opts` or else from `environment`, goes to the helper as a flag."
  [go-opts environment]
  (let [target-goos (or (:goos go-opts) (get environment "GOOS"))]
    {:command (cond-> [*go-executable* "run" (helper-source-path)]
                target-goos (conj "-goos" target-goos))
     :environment (dissoc environment "GOOS" "GOARCH")}))

(defn- start-helper [root go-opts stderr-file]
  (let [{:keys [command environment]} (helper-process go-opts (into {} (System/getenv)))
        builder (-> (ProcessBuilder. ^java.util.List command)
                    (.directory (io/file root))
                    (.redirectError stderr-file))]
    (doto (.environment builder) .clear (.putAll environment))
    (try (.start builder)
         (catch java.io.IOException cause
           (throw (ex-info (str "Go toolchain not found on PATH: " (ex-message cause))
                           {:root (str root) :missing-executable (first command)}
                           cause))))))

(defn- run-helper
  "Stdout of the goscan helper run in module `root`."
  [root go-opts]
  ;; stderr goes to a file: an undrained stderr pipe blocks the child once it fills
  (let [stderr-file (java.io.File/createTempFile "goscan" ".stderr")]
    (try
      (let [process (start-helper root go-opts stderr-file)
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

(defn- reject-shared-ids
  "Throws when two packages have one class id: the diagram would show a
  single node for both."
  [packages]
  (doseq [[id sharing-packages] (sort-by key (group-by :id packages))
          :when (next sharing-packages)]
    (let [import-paths (mapv :import-path sharing-packages)]
      (throw (ex-info (str "Go packages " (str/join " and " import-paths)
                           " both map to class id " id)
                      {:id id :import-paths import-paths})))))

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
          _ (reject-shared-ids packages)
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
