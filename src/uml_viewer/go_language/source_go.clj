(ns uml-viewer.go-language.source-go
  "Go LanguageSource: open the ident's own :file at its :line."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [uml-viewer.source :as source]))

(defn- existing-file [ident]
  (let [file (:file ident)]
    (when (and (seq (str file)) (.isFile (io/file file)))
      (str/replace (str file) #"\\" "/"))))

(defn- line-text [source line-number]
  (when line-number
    (nth (str/split-lines source) (dec line-number) nil)))

(defrecord GoSource []
  source/LanguageSource
  (locate [_ ident]
    (existing-file ident))
  (extract [this source ident]
    (some-> (line-text source (source/start-line this source ident))
            str/trimr))
  (start-line [_ _source ident]
    (:line ident))
  (title [_ ident]
    (str (or (:file ident) (:ns ident))
         (when (:name ident) (str "/" (:name ident))))))

(def impl (->GoSource))

(source/register! :go impl)
