(ns uml-viewer.go-language.source-go
  "Go LanguageSource: open the ident's own :file at the member's declaration,
  which is at the ident's :line or, when that line is stale, found by search;
  a declaration the search does not find opens at the ident's :line."
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

(def ^:private not-an-identifier-character "(?![\\p{L}\\p{Nd}_])")

(defn- literal [identifier]
  (java.util.regex.Pattern/quote identifier))

(defn- method-pattern [receiver method]
  (re-pattern (str "^\\s*func\\s*\\(\\s*(?:[^\\s()*]+\\s+)?\\*?\\s*" (literal receiver)
                   "\\s*(?:\\[[^\\]]*\\])?\\s*\\)\\s*" (literal method) "\\s*\\(")))

(defn- function-or-type-pattern [declared-name]
  (re-pattern (str "^\\s*(?:func\\s+" (literal declared-name) "\\s*[(\\[]"
                   "|type\\s+" (literal declared-name) not-an-identifier-character ")")))

(defn- type-block-entry-pattern [declared-name]
  (re-pattern (str "^\\s*" (literal declared-name) not-an-identifier-character)))

(defn- bracket-balance [line]
  (let [code (str/replace line #"//.*" "")]
    (- (count (re-seq #"[(\[{]" code)) (count (re-seq #"[)\]}]" code)))))

(defn- type-block-depths
  "For each line, the bracket depth at its start inside a `type ( … )` block:
  1 for the block's own entries, 0 outside a block."
  [lines]
  (reductions (fn [depth line]
                (if (or (pos? depth) (re-find #"^\s*type\s*\(" line))
                  (+ depth (bracket-balance line))
                  0))
              0
              lines))

(defn- declaration-test
  "A predicate of a line and its type-block depth: does the line declare
  `member-name`, which is `Func`, `Receiver.Method`, or a type name?"
  [member-name]
  (let [[receiver-or-name method] (str/split member-name #"\." 2)]
    (if method
      (let [pattern (method-pattern receiver-or-name method)]
        (fn [line _type-block-depth]
          (re-find pattern line)))
      (let [keyword-pattern (function-or-type-pattern member-name)
            entry-pattern (type-block-entry-pattern member-name)]
        (fn [line type-block-depth]
          (or (re-find keyword-pattern line)
              (and (= 1 type-block-depth) (re-find entry-pattern line))))))))

(defn- declaration-lines
  "1-based lines of `source` that declare `member-name`, in file order."
  [source member-name]
  (let [lines (str/split-lines source)
        declares? (declaration-test member-name)]
    (keep (fn [[line-number line type-block-depth]]
            (when (declares? line type-block-depth) line-number))
          (map vector (iterate inc 1) lines (type-block-depths lines)))))

(defn- member-line
  "The ident's recorded line when it declares the member (two `init` functions
  share one name), else the first line that does. When the search knows no
  such line, the recorded line stands if the source has it: the scanner read
  it from the syntax tree, which knows more declaration forms than the search."
  [source ident]
  (let [declared (declaration-lines source (:name ident))
        recorded-line (:line ident)]
    (or (some #{recorded-line} declared)
        (first declared)
        (when (line-text source recorded-line) recorded-line))))

(defrecord GoSource []
  source/LanguageSource
  (locate [_ ident]
    (existing-file ident))
  (extract [this source ident]
    (some-> (line-text source (source/start-line this source ident))
            str/trimr))
  (start-line [_ source ident]
    (if (seq (str (:name ident)))
      (member-line source ident)
      (:line ident)))
  (title [_ ident]
    (str (or (:file ident) (:ns ident))
         (when (:name ident) (str "/" (:name ident))))))

(def impl (->GoSource))

(source/register! :go impl)
