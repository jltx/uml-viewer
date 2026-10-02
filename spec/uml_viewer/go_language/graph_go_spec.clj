(ns uml-viewer.go-language.graph-go-spec
  (:require [clojure.java.io :as io]
            [speclj.core :refer :all]
            [uml-viewer.go-language.graph-go :as graph-go]))

(def ^:private fixture-root "spec/fixtures/go/demo")

(def ^:private go-installed?
  (delay (try (zero? (-> (ProcessBuilder. ["go" "version"])
                         (.redirectOutput java.lang.ProcessBuilder$Redirect/DISCARD)
                         (.redirectError java.lang.ProcessBuilder$Redirect/DISCARD)
                         .start
                         .waitFor))
              (catch java.io.IOException _ false))))

(defmacro ^:private with-go [& body]
  `(if @go-installed?
     (do ~@body)
     (println "go not found; Go scan skipped")))

(defn- temp-copy-of-fixture []
  (let [fixture (io/file fixture-root)
        copy-root (io/file (System/getProperty "java.io.tmpdir")
                           (str "uml-go-" (System/nanoTime)))]
    (doseq [file (file-seq fixture)
            :when (.isFile file)]
      (let [copy (io/file copy-root (str (.relativize (.toPath fixture) (.toPath file))))]
        (io/make-parents copy)
        (io/copy file copy)))
    copy-root))

(defn- thrown-by [scan-thunk]
  (try (scan-thunk)
       nil
       (catch clojure.lang.ExceptionInfo failure failure)))

(describe "go scan facts"
  (it "reports the module's packages, each with its dotted namespace"
    (with-go
      (let [facts (graph-go/scan-facts fixture-root {:goos "linux"})]
        (should= "example.com/demo" (:module facts))
        (should= "linux" (:goos facts))
        (should= ["example.com.demo"
                  "example.com.demo.cmd.demo"
                  "example.com.demo.internal.util"
                  "example.com.demo.pkg.util"
                  "example.com.demo.store"]
                 (map :ns (:packages facts)))
        (should= ["store/open.go" "store/query.go" "store/store_linux.go"]
                 (:files (last (:packages facts)))))))

  (it "inherits GOOS from the environment when no go opts are given"
    (with-go
      (let [facts (graph-go/scan-facts fixture-root nil)]
        (should (seq (:goos facts)))
        (should= 5 (count (:packages facts))))))

  (it "throws with the helper's stderr when the helper fails"
    (with-go
      (let [broken-root (temp-copy-of-fixture)]
        (spit (io/file broken-root "store/query.go") "package store\n\nfunc broken( {\n")
        (let [failure (thrown-by #(graph-go/scan-facts broken-root {:goos "linux"}))]
          (should-not-be-nil failure)
          (should-contain "store/query.go" (ex-message failure)))))))
