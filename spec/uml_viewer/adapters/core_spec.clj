(ns uml-viewer.adapters.core-spec
  (:require [clojure.java.io :as io]
            [speclj.core :refer :all]
            [uml-viewer.adapters.core :as core]
            [uml-viewer.adapters.sketch :as sketch]
            [uml-viewer.application.document :as document]))

(defn- tree-state []
  (let [doc {:hierarchical true
             :title "Demo"
             :classes [{:id :shop.cart :name "Cart" :ns "demo.shop.cart"}
                       {:id :shop.billing.invoice :name "Invoice"
                        :ns "demo.shop.billing.invoice"}
                       {:id :shop.billing.tax :name "Tax"
                        :ns "demo.shop.billing.tax"}
                       {:id :report :name "Report" :ns "demo.report"}]
             :edges []}]
    {:doc doc
     :path "demo.edn"
     :focus []
     :scene (document/compile-view doc (System/getProperty "java.io.tmpdir") [] false)}))

(defn- scene-ids [state]
  (set (map :id (:classes (:scene state)))))

(defn- run-start
  "Run `core/start!` with the sketch and the JVM exit stubbed.
  Returns what was started, the exit codes, and the printed text."
  [loaded & args]
  (let [calls (atom {:live [] :snapshot [] :exits []})
        err (java.io.StringWriter.)
        out (with-redefs [document/load-path (fn [_] loaded)
                          sketch/start! (fn [& a] (swap! calls update :live conj a))
                          sketch/snapshot! (fn [& a] (swap! calls update :snapshot conj a))
                          uml-viewer.adapters.core/exit! (fn [code] (swap! calls update :exits conj code))]
              (binding [*err* err]
                (with-out-str (apply core/start! :src args))))]
    (assoc @calls :out out :err (str err))))

(def ^:private writable-png
  (.getPath (io/file (System/getProperty "java.io.tmpdir") "uml-viewer-spec.png")))

(describe "cli args"
  (it "defaults the path and does not restart"
    (should= {:path "examples/library.edn" :restart? false :help? false}
             (core/parse-args nil))
    (should= {:path "examples/library.edn" :restart? false :help? false}
             (core/parse-args [])))

  (it "takes a path and the --restart flag in either order"
    (should= {:path "doc.edn" :restart? false :help? false}
             (core/parse-args ["doc.edn"]))
    (should= {:path "examples/library.edn" :restart? true :help? false}
             (core/parse-args ["--restart"]))
    (should= {:path "doc.edn" :restart? true :help? false}
             (core/parse-args ["--restart" "doc.edn"]))
    (should= {:path "doc.edn" :restart? true :help? false}
             (core/parse-args ["doc.edn" "--restart"])))

  (it "prints a description of the arguments on --help"
    (should= {:path "examples/library.edn" :restart? false :help? true}
             (core/parse-args ["--help"]))
    (should (:help? (core/parse-args ["-h" "doc.edn"])))
    (should (re-find #"edn-file" core/help-text))
    (should (re-find #"--restart" core/help-text))
    (with-redefs [sketch/start! (fn [& _] (throw (Exception. "should not start")))]
      (let [ret (atom nil)
            out (with-out-str (reset! ret (core/start! :unused "--help")))]
        (should= :help @ret)
        (should (re-find #"Usage: clj -M:run" out)))))

  (it "takes --snapshot and --focus with their values, in any position"
    (should= {:path "x.edn" :restart? false :help? false
              :snapshot "o.png" :focus "a.b"}
             (core/parse-args ["--snapshot" "o.png" "--focus" "a.b" "x.edn"]))
    (should= {:path "x.edn" :restart? false :help? false
              :snapshot "o.png" :focus "a.b"}
             (core/parse-args ["x.edn" "--focus" "a.b" "--snapshot" "o.png"]))
    (should= {:path "x.edn" :restart? false :help? false :snapshot "o.png"}
             (core/parse-args ["--snapshot" "o.png" "x.edn"]))
    (should= {:path "examples/library.edn" :restart? false :help? false
              :focus "a.b"}
             (core/parse-args ["--focus" "a.b"])))

  (it "starts the sketch when not asking for help"
    (let [args (atom nil)]
      (with-redefs [sketch/start! (fn [& a] (reset! args a) :started)]
        (let [out (with-out-str (core/start! :src "doc.edn"))]
          (should= ["doc.edn" :src false] @args)
          (should (re-find #"Watching" out))
          (should (re-find #"real diagram above Proposals" out))))))

  (it "describes --snapshot and --focus in the help"
    (should (re-find #"--snapshot" core/help-text))
    (should (re-find #"--focus" core/help-text))))

(describe "focus"
  (it "opens each node along a dotted id"
    (let [shop (core/focus-state (tree-state) "shop")
          billing (core/focus-state (tree-state) "shop.billing")]
      (should= [:shop] (:focus shop))
      (should= #{:shop.cart :shop.billing} (scene-ids shop))
      (should= [:shop :billing] (:focus billing))
      (should= #{:shop.billing.invoice :shop.billing.tax} (scene-ids billing))))

  (it "is nil when a step names no box that opens"
    (should-be-nil (core/focus-state (tree-state) "no.such.node"))
    (should-be-nil (core/focus-state (tree-state) "shop.nowhere"))
    (should-be-nil (core/focus-state (tree-state) "billing"))
    (should-be-nil (core/focus-state (tree-state) "report"))
    (should-be-nil (core/focus-state (tree-state) "shop.cart")))

  (it "is nil when a proposal group with the same id would open instead"
    (let [grouped (assoc-in (tree-state) [:doc :proposals]
                            [{:id :split :name "split"
                              :layers [{:id :front :label "Front"
                                        :nses [{:id :shop
                                                :label "Shop"
                                                :nses [:shop.cart]}]}]}])]
      (should-be-nil (core/focus-state grouped "shop")))))

(describe "snapshot start"
  (it "hands the loaded diagram and the absolute PNG path to the snapshot sketch"
    (let [loaded (tree-state)
          ran (run-start loaded "--snapshot" writable-png "demo.edn")]
      (should= [[loaded (.getAbsolutePath (io/file writable-png))]] (:snapshot ran))
      (should= [] (:live ran))
      (should= [] (:exits ran))
      (should= "" (:out ran))))

  (it "resolves a relative PNG path against the working directory"
    (let [ran (run-start (tree-state) "--snapshot" "relative.png" "demo.edn")]
      (should= (.getAbsolutePath (io/file "relative.png"))
               (second (first (:snapshot ran))))))

  (it "opens the --focus node before drawing"
    (let [ran (run-start (tree-state) "--snapshot" writable-png "--focus" "shop.billing" "demo.edn")
          [state] (first (:snapshot ran))]
      (should= [:shop :billing] (:focus state))
      (should= #{:shop.billing.invoice :shop.billing.tax} (scene-ids state))
      (should= [] (:exits ran))))

  (it "prints the load error and exits 1 without a sketch"
    (let [ran (run-start {:path "missing.edn" :error "file not found: missing.edn"}
                         "--snapshot" writable-png "missing.edn")]
      (should= [1] (:exits ran))
      (should= [] (:snapshot ran))
      (should= [] (:live ran))
      (should (re-find #"file not found: missing.edn" (:err ran)))))

  (it "exits 1 when --focus names no node"
    (let [ran (run-start (tree-state) "--snapshot" writable-png "--focus" "no.such.node" "demo.edn")]
      (should= [1] (:exits ran))
      (should= [] (:snapshot ran))
      (should (re-find #"--focus names no node that can be opened: no.such.node" (:err ran)))))

  (it "exits 1 when the PNG directory does not exist, and does not create it"
    (let [missing-dir (io/file (System/getProperty "java.io.tmpdir")
                               (str "uml-viewer-no-dir-" (System/nanoTime)))
          ran (run-start (tree-state) "--snapshot" (.getPath (io/file missing-dir "x.png")) "demo.edn")]
      (should= [1] (:exits ran))
      (should= [] (:snapshot ran))
      (should (re-find #"snapshot directory not found" (:err ran)))
      (should-not (.exists missing-dir))))

  (it "exits 1 when the snapshot name does not end in .png"
    (doseq [args [["--snapshot" "out" "demo.edn"]
                  ["--snapshot" "out.jpg" "demo.edn"]
                  ["--snapshot" "--restart" "demo.edn"]
                  ["--snapshot" "demo.edn"]]]
      (let [ran (apply run-start (tree-state) args)]
        (should= [1] (:exits ran))
        (should= [] (:snapshot ran))
        (should= [] (:live ran))
        (should (re-find (re-pattern (str "snapshot file must end in \\.png: " (second args)))
                         (:err ran)))))
    (should= [] (:exits (run-start (tree-state) "--snapshot" "UPPER.PNG" "demo.edn"))))

  (it "prints usage and exits 1 for --focus without --snapshot"
    (let [ran (run-start (tree-state) "--focus" "shop" "demo.edn")]
      (should= [1] (:exits ran))
      (should= [] (:snapshot ran))
      (should= [] (:live ran))
      (should (re-find #"--focus is only supported with --snapshot" (:err ran)))
      (should (re-find #"Usage: clj -M:run" (:err ran)))))

  (it "prints usage and exits 1 when --snapshot or --focus has no value"
    (doseq [args [["demo.edn" "--snapshot"]
                  ["--snapshot" writable-png "demo.edn" "--focus"]]]
      (let [ran (apply run-start (tree-state) args)]
        (should= [1] (:exits ran))
        (should= [] (:snapshot ran))
        (should= [] (:live ran))
        (should (re-find #"--snapshot and --focus each need a value" (:err ran)))
        (should (re-find #"Usage: clj -M:run" (:err ran)))))))
