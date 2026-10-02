(ns uml-viewer.adapters.core
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [uml-viewer.adapters.sketch :as sketch]
            [uml-viewer.application.document :as document]
            [uml-viewer.application.events :as events]
            [uml-viewer.engine.hit :as hit]))

(def help-text
  (str "Usage: clj -M:run [options] [edn-file]\n"
       "\n"
       "  edn-file          Diagram to watch (default: examples/library.edn).\n"
       "                    A fresh start waits for the companion Grok to send\n"
       "                    :display unless the associated agent recycles\n"
       "                    the window with :uml-viewer-restart, or you press R.\n"
       "\n"
       "  --restart         Associated agent only (via :uml-viewer-restart).\n"
       "                    New JVM, keep the existing Grok tmux session.\n"
       "                    Reloads the last view (depth, pan, zoom, proposal).\n"
       "                    Do not use this if no companion is attached.\n"
       "\n"
       "  --snapshot FILE   Draw the diagram once, save the window to FILE as\n"
       "                    a PNG, and exit. No companion is started and no\n"
       "                    mail is read. The directory of FILE must exist.\n"
       "\n"
       "  --focus ID        With --snapshot only: open the node with this\n"
       "                    dotted id (for example adapters) before drawing.\n"
       "\n"
       "  -h, --help        Print this help and exit.\n"))

(def ^:private value-flags
  {"--snapshot" :snapshot
   "--focus" :focus})

(defn parse-args
  "EDN path and flags. `--restart` skips spawning a new agent.
  `:snapshot` and `:focus` appear only when their flags are given."
  [args]
  (loop [args (keep identity args)
         parsed {:help? false :restart? false :path nil}]
    (if-let [[arg & more] (seq args)]
      (cond
        (#{"--help" "-h"} arg) (recur more (assoc parsed :help? true))
        (= "--restart" arg) (recur more (assoc parsed :restart? true))
        (value-flags arg) (recur (rest more)
                                 (assoc parsed (value-flags arg) (first more)))
        (:path parsed) (recur more parsed)
        :else (recur more (assoc parsed :path arg)))
      (update parsed :path #(or % "examples/library.edn")))))

(defn- exit! [code]
  (System/exit code))

(defn- fail! [message]
  (binding [*out* *err*]
    (println message))
  (exit! 1))

(defn- node-ids
  "Ids from the top node down to `dotted-id`: \"a.b\" is [:a :a.b]."
  [dotted-id]
  (->> (str/split dotted-id #"\.")
       (reductions #(str %1 "." %2))
       (map keyword)))

(defn focus-state
  "`state` opened at the node `dotted-id` names, as if each box on the way
  was double-clicked. Nil when a step is not a box that opens."
  [state dotted-id]
  (reduce (fn [state id]
            (if (:drill? (hit/class-by-id (:scene state) id))
              (events/drill state id)
              (reduced nil)))
          state
          (node-ids dotted-id)))

(defn- start-snapshot! [path png-path focus]
  (let [png-file (.getAbsoluteFile (io/file png-path))
        loaded (document/load-path path)
        state (if focus (focus-state loaded focus) loaded)]
    (cond
      (:error loaded)
      (fail! (:error loaded))

      (nil? state)
      (fail! (str "--focus names no node that can be opened: " focus))

      ;; Processing creates missing directories when it saves a frame.
      (not (.isDirectory (.getParentFile png-file)))
      (fail! (str "snapshot directory not found: " (.getParent png-file)))

      :else
      (sketch/snapshot! state (.getPath png-file)))))

(defn start!
  "Launch the viewer. `source-impl` satisfies `LanguageSource`."
  [source-impl & args]
  (let [{:keys [path restart? help? snapshot focus]} (parse-args args)]
    (cond
      help?
      (do (print help-text) :help)

      snapshot
      (start-snapshot! path snapshot focus)

      focus
      (fail! (str "--focus is only supported with --snapshot.\n\n" help-text))

      :else
      (do
        (sketch/start! path source-impl restart?)
        (println "Watching" path)
        (println "Double-click a class for its card. Scroll to pan (Shift-scroll for horizontal). Ctrl+/− zoom; Ctrl+0 resets. R reloads. Click the real diagram above Proposals, or a proposal to show it.")))))
