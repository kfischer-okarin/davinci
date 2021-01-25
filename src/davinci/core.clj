(ns davinci.core
  (:require [clojure.string :as string]
            [clojure.java.shell :refer [sh]]
            [environ.core :refer [env]]
            [davinci.actions :refer :all]
            [davinci.editor :as e]
            [davinci.queries :as queries]
            [davinci.system :as s]
            [davinci.terminal :as t]
            [davinci.ui :as ui])
  (:gen-class))

(def editor (atom nil))
(def last-key (atom nil))
(def temp-file (atom nil))

(defn execute [& commands]
  (doseq [command commands]
    (swap! editor command)))

(defn load-config-file [filename]
  (binding [*ns* (find-ns 'davinci.core)]
    (apply execute (eval (s/read-all filename)))))

(defn set-editor-size [[ui-w ui-h]]
  (execute (set-size [ui-w (dec ui-h)])))

(defn handle-key [key]
  (let [action (or
                (e/get-action-for-key @editor key)
                (e/get-character-handler-action-for-key @editor key)
                do-nothing)]
    (execute action)))

(defn handle-input [input]
  (cond
    (contains? input :key) (handle-key input)
    (= (:type input) :resize) (set-editor-size (:payload input))))

(defn format-with-command-taking-file [& args]
  "Use external command to format buffer contents.
  Pass command to execute as argument array replacing the filename with the keyword :filename.
  Example: (format-with-command-taking-file \"rubocop\" \"-A\" :filename)"
  (fn [editor]
    (spit (queries/get-buffer-path editor) (queries/get-buffer-lines-as-string editor))
    (apply sh (replace {:filename (queries/get-buffer-path editor)} args))
    (set-buffer-lines (e/split-into-lines (slurp (queries/get-buffer-path editor))) editor)))

(defn format-buffer [editor]
  (cond
    (string/ends-with? (queries/get-buffer-path editor) ".rb") ((format-with-command-taking-file "rubocop" "-A" :filename) editor)
    (string/ends-with? (queries/get-buffer-path editor) ".clj") ((format-with-command-taking-file "lein" "cljfmt" "fix" :filename) editor)
    :else editor))

(defn format-and-save [editor]
  (-> editor
      (format-buffer)
      (save-file)))

(defn load-config []
  (load-config-file (str (env :davinci-path) "/default_config.clj")))

; TODO add execute command action
(defn get-available-actions []
  (keys (ns-publics 'davinci.actions)))

(defn init-terminal []
  (let [terminal (t/init-terminal)]
    (set-editor-size (t/get-size terminal))
    terminal))

(defn print-stacktrace [exception] (.printStackTrace exception))

(defn main
  "I don't do a whole lot ... yet."
  ([] (println "No args"))
  ([filename]
   (reset! editor e/initial-state)
   (load-config)
   (reset! temp-file (s/get-tempfile))
   (execute (open-file filename))
   (let [terminal (init-terminal)]
     (try
       (while (queries/is-running @editor)
         (ui/render-editor terminal @editor)
         (handle-input (ui/get-input terminal)))
       (ui/shutdown terminal)
       (catch Exception e#
         (ui/shutdown terminal)
         (print-stacktrace e#))))))

(defn -main [& args]
  (apply main args)
  (System/exit 0)) ; Quit immediately (because of possible future threads hanging around)
