(ns davinci.core
  (:require [clojure.string :as string]
            [clojure.java.shell :refer [sh]]
            [davinci.actions :refer :all]
            [davinci.editor :as e]
            [davinci.queries :as queries]
            [davinci.system :as s]
            [davinci.terminal :as t])
  (:gen-class))

(def editor (atom nil))
(def last-key (atom nil))
(def temp-file (atom nil))

(defn execute-action [action]
  (swap! editor action))

(defn execute-actions [& actions]
  (doseq [action actions] (execute-action action)))

(defn handle-key [key]
  (reset! last-key key)
  (let [action (or
                (e/get-action-for-key @editor key)
                (e/get-character-handler-action-for-key @editor key)
                do-nothing)]
    (execute-action action)))

(defn format-with-command-taking-file [& args]
  "Use external command to format buffer contents.
  Pass command to execute as argument array replacing the filename with the keyword :filename.
  Example: (format-with-command-taking-file \"rubocop\" \"-A\" :filename)"
  (fn [editor]
    (spit (:path editor) (queries/get-buffer-as-string editor))
    (apply sh (replace {:filename (:path editor)} args))
    ((set-buffer-to-string (slurp (:path editor))) editor)))

(defn format-buffer [editor]
  (cond
    (string/ends-with? (:path editor) ".rb") ((format-with-command-taking-file "rubocop" "-A" :filename) editor)
    (string/ends-with? (:path editor) ".clj") ((format-with-command-taking-file "lein" "cljfmt" "fix" :filename) editor)
    :else editor))

(defn format-and-save [editor]
  (-> editor
      (format-buffer)
      (save-file)))

(defn init-keybindings
  []
  (execute-actions
   (add-key-binding \w :ctrl quit-editor)
   (add-key-binding :up move-cursor-up)
   (add-key-binding :right move-cursor-right)
   (add-key-binding :left move-cursor-left)
   (add-key-binding :down move-cursor-down)
   (add-key-binding :page-up page-up)
   (add-key-binding :page-down page-down)
   (add-key-binding :home move-cursor-to-beginning-of-line)
   (add-key-binding :end move-cursor-to-end-of-line)
   (add-key-binding :backspace delete-previous-character)
   (add-key-binding :enter insert-newline)
   (add-key-binding :tab (insert-string "  "))
   (add-key-binding \x :ctrl (set-key-modifier :command-mode))
   (add-key-binding \x :command-mode (unset-key-modifier :command-mode))
   (add-key-binding \s :command-mode format-and-save)
   (add-key-binding \k :command-mode delete-line)
   (add-key-binding \l :command-mode delete-until-end-of-line)
   (add-key-binding \j :command-mode delete-from-beginning-of-line)
   (add-key-binding \d :command-mode duplicate-line)
   (set-character-handler insert-character)))

; TODO add execute command action
(defn get-available-actions []
  (keys (ns-publics 'davinci.actions)))

(defn set-editor-size [[terminal-w terminal-h]]
  (execute-action (set-size [terminal-w (dec terminal-h)])))

(defn render-two-part-status-bar [terminal left-content right-content]
  (let [[w _] (queries/get-size @editor)
        left-w (quot w 2)
        right-w (- w left-w)]
    (t/put-string terminal (format (str "%-" left-w "s") left-content) :white :red)
    (t/put-string terminal (format (str "%" right-w "s") right-content) :white :red)))

(defn render-status-bar [terminal]
  (let [[_ h] (queries/get-size @editor)
        [x y] (queries/get-cursor @editor)
        position (str (:path @editor) ":" (inc y) ":" (inc x))]
    (t/move-cursor terminal 0 h)
    (if (contains? (:key-modifiers @editor) :command-mode)
      (render-two-part-status-bar terminal position "COMMAND MODE")
      (render-two-part-status-bar terminal position (str "Last key: " @last-key)))))

(defn render-in-terminal
  [term]
  (t/clear term)
  (doseq [line (queries/get-visible-lines @editor)]
    (t/put-string term (str line \newline)))
  (let [[x y] (queries/get-cursor @editor)
        [ox oy] (:offset @editor)]
    (render-status-bar term)
    (t/move-cursor term (- x ox) (- y oy)))
  (t/flush-terminal term))

(defn init-terminal []
  (let [terminal (t/get-terminal)]
    (set-editor-size (t/get-size terminal))
    (t/add-resize-listener terminal
                           #(do
                              (set-editor-size %)
                              (render-in-terminal terminal)))
    terminal))

(defn main
  "I don't do a whole lot ... yet."
  ([] (println "No args"))
  ([filename]
   (reset! editor e/initial-state)
   (init-keybindings)
   (reset! temp-file (s/get-tempfile))
   (execute-action (open-file filename))
   (let [term (init-terminal)]
     (t/in-terminal term
                    (while (queries/is-running @editor)
                      (render-in-terminal term)
                      (handle-key (t/get-key term)))))))

(defn -main [& args]
  (apply main args)
  (System/exit 0)) ; Quit immediately (because of possible future threads hanging around)
