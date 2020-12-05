(ns davinci.core
  (:require [davinci.actions :refer :all]
            [davinci.editor :as e]
            [davinci.queries :as queries]
            [davinci.terminal :as t])
  (:gen-class))

(def last-key (atom nil))

(defn handle-key [key]
  (reset! last-key key)
  (let [action (or
                (e/get-action-for-key key)
                (e/get-character-handler-action-for-key key)
                do-nothing)]
    (e/execute-action action)))

(e/execute-actions
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
 (add-key-binding \x :ctrl (set-key-modifier :command-mode))
 (add-key-binding \x :command-mode (unset-key-modifier :command-mode))
 (add-key-binding \s :command-mode save-file)
 (set-character-handler #(insert-character %)))

; TODO add execute command action
(defn get-available-actions []
  (keys (ns-publics 'davinci.actions)))

(defn set-editor-size [[terminal-w terminal-h]]
  (e/execute-action (set-size [terminal-w (dec terminal-h)])))

(defn render-two-part-status-bar [terminal left-content right-content]
  (let [[w _] (e/get-value :size) left-w (quot w 2) right-w (- w left-w)]
    (t/put-string terminal (format (str "%-" left-w "s") left-content) :white :red)
    (t/put-string terminal (format (str "%" right-w "s") right-content) :white :red)))

(defn render-status-bar [terminal]
  (let [[_ h] (e/get-value :size)]
    (t/move-cursor terminal 0 h)
    (if (contains? (e/get-value :key-modifiers) :command-mode)
      (render-two-part-status-bar terminal (e/get-value :path) "COMMAND MODE")
      (render-two-part-status-bar terminal (e/get-value :path) (str "Last key: " @last-key)))))

(defn render-in-terminal
  [term]
  (t/clear term)
  (doseq [line (e/get-value queries/get-visible-lines)]
    (t/put-string term (str line \newline)))
  (let [[x y] (e/get-value :cursor)
        [ox oy] (e/get-value :offset)]
    (render-status-bar term)
    (t/move-cursor term (- x ox) (- y oy)))
  (t/flush term))

(defn init-terminal []
  (let [terminal (t/get-terminal)]
    (set-editor-size (t/get-size terminal))
    (t/add-resize-listener terminal
                           #(do
                              (set-editor-size %)
                              (render-in-terminal terminal)))
    terminal))

(defn -main
  "I don't do a whole lot ... yet."
  ([] (println "No args"))
  ([filename]
   (e/execute-action (open-file filename))
   (let [term (init-terminal)]
     (t/in-terminal term
                    (while (e/get-value :running)
                      (render-in-terminal term)
                      (handle-key (t/get-key term)))))))
