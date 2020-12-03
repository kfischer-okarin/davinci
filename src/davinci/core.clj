(ns davinci.core
  (:require [davinci.actions :refer :all]
            [davinci.editor :as e]
            [davinci.queries :as queries]
            [davinci.terminal :as t])
  (:gen-class))

(def last-key (atom nil))

(defn- character-without-modifier [key]
  (and (char? (:key key)) (empty? (:modifiers key))))

(defn handle-key [key]
  (reset! last-key key)
  (let [action (or
                (e/get-action-for-key key)
                (if (character-without-modifier key)
                  (insert-character (:key key))
                  do-nothing))]
    (e/execute-action action)))

(e/bind-key {:key \w :modifiers #{:ctrl}} quit-editor)
(e/bind-key :up move-cursor-up)
(e/bind-key :right move-cursor-right)
(e/bind-key :left move-cursor-left)
(e/bind-key :down move-cursor-down)
(e/bind-key :page-up page-up)
(e/bind-key :page-down page-down)
(e/bind-key :backspace delete-previous-character)
(e/bind-key :enter insert-newline)

(defn set-editor-size [[terminal-w terminal-h]]
  (e/execute-action (set-size [terminal-w (dec terminal-h)])))

(defn render-in-terminal
  [term]
  (t/clear term)
  (doseq [line (e/get-value queries/get-visible-lines)]
    (t/put-string term (str line \newline)))
  (let [[x y] (e/get-value :cursor)
        [w h] (e/get-value :size)
        [ox oy] (e/get-value :offset)]
    (t/move-cursor term 0 h)
    (t/put-string term (str "Last key: " @last-key))
    (t/move-cursor term (- x ox) (- y oy))))

(defn -main
  "I don't do a whole lot ... yet."
  ([] (println "No args"))
  ([filename]
   (e/execute-action (open-file filename))
   (let [term (t/get-terminal)]
     (t/in-terminal term
                    (set-editor-size (t/get-size term))
                    (while (e/get-value :running)
                      (render-in-terminal term)
                      (handle-key (t/get-key term)))))))
