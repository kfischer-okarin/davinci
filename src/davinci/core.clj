(ns davinci.core
  (:require [davinci.terminal :as t])
  (:gen-class))

(def editor (atom {:buffer []
                   :cursor [0 0]
                   :key-bindings {}
                   :running true}))

(defn fix-cursor-position [editor [x y]]
  "Keep cursor inside buffer with text"
  (let [max-y (- (count (:buffer editor)) 1)
        fixed-y (min (max 0 y) max-y)
        current-line (get-in editor [:buffer fixed-y])
        max-x (count current-line)
        fixed-x (min (max 0 x) max-x)]
    [fixed-x fixed-y]))

(defn quit-editor [editor]
  (assoc editor :running false))

(defn move-cursor [dx dy editor]
  (update editor :cursor
          (comp
           (partial fix-cursor-position editor)
           (fn [[x y]]  [(+ x dx) (+ y dy)]))))

(defn insert-character [character editor]
  (let [[x y] (:cursor editor)]
    (-> editor
        (update-in [:buffer y] #(str (subs % 0 x) character (subs % x)))
        (update-in [:cursor 0] #(+ % 1)))))

(defn delete-previous-character [editor]
  (let [[x y] (:cursor editor) prev-x (- x 1)]
    (-> editor
        (update-in [:buffer y] #(str (subs % 0 prev-x) (subs % x)))
        (update-in [:cursor 0] #(- % 1)))))

(defn open-file [filename editor]
  (assoc editor :buffer
         (clojure.string/split (slurp filename) #"\n")))

(defn bind-key [key action]
  (swap! editor #(assoc-in % [:key-bindings (if (map? key) key {:key key :modifiers #{}})] action)))

(defn execute-action [key editor]
  (let [action (get-in editor [:key-bindings key])]
    (if action
      (action editor)
      (if (and (char? (:key key)) (empty? (:modifiers key)))
        (insert-character (:key key) editor)
        editor))))

(bind-key {:key \q :modifiers #{:ctrl}} quit-editor)
(bind-key :up (partial move-cursor 0 -1))
(bind-key :right (partial move-cursor 1 0))
(bind-key :left (partial move-cursor -1 0))
(bind-key :down (partial move-cursor 0 1))
(bind-key :backspace delete-previous-character)

(defn render-in-terminal
  [editor term]
  (let [[w h] (t/get-size term)]
    (t/clear term)
    (doseq [line (take (- h 1) (:buffer editor))]
      (t/put-string term (str line \newline)))
    (let [[x y] (:cursor editor)]
      (t/move-cursor term x y))))

(defn -main
  "I don't do a whole lot ... yet."
  ([] (println "No args"))
  ([filename]
   (swap! editor (partial open-file filename))
   (let [term (t/get-terminal)]
     (t/in-terminal term
                    (while (:running @editor)
                      (render-in-terminal @editor term)
                      (swap! editor (partial execute-action (t/get-key term))))))))
