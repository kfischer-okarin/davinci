(ns davinci.core
  (:require [lanterna.terminal :as t] [lanterna.common :as l])
  (:gen-class))

(def editor (atom {:buffer []
                   :cursor [0 0]
                   :key-bindings {}
                   :running true}))

(defn quit-editor [editor]
  (assoc editor :running false))

(defn move-cursor [dx dy editor]
  (update editor :cursor
          (fn [[x y]] [(+ x dx) (+ y dy)])))

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
  (swap! editor #(assoc-in % [:key-bindings (if (coll? key) (set key) #{key})] action)))

(defn execute-action [key editor]
  (let [action (get-in editor [:key-bindings key])]
    (if action
      (action editor)
      (if (= (count key) 1)
        (insert-character (first key) editor)))))

(bind-key [:ctrl \q] quit-editor)
(bind-key :up (partial move-cursor 0 -1))
(bind-key :right (partial move-cursor 1 0))
(bind-key :left (partial move-cursor -1 0))
(bind-key :down (partial move-cursor 0 1))
(bind-key :backspace (partial delete-previous-character))

(defn render-in-terminal
  [editor term]
  (let [[w h] (t/get-size term)]
    (t/clear term)
    (doseq [line (take (- h 1) (:buffer editor))]
      (t/put-string term (str line \newline)))
    (let [[x y] (:cursor editor)]
      (t/move-cursor term x y))))

(defn get-key-raw
  "Gets the raw Key object from the terminal"
  [terminal] (.readInput terminal))

(defn parse-key [key]
  "Parses the key into a set containing the actual key and all modifiers"
  (cond-> #{(l/parse-key key)}
    (.isCtrlPressed key) (conj :ctrl)
    (.isAltPressed key) (conj :alt)))

(defn get-key [terminal]
  (parse-key (l/block-on get-key-raw [terminal])))

(defn -main
  "I don't do a whole lot ... yet."
  ([] (println "No args"))
  ([filename]
   (swap! editor (partial open-file filename))
   (let [term (t/get-terminal :text)]
     (t/in-terminal term
                    (while (:running @editor)
                      (render-in-terminal @editor term)
                      (swap! editor (partial execute-action (get-key term))))))))
