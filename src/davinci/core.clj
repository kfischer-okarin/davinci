(ns davinci.core
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

(defn open-file [filename editor]
  (assoc editor :buffer
         (clojure.string/split (slurp filename) #"\n")))

(defn bind-key [key action]
  (swap! editor #(assoc-in % [:key-bindings key] action)))

(defn execute-action [key editor]
  (let [action (get-in editor [:key-bindings key])]
    (if action
      (action editor)
      editor)))

(bind-key \q quit-editor)
(bind-key :up (partial move-cursor 0 -1))
(bind-key :right (partial move-cursor 1 0))
(bind-key :left (partial move-cursor -1 0))
(bind-key :down (partial move-cursor 0 1))

(require '[lanterna.terminal :as t])

(defn render-in-terminal
  [editor term]
  (let [[w h] (t/get-size term)]
    (t/move-cursor term 0 0)
    (doseq [line (take h (:buffer editor))]
      (t/put-string term (str line \newline)))
    (let [[x y] (:cursor editor)]
      (t/move-cursor term x y))))

(defn -main
  "I don't do a whole lot ... yet."
  ([] (println "No args"))
  ([filename]
   (swap! editor (partial open-file filename))
   (let [term (t/get-terminal :text)]
     (t/in-terminal term
                    (while (:running @editor)
                      (render-in-terminal @editor term)
                      (swap! editor (partial execute-action (t/get-key-blocking term))))))))
