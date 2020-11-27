(ns editor.core
  (:gen-class))

(def buffer (atom []))

(def key-bindings (atom {}))

(def cursor (atom [0 0]))
(defn move-cursor [dx dy]
  (swap! cursor #(let [[x y] %] [(+ x dx) (+ y dy)])))

(def will-quit (atom false))
(defn quit-editor [] (reset! will-quit true))

(defn open-file [filename]
  (reset! buffer (clojure.string/split (slurp filename) #"\n")))

(require '[lanterna.terminal :as t])

(def term (t/get-terminal :text))

(defn render-buffer
  []
  (let [[w h] (t/get-size term)]
    (t/move-cursor term 0 0)
    (doseq [line (take h @buffer)]
      (t/put-string term (str line \newline)))
    (let [[x y] @cursor]
      (t/move-cursor term x y))))

(defn bind-key [key action]
  (swap! key-bindings #(assoc % key action)))

(defn execute-action [key]
  (let [action (get @key-bindings key)] (if action (action))))

(bind-key \q (fn [] (quit-editor)))
(bind-key :up (fn [] (move-cursor 0 -1)))
(bind-key :right (fn [] (move-cursor 1 0)))
(bind-key :left (fn [] (move-cursor -1 0)))
(bind-key :down (fn [] (move-cursor 0 1)))

(defn -main
  "I don't do a whole lot ... yet."
  ([] (println "No args"))
  ([filename]
   (open-file filename)
   (t/in-terminal term
                  (while (not @will-quit)
                    (render-buffer)
                    (execute-action (t/get-key-blocking term))))))
