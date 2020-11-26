(ns editor.core
  (:gen-class))

(def buffer (atom []))

(defn open-file [filename]
  (reset! buffer (clojure.string/split (slurp filename) #"\n")))

(require '[lanterna.terminal :as t])

(def term (t/get-terminal :text))

(defn render-buffer
  []
  (let [[w h] (t/get-size term)]
    (t/move-cursor term 0 0)
    (doseq [line (take h @buffer)]
      (t/put-string term (str line \newline)))))

(defn -main
  "I don't do a whole lot ... yet."
  ([] (println "No args"))
  ([filename]
   (open-file filename)
   (t/in-terminal term
                  (render-buffer)
                  (t/get-key-blocking term))))
