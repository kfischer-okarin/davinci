(ns editor.core
  (:gen-class))

(require '[lanterna.terminal :as t])

(def term (t/get-terminal :text))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (t/start term)
  (t/get-key-blocking term))
