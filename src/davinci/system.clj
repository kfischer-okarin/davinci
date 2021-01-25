(ns davinci.system
  (:require [clojure.java.io :as io])
  (:import java.io.File
           java.io.PushbackReader))

(defn get-tempfile []
  (let [tempfile (File/createTempFile "davinci" nil)]
    (.deleteOnExit tempfile)
    tempfile))

(defn read-all [filename]
  "Reads a file and returns an unevaluated list of all forms in the file"
  (let [rdr (-> filename io/file io/reader PushbackReader.)]
    (loop [forms []]
      (let [form (try (read rdr) (catch Exception e nil))]
        (if form
          (recur (conj forms form))
          forms)))))
