(ns davinci.system
  (:import java.io.File))

(defn get-tempfile []
  (let [tempfile (File/createTempFile "davinci" nil)]
    (.deleteOnExit tempfile)
    tempfile))
