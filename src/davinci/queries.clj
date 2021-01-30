(ns davinci.queries
  (:require [davinci.lines :as lines]))

(defn is-running [editor]
  (:running editor))

(def get-cursor :cursor)

(defn get-buffer-lines [editor]
  (get-in editor [:buffer :lines]))

(defn get-buffer-path [editor]
  (get-in editor [:buffer :path]))

(defn get-buffer-type [editor]
  (get-in editor [:buffer :type]))

(def get-offset :offset)

(def get-size :size)

(defn get-max-y-offset [editor]
  (let [[w h] (get-size editor)]
    (max (- (count (get-buffer-lines editor)) h) 0)))

(defn get-visible-lines [editor]
  (let [[_ h] (get-size editor)
        [_ oy] (get-offset editor)
        lines-count (min (+ oy h) (count (get-buffer-lines editor)))]
    (lines/get-lines (get-buffer-lines editor) oy lines-count)))
