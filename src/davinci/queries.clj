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
        [_ oy] (get-offset editor)]
    (subvec (get-buffer-lines editor) oy (min (+ oy h) (count (get-buffer-lines editor))))))

(defn get-position-left-of-cursor [editor]
  (let [[x y] (get-cursor editor)
        lines (get-buffer-lines editor)]
    (if (pos? x)
      [(dec x) y]
      (if (pos? y)
        [(lines/get-length lines (dec y)) (dec y)]
        [x y]))))

(defn get-position-right-of-cursor [editor]
  (let [[x y] (get-cursor editor)
        lines (get-buffer-lines editor)
        current-line-length (lines/get-length lines y)]
    (if (< x current-line-length)
      [(inc x) y]
      (if (< y (lines/max-y lines))
        [0 (inc y)]
        [x y]))))

(defn get-position-up-of-cursor [editor]
  (let [[x y] (get-cursor editor)
        lines (get-buffer-lines editor)]
    (if (> y 0)
      (let [previous-line-length (lines/get-length lines (dec y))]
        [(min previous-line-length x) (dec y)])
      [x y])))

(defn get-position-down-of-cursor [editor]
  (let [[x y] (get-cursor editor)
        lines (get-buffer-lines editor)]
    (if (< y (lines/max-y lines))
      (let [next-line-length (lines/get-length lines (inc y))]
        [(min next-line-length x) (inc y)])
      [x y])))
