(ns davinci.queries
  (:require [clojure.string :as string]))

(defn is-running [editor]
  (:running editor))

(def get-cursor :cursor)

(def get-buffer-lines :buffer)

(defn get-buffer-lines-as-string [editor]
  (string/join "\n" (get-buffer-lines editor)))

(def get-buffer-path :path)

(def get-offset :offset)

(def get-size :size)

(defn get-line [line-no editor]
  (get (get-buffer-lines editor) line-no))

(defn get-line-relative-to-cursor [dy]
  (fn [editor]
    (let [[_ y] (get-cursor editor)]
      (get-line (+ y dy) editor))))

(def get-previous-line
  (get-line-relative-to-cursor -1))

(def get-current-line
  (get-line-relative-to-cursor 0))

(def get-next-line
  (get-line-relative-to-cursor 1))

(defn get-line-count [editor]
  (count (get-buffer-lines editor)))

(defn get-max-y [editor]
  (dec (get-line-count editor)))

(defn get-max-y-offset [editor]
  (let [[w h] (get-size editor)]
    (max (- (get-line-count editor) h) 0)))

(defn get-visible-lines [editor]
  (let [[_ h] (get-size editor)
        [_ oy] (get-offset editor)]
    (subvec (get-buffer-lines editor) oy (min (+ oy h) (get-line-count editor)))))

(defn get-position-left-of-cursor [editor]
  (let [[x y] (get-cursor editor)]
    (if (pos? x)
      [(dec x) y]
      (let [previous-line (get-previous-line editor)]
        (if previous-line
          [(count previous-line) (dec y)]
          [x y])))))

(defn get-position-right-of-cursor [editor]
  (let [[x y] (get-cursor editor)
        current-line (get-current-line editor)
        current-line-length (count current-line)]
    (if (< x current-line-length)
      [(inc x) y]
      (let [next-line (get-next-line editor)]
        (if next-line
          [0 (inc y)]
          [x y])))))

(defn get-position-up-of-cursor [editor]
  (let [[x y] (get-cursor editor)
        previous-line (get-previous-line editor)]
    (if-not previous-line
      [x y]
      (let [previous-line-length (count previous-line)]
        [(min previous-line-length x) (dec y)]))))

(defn get-position-down-of-cursor [editor]
  (let [[x y] (get-cursor editor)
        next-line (get-next-line editor)]
    (if-not next-line
      [x y]
      (let [next-line-length (count next-line)]
        [(min next-line-length x) (inc y)]))))
