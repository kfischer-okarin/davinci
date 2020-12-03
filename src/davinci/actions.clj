(ns davinci.actions
  (:require [davinci.queries :refer :all]))

(defn quit-editor [editor]
  (assoc editor :running false))

(defn- scroll-to-cursor [editor]
  (let [[x y] (:cursor editor) [w h] (:size editor) [ox oy] (:offset editor)]
    (cond
      (>= y (+ h oy)) (assoc-in editor [:offset 1] (- y (dec h)))
      (< y oy) (assoc-in editor [:offset 1] y)
      :else editor)))

(defn move-cursor-left [editor]
  (assoc editor :cursor (position-left-of-cursor editor)))

(defn move-cursor-right [editor]
  (assoc editor :cursor (position-right-of-cursor editor)))

(defn move-cursor-up [editor]
  (-> editor
      (assoc :cursor (position-up-of-cursor editor))
      (scroll-to-cursor)))

(defn move-cursor-down [editor]
  (-> editor
      (assoc :cursor (position-down-of-cursor editor))
      (scroll-to-cursor)))

(defn replace-lines [[start end] new-lines]
  (fn [editor]
    (update editor :buffer #(into [] cat [(take start %) new-lines (drop end %)]))))

(defn delete-previous-character [editor]
  (let [[x y] (:cursor editor)]
    (if (pos? x)
      (-> editor
          (update-in [:buffer y] #(str (subs % 0 (dec x)) (subs % x)))
          move-cursor-left)
      (if (pos? y)
        (let [previous-line (get-previous-line editor)
              merged-with-previous-line (str previous-line (get-current-line editor))
              merge-lines (replace-lines [(dec y) (inc y)] [merged-with-previous-line])]
          (-> editor
              merge-lines
              (assoc :cursor [(count previous-line) (dec y)])))
        editor))))

(defn insert-newline [editor]
  (let [[x y] (:cursor editor)
        current-line (get-current-line editor)
        before-cursor (subs current-line 0 x)
        after-cursor (subs current-line x)
        split-line-at-cursor (replace-lines [y (inc y)] [before-cursor after-cursor])]
    (-> editor
        split-line-at-cursor
        move-cursor-right)))

(def do-nothing identity)

(defn open-file [filename]
  #(assoc % :buffer (conj (clojure.string/split (slurp filename) #"\n") "")))

(defn insert-character [character]
  (fn [editor]
    (let [[x y] (:cursor editor)]
      (-> editor
          (update-in [:buffer y] #(str (subs % 0 x) character (subs % x)))
          (move-cursor-right)))))

(defn set-size [size]
  #(assoc % :size size))
