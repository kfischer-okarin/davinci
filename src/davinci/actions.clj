(ns davinci.actions
  (:require [davinci.queries :refer :all]))

(defn quit-editor [editor]
  (assoc editor :running false))

(defn- fix-cursor-position [editor [x y]]
  (let [max-y (dec (count (:buffer editor)))
        fixed-y (min (max 0 y) max-y)
        current-line (get-in editor [:buffer fixed-y])
        max-x (count current-line)
        fixed-x (min (max 0 x) max-x)]
    [fixed-x fixed-y]))

(defn- move-cursor [dx dy editor]
  (update editor :cursor
          (comp
           (partial fix-cursor-position editor)
           (fn [[x y]]  [(+ x dx) (+ y dy)]))))

(defn move-cursor-left [editor]
  (assoc editor :cursor (position-left-of-cursor editor)))

(defn move-cursor-right [editor]
  (assoc editor :cursor (position-right-of-cursor editor)))

(defn move-cursor-up [editor]
  (assoc editor :cursor (position-up-of-cursor editor)))

(defn move-cursor-down [editor]
  (assoc editor :cursor (position-down-of-cursor editor)))

(defn replace-lines [[start end] new-lines]
  (fn [editor]
    (update editor :buffer #(into [] cat [(take start %) new-lines (drop end %)]))))

(defn delete-previous-character [editor]
  (let [[x y] (:cursor editor) prev-x (dec x)]
    (-> editor
        (update-in [:buffer y] #(str (subs % 0 prev-x) (subs % x)))
        move-cursor-left)))

(defn insert-newline [editor]
  (let [[x y] (:cursor editor)
        current (current-line editor)
        before-cursor (subs current 0 x)
        after-cursor (subs current x)
        split-line-at-cursor (replace-lines [y (inc y)] [before-cursor after-cursor])]
    (-> editor
        split-line-at-cursor
        move-cursor-right)))

(def do-nothing identity)

(defn open-file [filename]
  #(assoc % :buffer (clojure.string/split (slurp filename) #"\n")))

(defn insert-character [character]
  (fn [editor]
    (let [[x y] (:cursor editor)]
      (-> editor
          (update-in [:buffer y] #(str (subs % 0 x) character (subs % x)))
          (move-cursor-right)))))
