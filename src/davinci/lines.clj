(ns davinci.lines
  (:require [clojure.string :as string]))

(defn ->string [lines]
  (string/join "\n" lines))

(defn ->lines [string]
  (let [lines (string/split string #"\n")]
    (if (string/ends-with? string "\n")
      (conj lines "")
      lines)))

(defn get-line [lines line-no]
  (nth lines line-no))

(defn get-length [lines line-no]
  (count (get-line lines line-no)))

(defn max-y [lines]
  (dec (count lines)))

(def get-lines subvec)

(defn position-right-of [lines [x y]]
  (let [line-length (get-length lines y)]
    (cond
      (< x line-length) [(inc x) y]
      (< y (max-y lines)) [0 (inc y)])))

(defn position-left-of [lines [x y]]
  (cond
    (pos? x) [(dec x) y]
    (pos? y) (let [previous-y (dec y)
                   previous-line-length (get-length lines previous-y)]
               [previous-line-length previous-y])))

(defn position-up-of [lines [x y]]
  (if (pos? y)
    (let [previous-y (dec y)
          previous-line-length (get-length lines previous-y)]
      [(min x previous-line-length) previous-y])))

(defn position-down-of [lines [x y]]
  (if (< y (max-y lines))
    (let [next-y (inc y)
          next-line-length (get-length lines next-y)]
      [(min x next-line-length) (inc y)])))

(defn- clamp [x lower-bound upper-bound]
  (max (min x upper-bound) lower-bound))

(defn clamp-position [lines [x y]]
  (let [clamped-y (clamp y 0 (max-y lines))
        clamped-x (clamp x 0 (get-length lines clamped-y))]
    [clamped-x clamped-y]))

(defn replace-lines [lines start end replacement]
  (let [until-start (get-lines lines 0 start)
        after-end (get-lines lines end)]
    (into until-start cat [replacement after-end])))

(defn replace-line [lines line-no replacement]
  (replace-lines lines line-no (inc line-no) [replacement]))

(defn update-line [lines line-no f]
  (replace-line lines line-no (f (get-line lines line-no))))

(defn insert-string [lines [x y] s]
  (let [line (get-line lines y)
        before-position (subs line 0 x)
        after-position (subs line x)]
    (replace-line lines y (str before-position s after-position))))

(defn insert-character [lines position c]
  (insert-string lines position (str c)))

(defn insert-newline [lines [x y]]
  (let [line (get-line lines y)
        before-position (subs line 0 x)
        after-position (subs line x)]
    (replace-lines lines y (inc y) [before-position after-position])))

(defn delete-character [lines [x y]]
  (let [line-length (get-length lines y)]
    (if (< x line-length)
      (update-line lines y #(str (subs % 0 x) (subs % (inc x))))
      (let [merged-line (str (get-line lines y) (get-line lines (inc y)))]
        (replace-lines lines y (+ y 2) [merged-line])))))
