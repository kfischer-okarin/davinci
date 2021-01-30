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
