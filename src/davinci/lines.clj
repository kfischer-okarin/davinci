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
