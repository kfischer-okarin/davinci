(ns davinci.lines-test
  (:require [clojure.test :refer :all]
            [davinci.lines :refer :all]))

(deftest ->string-test
  (are [string lines] (= string (->string lines))
    "Line 1\nLine 2\n" ["Line 1" "Line 2" ""]
    "Line 1\nLine 2" ["Line 1" "Line 2"]))

(deftest ->lines-test
  (are [lines string] (= lines (->lines string))
    ["Line 1" "Line 2" ""] "Line 1\nLine 2\n"
    ["Line 1" "Line 2"] "Line 1\nLine 2"))

(deftest get-line-test
  (is (= "Line 3" (get-line ["Line 1" "Loong Line 2" "Line 3"] 2))))

(deftest get-length-test
  (is (= 12 (get-length ["Line 1" "Loong Line 2" "Line 3"] 1))))

(deftest max-y-test
  (is (= 2 (max-y ["Line 1" "Loong Line 2" "Line 3"]))))

(deftest get-lines-test
  (is (= ["Line 2" "Line 3"] (get-lines ["Line 1" "Line 2" "Line 3" "Line 4"] 1 3))))

(deftest get-lines-just-start-index-test
  (is (= ["Line 2" "Line 3" "Line 4"] (get-lines ["Line 1" "Line 2" "Line 3" "Line 4"] 1))))

(deftest position-right-of-test
  (let [lines ["Line 1" "Line 2" "Line 3"]]
    (are [new-position position] (= new-position (position-right-of lines position))
      [5 1] [4 1]
      [0 1] [6 0]
      nil [6 2])))

(deftest position-left-of-test
  (let [lines ["Line 1" "Line 2" "Line 3"]]
    (are [new-position position] (= new-position (position-left-of lines position))
      [3 1] [4 1]
      [6 0] [0 1]
      nil [0 0])))

(deftest position-up-of-test
  (let [lines ["Line 1" "Long Line 2"]]
    (are [new-position position] (= new-position (position-up-of lines position))
      [4 0] [4 1]
      [6 0] [8 1]
      nil [3 0])))

(deftest position-down-of-test
  (let [lines ["Long Line 1" "Line 2"]]
    (are [new-position position] (= new-position (position-down-of lines position))
      [4 1] [4 0]
      [6 1] [8 0]
      nil [3 1])))
