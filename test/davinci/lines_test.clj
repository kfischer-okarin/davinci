(ns davinci.lines-test
  (:require [clojure.test :refer :all]
            [davinci.lines :refer :all]))

(deftest ->string-with-final-newline-test
  (is (= "Line 1\nLine 2\n" (->string ["Line 1" "Line 2" ""]))))

(deftest ->string-without-final-newline-test
  (is (= "Line 1\nLine 2" (->string ["Line 1" "Line 2"]))))

(deftest ->lines-with-final-newline-test
  (is (=  ["Line 1" "Line 2" ""] (->lines "Line 1\nLine 2\n"))))

(deftest ->lines-without-final-newline-test
  (is (=  ["Line 1" "Line 2"] (->lines "Line 1\nLine 2"))))

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
  (let [lines ["Line 1" "Long Line 2" "Line 3"]]
    (is (= [5 1] (position-right-of lines [4 1])))))

(deftest position-right-of-across-lines-test
  (let [lines ["Line 1" "Long Line 2" "Line 3"]]
    (is (= [0 1] (position-right-of lines [6 0])))))

(deftest position-right-at-end-of-document-test
  (let [lines ["Line 1" "Long Line 2" "Line 3"]]
    (is (= nil (position-right-of lines [6 2])))))

(deftest position-left-of-test
  (let [lines ["Line 1" "Long Line 2"]]
    (is (= [3 1] (position-left-of lines [4 1])))))

(deftest position-left-of-across-lines-test
  (let [lines ["Line 1" "Long Line 2"]]
    (is (= [6 0] (position-left-of lines [0 1])))))

(deftest position-left-at-beginning-of-document-test
  (let [lines ["Line 1"]]
    (is (= nil (position-left-of lines [0 0])))))

(deftest position-up-of-test
  (let [lines ["Line 1" "Long Line 2"]]
    (is (= [4 0] (position-up-of lines [4 1])))))

(deftest position-up-of-with-shorter-linetest
  (let [lines ["Line 1" "Long Line 2"]]
    (is (= [6 0] (position-up-of lines [8 1])))))

(deftest position-up-at-beginning-of-document-linetest
  (let [lines ["Line 1" "Long Line 2"]]
    (is (= nil (position-up-of lines [3 0])))))

(deftest position-down-of-test
  (let [lines ["Line 1" "Long Line 2"]]
    (is (= [4 1] (position-down-of lines [4 0])))))

(deftest position-down-of-with-shorter-linetest
  (let [lines ["Long Line 1" "Line 2"]]
    (is (= [6 1] (position-down-of lines [8 0])))))

(deftest position-down-at-end-of-document-linetest
  (let [lines ["Line 1" "Long Line 2"]]
    (is (= nil (position-down-of lines [4 1])))))
