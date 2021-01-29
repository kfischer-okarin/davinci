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
