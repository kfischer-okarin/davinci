(ns davinci.lines-test
  (:require [clojure.test :refer :all]
            [clojure.spec.test.alpha :as stest]
            [davinci.lines :refer :all]))

(deftest spec-tests
  (let [options {:clojure.spec.test.check/opts {:num-tests 100}}
        test-results (map stest/abbrev-result (stest/check (stest/checkable-syms) options))]
    (is (not-any? :failure test-results))))

(deftest ->string-test
  (are [string lines] (= string (->string lines))
    "Line 1\nLine 2\n" ["Line 1" "Line 2" ""]
    "Line 1\nLine 2" ["Line 1" "Line 2"]))

(deftest ->lines-test
  (are [lines string] (= lines (->lines string))
    ["Line 1" "Line 2" ""] "Line 1\nLine 2\n"
    ["Line 1" "Line 2"] "Line 1\nLine 2"
    [""] ""))

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

(deftest clamp-position-test
  (let [lines ["Line 1" "Line 2"]]
    (are [clamped-position position] (= clamped-position (clamp-position lines position))
      [6 0] [8 0]
      [0 0] [-1 0]
      [3 0] [3 -1]
      [3 1] [3 4]
      [6 0] [10 -4]
      [0 1] [-20 10])))

(deftest replace-lines-test
  (let [lines ["Line 1" "Line 2" "Line 3"]]
    (are [new-lines start end replacement] (= new-lines (replace-lines lines start end replacement))
      ["Line 1" "New Line 2-1" "New Line 2-2" "Line 3"] 1 2 ["New Line 2-1" "New Line 2-2"]
      ["New Line 1-1" "New Line 1-2" "Line 3"] 0 2 ["New Line 1-1" "New Line 1-2"]
      ["New Line"] 0 3 ["New Line"])))

(deftest replace-line-test
  (let [lines ["Line 1" "Line 2" "Line 3"]]
    (is (= ["Line 1" "New Line 2" "Line 3"] (replace-line lines 1 "New Line 2")))))

(deftest update-line-test
  (let [lines ["Line 1" "Line 2" "Line 3"]]
    (is (= ["Line 1" "Line 2Line 2" "Line 3"] (update-line lines 1 #(str % %))))))

(deftest insert-newline-test
  (let [lines ["Line 1"]]
    (are [new-lines position] (= new-lines (insert-newline lines position))
      ["Lin" "e 1"] [3 0]
      ["Line 1" ""] [6 0])))

(deftest delete-character-test
  (let [lines ["Line 1" "Line 2"]]
    (are [new-lines position] (= new-lines (delete-character lines position))
      ["Lin 1" "Line 2"] [3 0]
      ["Line 1Line 2"] [6 0])))

(deftest insert-string-test
  (let [lines ["Line 1" "Line 2"]]
    (is (= ["Line 1" "LiSTRINGne 2"] (insert-string lines [2 1] "STRING")))))

(deftest insert-character-test
  (let [lines ["Line 1" "Line 2"]]
    (is (= ["Line 1" "LiAne 2"] (insert-character lines [2 1] \A)))))
