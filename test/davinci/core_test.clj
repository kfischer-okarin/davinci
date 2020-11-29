(ns davinci.core-test
  (:require [clojure.test :refer :all]
            [davinci.core :refer :all]))

(deftest test-insert-character
  (let [editor {:buffer ["This is some text" "Second line is nice"] :cursor [4 1]}
        expected-next-editor {:buffer ["This is some text" "Secotnd line is nice"] :cursor [5 1]}]
    (is (= (insert-character \t editor) expected-next-editor))))

(deftest test-delete-previous-character
  (let [editor {:buffer ["This is some text" "Second line is nice"] :cursor [11 0]}
        expected-next-editor {:buffer ["This is soe text" "Second line is nice"] :cursor [10 0]}]
    (is (= (delete-previous-character editor) expected-next-editor))))
