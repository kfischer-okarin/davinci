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

(deftest test-move-up-not-beyond-document
  (let [buffer ["This is some text" "Second line is nice"]
        editor {:buffer buffer :cursor [5 0]}
        expected-next-editor {:buffer buffer :cursor [5 0]}]
    (is (= (move-cursor 0 -1 editor) expected-next-editor))))

(deftest test-move-down-not-beyond-document
  (let [buffer ["This is some text" "Second line is nice"]
        editor {:buffer buffer :cursor [5 1]}
        expected-next-editor {:buffer buffer :cursor [5 1]}]
    (is (= (move-cursor 0 1 editor) expected-next-editor))))

(deftest test-move-right-not-beyond-line
  (let [buffer ["This is some text" "Second line is nice"]
        editor {:buffer buffer :cursor [17 0]}
        expected-next-editor {:buffer buffer :cursor [17 0]}]
    (is (= (move-cursor 1 0 editor) expected-next-editor))))

(deftest test-move-left-not-beyond-line
  (let [buffer ["This is some text" "Second line is nice"]
        editor {:buffer buffer :cursor [0 0]}
        expected-next-editor {:buffer buffer :cursor [0 0]}]
    (is (= (move-cursor -1 0 editor) expected-next-editor))))

(deftest test-move-down-not-beyond-line
  (let [buffer ["This is some text" "Short"]
        editor {:buffer buffer :cursor [10 0]}
        expected-next-editor {:buffer buffer :cursor [5 1]}]
    (is (= (move-cursor 0 1 editor) expected-next-editor))))
