(ns davinci.actions-test
  (:require [clojure.test :refer :all]
            [davinci.actions :refer :all]))

(deftest test-delete-previous-character
  (let [editor {:buffer ["This is some text" "Second line is nice"] :cursor [11 0]}
        expected-next-editor {:buffer ["This is soe text" "Second line is nice"] :cursor [10 0]}]
    (is (= (delete-previous-character editor) expected-next-editor))))

(deftest test-move-cursor-up
  (let [buffer ["Abc" "Def"]
        editor {:buffer buffer :cursor [2 1]}
        expected-next-editor {:buffer buffer :cursor [2 0]}]
    (is (= (move-cursor-up editor) expected-next-editor))))

(deftest test-move-cursor-up-not-beyond-document
  (let [buffer ["This is some text" "Second line is nice"]
        editor {:buffer buffer :cursor [5 0]}
        expected-next-editor {:buffer buffer :cursor [5 0]}]
    (is (= (move-cursor-up editor) expected-next-editor))))

(deftest test-move-cursor-up-not-beyond-line
  (let [buffer ["Abc" "Loooooong line"]
        editor {:buffer buffer :cursor [10 1]}
        expected-next-editor {:buffer buffer :cursor [3 0]}]
    (is (= (move-cursor-up editor) expected-next-editor))))

(deftest test-move-cursor-down
  (let [buffer ["Abc" "Def"]
        editor {:buffer buffer :cursor [2 0]}
        expected-next-editor {:buffer buffer :cursor [2 1]}]
    (is (= (move-cursor-down editor) expected-next-editor))))

(deftest test-move-cursor-down-not-beyond-document
  (let [buffer ["This is some text" "Second line is nice"]
        editor {:buffer buffer :cursor [5 1]}
        expected-next-editor {:buffer buffer :cursor [5 1]}]
    (is (= (move-cursor-down editor) expected-next-editor))))

(deftest test-move-cursor-down-not-beyond-line
  (let [buffer ["This is some text" "Short"]
        editor {:buffer buffer :cursor [10 0]}
        expected-next-editor {:buffer buffer :cursor [5 1]}]
    (is (= (move-cursor-down editor) expected-next-editor))))

(deftest test-move-cursor-right
  (let [buffer ["Abc"]
        editor {:buffer buffer :cursor [1 0]}
        expected-next-editor {:buffer buffer :cursor [2 0]}]
    (is (= (move-cursor-right editor) expected-next-editor))))

(deftest test-move-cursor-right-not-beyond-document
  (let [buffer ["Abc"]
        editor {:buffer buffer :cursor [3 0]}
        expected-next-editor {:buffer buffer :cursor [3 0]}]
    (is (= (move-cursor-right editor) expected-next-editor))))

(deftest test-move-cursor-right-to-next-line
  (let [buffer ["Abc" "Def"]
        editor {:buffer buffer :cursor [3 0]}
        expected-next-editor {:buffer buffer :cursor [0 1]}]
    (is (= (move-cursor-right editor) expected-next-editor))))

(deftest test-move-cursor-left
  (let [buffer ["Abc"]
        editor {:buffer buffer :cursor [2 0]}
        expected-next-editor {:buffer buffer :cursor [1 0]}]
    (is (= (move-cursor-left editor) expected-next-editor))))

(deftest test-move-cursor-left-not-beyond-document
  (let [buffer ["Abc"]
        editor {:buffer buffer :cursor [0 0]}
        expected-next-editor {:buffer buffer :cursor [0 0]}]
    (is (= (move-cursor-left editor) expected-next-editor))))

(deftest test-move-cursor-left-to-previous-line
  (let [buffer ["Abc" "Def"]
        editor {:buffer buffer :cursor [0 1]}
        expected-next-editor {:buffer buffer :cursor [3 0]}]
    (is (= (move-cursor-left editor) expected-next-editor))))

(deftest test-insert-character
  (let [editor {:buffer ["This is some text" "Second line is nice"] :cursor [4 1]}
        expected-next-editor {:buffer ["This is some text" "Secotnd line is nice"] :cursor [5 1]}]
    (is (= ((insert-character \t) editor) expected-next-editor))))

(deftest test-insert-newline
  (let [editor {:buffer ["This is some text" "Second line is nice"] :cursor [4 1]}
        expected-next-editor {:buffer ["This is some text" "Seco" "nd line is nice"] :cursor [0 2]}]
    (is (= (insert-newline editor) expected-next-editor))))
