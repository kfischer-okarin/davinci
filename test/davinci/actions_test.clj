(ns davinci.actions-test
  (:require [clojure.test :refer :all]
            [davinci.actions :refer :all]
            [davinci.editor :as editor]))

(defn editor-with [values] (merge editor/initial-state values))

(deftest test-replace-lines
  (let [editor (editor-with {:buffer ["Line 0" "Line 1" "Line 2" "Line 3"]})
        expected-next-editor (merge editor {:buffer ["Line 0" "New Line 0" "New Line 1" "New Line 2" "Line 3"]})]
    (is (= ((replace-lines [1 3] ["New Line 0" "New Line 1" "New Line 2"]) editor) expected-next-editor))))

(deftest test-delete-previous-character
  (let [editor (editor-with {:buffer ["This is some text" "Second line is nice"] :cursor [11 0]})
        expected-next-editor (merge editor {:buffer ["This is soe text" "Second line is nice"] :cursor [10 0]})]
    (is (= (delete-previous-character editor) expected-next-editor))))

(deftest test-delete-previous-character-and-merge-line
  (let [editor (editor-with {:buffer ["Abc" "def"] :cursor [0 1]})
        expected-next-editor (merge editor {:buffer ["Abcdef"] :cursor [3 0]})]
    (is (= (delete-previous-character editor) expected-next-editor))))

(deftest test-delete-previous-character-not-beyond-document
  (let [editor (editor-with {:buffer ["Abc" "def"] :cursor [0 0]})]
    (is (= (delete-previous-character editor) editor))))

(deftest test-move-cursor-up
  (let [editor (editor-with {:buffer ["Abc" "Def"] :cursor [2 1]})
        expected-next-editor (merge editor {:cursor [2 0]})]
    (is (= (move-cursor-up editor) expected-next-editor))))

(deftest test-move-cursor-up-scrolls-editor
  (let [editor (editor-with {:buffer ["Line 1" "Line 2" "Line 3"] :cursor [0 1] :size [80 2] :offset [0 1]})
        expected-next-editor (merge editor {:cursor [0 0] :offset [0 0]})]
    (is (= (move-cursor-up editor) expected-next-editor))))

(deftest test-move-cursor-up-not-beyond-document
  (let [editor (editor-with {:buffer ["This is some text" "Second line is nice"] :cursor [5 0]})]
    (is (= (move-cursor-up editor) editor))))

(deftest test-move-cursor-up-not-beyond-line
  (let [editor (editor-with {:buffer ["Abc" "Loooooong line"] :cursor [10 1]})
        expected-next-editor (merge editor {:cursor [3 0]})]
    (is (= (move-cursor-up editor) expected-next-editor))))

(deftest test-move-cursor-down
  (let [editor (editor-with {:buffer ["Abc" "Def"] :cursor [2 0]})
        expected-next-editor (merge editor {:cursor [2 1]})]
    (is (= (move-cursor-down editor) expected-next-editor))))

(deftest test-move-cursor-down-scrolls-editor
  (let [editor (editor-with {:buffer ["Line 1" "Line 2" "Line 3"] :cursor [0 1] :size [80 2] :offset [0 0]})
        expected-next-editor (merge editor {:cursor [0 2] :offset [0 1]})]
    (is (= (move-cursor-down editor) expected-next-editor))))

(deftest test-move-cursor-down-not-beyond-document
  (let [editor (editor-with {:buffer ["This is some text" "Second line is nice"] :cursor [5 1]})]
    (is (= (move-cursor-down editor) editor))))

(deftest test-move-cursor-down-not-beyond-line
  (let [editor (editor-with {:buffer ["This is some text" "Short"] :cursor [10 0]})
        expected-next-editor (merge editor {:cursor [5 1]})]
    (is (= (move-cursor-down editor) expected-next-editor))))

(deftest test-move-cursor-right
  (let [editor (editor-with {:buffer ["Abc"] :cursor [1 0]})
        expected-next-editor (merge editor {:cursor [2 0]})]
    (is (= (move-cursor-right editor) expected-next-editor))))

(deftest test-move-cursor-right-not-beyond-document
  (let [editor (editor-with {:buffer  ["Abc"] :cursor [3 0]})]
    (is (= (move-cursor-right editor) editor))))

(deftest test-move-cursor-right-to-next-line
  (let [editor (editor-with {:buffer ["Abc" "Def"] :cursor [3 0]})
        expected-next-editor (merge editor {:cursor [0 1]})]
    (is (= (move-cursor-right editor) expected-next-editor))))

(deftest test-move-cursor-left
  (let [editor (editor-with {:buffer ["Abc"] :cursor [2 0]})
        expected-next-editor (merge editor {:cursor [1 0]})]
    (is (= (move-cursor-left editor) expected-next-editor))))

(deftest test-move-cursor-left-not-beyond-document
  (let [editor (editor-with {:buffer ["Abc"] :cursor [0 0]})]
    (is (= (move-cursor-left editor) editor))))

(deftest test-move-cursor-left-to-previous-line
  (let [editor (editor-with {:buffer  ["Abc" "Def"] :cursor [0 1]})
        expected-next-editor (merge editor {:cursor [3 0]})]
    (is (= (move-cursor-left editor) expected-next-editor))))

(deftest test-insert-character
  (let [editor (editor-with {:buffer ["Abc" "Def"] :cursor [1 1]})
        expected-next-editor (merge editor {:buffer ["Abc" "Dtef"] :cursor [2 1]})]
    (is (= ((insert-character \t) editor) expected-next-editor))))

(deftest test-insert-newline
  (let [editor (editor-with {:buffer ["Abc" "Def"] :cursor [2 1]})
        expected-next-editor (merge editor {:buffer ["Abc" "De" "f"] :cursor [0 2]})]
    (is (= (insert-newline editor) expected-next-editor))))

(deftest test-page-down
  (let [editor (editor-with {:buffer ["Line 1" "Line 2" "Line 3" "Line 4"] :cursor [0 0] :size [80 2] :offset [0 0]})
        expected-next-editor (merge editor {:cursor [0 2] :offset [0 2]})]
    (is (= (page-down editor) expected-next-editor))))

(deftest test-page-down-not-beyond-document
  (let [editor (editor-with {:buffer ["Line 1" "Line 2" "Line 3" "Line 4"] :cursor [0 2] :size [80 2] :offset [0 1]})
        expected-next-editor (merge editor {:cursor [0 3] :offset [0 2]})]
    (is (= (page-down editor) expected-next-editor))))

(deftest test-page-down-not-beyond-line
  (let [editor (editor-with {:buffer ["Long Line 1" "Line 2" "Line 3" "Line 4"] :cursor [10 0] :size [80 5] :offset [0 0]})
        expected-next-editor (merge editor {:cursor [6 3]})]
    (is (= (page-down editor) expected-next-editor))))

(deftest test-page-up
  (let [editor (editor-with {:buffer ["Line 1" "Line 2" "Line 3" "Line 4"] :cursor [0 3] :size [80 2] :offset [0 2]})
        expected-next-editor (merge editor {:cursor [0 1] :offset [0 0]})]
    (is (= (page-up editor) expected-next-editor))))

(deftest test-page-up-not-beyond-document
  (let [editor (editor-with {:buffer ["Line 1" "Line 2" "Line 3" "Line 4"] :cursor [0 2] :size [80 2] :offset [0 1]})
        expected-next-editor (merge editor {:cursor [0 0] :offset [0 0]})]
    (is (= (page-up editor) expected-next-editor))))

(deftest test-page-up-not-beyond-line
  (let [editor (editor-with {:buffer ["Line 1" "Line 2" "Line 3" "Long Line 4"] :cursor [10 3] :size [80 5] :offset [0 0]})
        expected-next-editor (merge editor {:cursor [6 0]})]
    (is (= (page-up editor) expected-next-editor))))

(deftest test-move-cursor-to-beginning-of-line
  (let [editor (editor-with {:buffer ["Abc"] :cursor [2 0]})
        expected-next-editor (merge editor {:cursor [0 0]})]
    (is (= (move-cursor-to-beginning-of-line editor) expected-next-editor))))

(deftest test-move-cursor-to-end-of-line
  (let [editor (editor-with {:buffer ["Abc"] :cursor [1 0]})
        expected-next-editor (merge editor {:cursor [3 0]})]
    (is (= (move-cursor-to-end-of-line editor) expected-next-editor))))

(deftest test-set-size
  (let [editor (editor-with {:buffer ["Line 1" "Line 2" "Line 3" "Line 4"] :size [80 2]})
        expected-next-editor (merge editor {:size [80 4]})]
    (is (= ((set-size [80 4]) editor) expected-next-editor))))

(deftest test-set-size-beyond-document-end
  (let [editor (editor-with {:buffer ["Line 1" "Line 2" "Line 3" "Line 4"] :cursor [0 2] :size [80 2] :offset [0 2]})
        expected-next-editor (merge editor {:size [80 3] :offset [0 1]})]
    (is (= ((set-size [80 3]) editor) expected-next-editor))))

(deftest test-set-size-bigger-than-document
  (let [editor (editor-with {:buffer ["Line 1" "Line 2" "Line 3" "Line 4"] :cursor [0 2] :size [80 2] :offset [0 2]})
        expected-next-editor (merge editor {:size [80 5] :offset [0 0]})]
    (is (= ((set-size [80 5]) editor) expected-next-editor))))
