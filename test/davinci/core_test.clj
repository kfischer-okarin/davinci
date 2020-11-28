(ns davinci.core-test
  (:require [clojure.test :refer :all]
            [davinci.core :refer :all])
  (:import com.googlecode.lanterna.input.Key))

(defn term-with-next-input [key]
  (proxy [com.googlecode.lanterna.terminal.Terminal] []
    (readInput [] key)))

(deftest test-get-key-raw
  (let [key (Key. \a)]
    (is (= (get-key-raw (term-with-next-input key)) key))))

(deftest test-parse-key-without-modifier
  (let [key (Key. \a)]
    (is (= (parse-key key) #{\a}))))

(deftest test-parse-key-with-modifier
  (let [key (Key. \c true false)]
    (is (= (parse-key key) #{\c :ctrl}))))

(deftest test-insert-character
  (let [editor {:buffer ["This is some text" "Second line is nice"] :cursor [4 1]}
        expected-next-editor {:buffer ["This is some text" "Secotnd line is nice"] :cursor [5 1]}]
    (is (= (insert-character \t editor) expected-next-editor))))

(deftest test-delete-previous-character
  (let [editor {:buffer ["This is some text" "Second line is nice"] :cursor [11 0]}
        expected-next-editor {:buffer ["This is soe text" "Second line is nice"] :cursor [10 0]}]
    (is (= (delete-previous-character editor) expected-next-editor))))
