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
