(ns davinci.core-test
  (:import
   com.googlecode.lanterna.terminal.Terminal)
  (:require [clojure.test :refer :all]
            [davinci.core :refer :all]
            [davinci.editor :as e]
            [davinci.system :as s]
            [davinci.terminal :as t]))

(defmethod print-method
  java.lang.Object [object writer]
  (print-simple (str "<#" (.getSimpleName (.getClass object)) ">") writer))

(defn record-function-call [name output-atom]
  (fn [& args] (swap! output-atom #(conj % (into [name] args)))))

(defn return-in-order [input-atom]
  (fn [& _] (let [return-value (peek @input-atom)]
              (swap! input-atom pop)
              return-value)))

(def do-nothing (constantly nil))

(deftest test-integration
  (let [operations (atom nil) keys (atom nil) term (proxy [Terminal] [])]
    (with-redefs [t/put-string (record-function-call :put-string operations)
                  s/get-tempfile do-nothing
                  slurp (constantly "My Text\nLine 2\nLine 3\nLast Line\n")
                  t/get-terminal (constantly term)
                  t/add-resize-listener do-nothing
                  t/get-size (constantly [80 4])
                  t/start do-nothing
                  t/stop (record-function-call :stop operations)
                  t/flush-terminal do-nothing
                  t/clear do-nothing
                  t/move-cursor (record-function-call :move-cursor operations)
                  t/get-key (return-in-order keys)]
      (reset! operations [])
      (reset! keys [{:key \w :modifiers #{:ctrl}} {:key :f15 :modifiers #{}} {:key \a :modifiers #{}}])
      (main "test.txt")
      (reset! last-key nil)
      (is (= @operations [; Initial render
                          [:put-string term "My Text\n"]
                          [:put-string term "Line 2\n"]
                          [:put-string term "Line 3\n"]
                          [:move-cursor term 0 3]
                          [:put-string term "test.txt:1:1                            " :white :red]
                          [:put-string term "                              Last key: " :white :red]
                          [:move-cursor term 0 0]
                          ; After pressing a
                          [:put-string term "aMy Text\n"]
                          [:put-string term "Line 2\n"]
                          [:put-string term "Line 3\n"]
                          [:move-cursor term 0 3]
                          [:put-string term "test.txt:1:2                            " :white :red]
                          [:put-string term "     Last key: {:key \\a, :modifiers #{}}" :white :red]
                          [:move-cursor term 1 0]
                          ; After pressing F15 (no function)
                          [:put-string term "aMy Text\n"]
                          [:put-string term "Line 2\n"]
                          [:put-string term "Line 3\n"]
                          [:move-cursor term 0 3]
                          [:put-string term "test.txt:1:2                            " :white :red]
                          [:put-string term "   Last key: {:key :f15, :modifiers #{}}" :white :red]
                          [:move-cursor term 1 0]
                          [:stop term]])))))
