(ns davinci.core-test
  (:require [clojure.test :refer :all]
            [davinci.core :refer :all]
            [davinci.terminal :as t]))

; (deftest test-render-lines-in-rect
;   (let [operations (atom [])
;         record-operation (fn [name args] (swap! operations #(conj % (into [name] args))))]
;     (with-redefs [t/put-string (fn [_ & args] (record-operation :put-string args))]
;       (render-lines-in-rect :term)
;       (is (= @operations [[:put-string]])))))
