(ns davinci.commands-test
  (:require [clojure.test :refer :all]
            [davinci.commands :refer :all]
            [davinci.queries :refer :all]
            [davinci.editor :as editor]))

(deftest execute-command-without-args-test
  (let [editor (-> editor/initial-state (set-buffer-lines ["Line 1"]))
        command-f (fn [editor] (set-buffer-lines editor ["Different" "Lines"]))
        updated-editor (-> editor (execute-command command-f))]
    (is (= ["Different" "Lines"] (get-buffer-lines updated-editor)))))

(deftest execute-command-with-args-test
  (let [editor (-> editor/initial-state (set-buffer-lines ["Line 1"]))
        command-f (fn [editor new-lines] (set-buffer-lines editor new-lines))
        updated-editor (-> editor (execute-command (list command-f ["Different" "Lines"])))]
    (is (= ["Different" "Lines"] (get-buffer-lines updated-editor)))))

(deftest bind-input-test
  (let [command-f (fn [editor] editor)
        editor (-> editor/initial-state (bind-input #{\a} command-f))]
    (is (= command-f (get-bound-command editor #{\a})))))

(deftest bind-input-with-arguments-test
  (let [command-f (fn [editor arg] editor)
        editor (-> editor/initial-state (bind-input #{\a} (command-f 22)))]
    (is (= (list command-f 22) (get-bound-command editor #{\a})))))
