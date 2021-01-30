(ns davinci.actions-test
  (:require [clojure.test :refer :all]
            [davinci.actions :refer :all]
            [davinci.queries :refer :all]
            [davinci.editor :as editor]))

(defn editor-with [values] (merge editor/initial-state values))

(deftest test-quit-editor
  (let [editor editor/initial-state]
    (do
      (is (is-running editor))
      (is (not (is-running (quit-editor editor)))))))

(deftest test-open-file
  (with-redefs [slurp (constantly "Line 1\nLine 2\n")]
    (let [editor editor/initial-state
          next-editor (open-file "abc" editor)]
      (is (= ["Line 1" "Line 2" ""] (get-buffer-lines next-editor)))
      (is (= "abc" (get-buffer-path next-editor))))))

(deftest test-open-file-without-final-newline
  (with-redefs [slurp (constantly "Line 1\nLine 2")]
    (let [editor editor/initial-state
          next-editor (open-file "abc" editor)]
      (is (= ["Line 1" "Line 2"] (get-buffer-lines next-editor)))
      (is (= "abc" (get-buffer-path next-editor))))))

(deftest open-file-with-file-type-parser-test
  (with-redefs [slurp (constantly "")]
    (let [editor (->> editor/initial-state (recognize-file-type :text #"\.txt\Z"))
          next-editor (open-file "abc.txt" editor)]
      (is (= :text (get-buffer-type next-editor))))))

(deftest last-file-type-parser-wins-test
  (with-redefs [slurp (constantly "")]
    (let [editor (->> editor/initial-state (recognize-file-type :text #"\.txt\Z") (recognize-file-type :nottext #"\.txt\Z"))
          next-editor (open-file "abc.txt" editor)]
      (is (= :nottext (get-buffer-type next-editor))))))

(deftest test-save-file
  (let [output (atom nil)]
    (with-redefs [spit (fn [filename content] (reset! output [filename content]))]
      (let [editor (->> editor/initial-state (set-buffer ["Line 1" "Line 2" ""] "test.txt"))]
        (is (= editor (save-file editor)))
        (is (= ["test.txt" "Line 1\nLine 2\n"] @output))))))

(deftest test-save-file-without-final-newline
  (let [output (atom nil)]
    (with-redefs [spit (fn [filename content] (reset! output [filename content]))]
      (let [editor (->> editor/initial-state (set-buffer ["Line 1" "Line 2"] "test.txt"))]
        (is (= editor (save-file editor)))
        (is (= ["test.txt" "Line 1\nLine 2"] @output))))))

(deftest test-save-file-to
  (let [output (atom nil)]
    (with-redefs [spit (fn [filename content] (reset! output [filename content]))]
      (let [editor (->> editor/initial-state (set-buffer-lines ["Line 1" "Line 2" ""]))]
        (is (= editor (save-file-to "new-file.txt" editor)))
        (is (= ["new-file.txt" "Line 1\nLine 2\n"] @output))))))

(deftest test-delete-previous-character
  (let [editor (->> editor/initial-state (set-buffer-lines ["This is some text" "Second line is nice"]) (set-cursor [11 0]))
        expected-next-editor (->> editor (set-buffer-lines ["This is soe text" "Second line is nice"]) (set-cursor [10 0]))]
    (is (= expected-next-editor (delete-previous-character editor)))))

(deftest test-delete-previous-character-and-merge-line
  (let [editor (->> editor/initial-state (set-buffer-lines ["Abc" "def"]) (set-cursor [0 1]))
        expected-next-editor (->> editor (set-buffer-lines ["Abcdef"]) (set-cursor [3 0]))]
    (is (= expected-next-editor (delete-previous-character editor)))))

(deftest test-delete-previous-character-not-beyond-document
  (let [editor (->> editor/initial-state (set-buffer-lines ["Abc" "def"]) (set-cursor [0 0]))]
    (is (= editor (delete-previous-character editor)))))

(deftest test-move-cursor-up
  (let [editor (->> editor/initial-state (set-buffer-lines ["Abc" "Def"]) (set-cursor [2 1]))
        expected-next-editor (->> editor (set-cursor [2 0]))]
    (is (= expected-next-editor (move-cursor-up editor)))))

(deftest test-move-cursor-up-scrolls-editor
  (let [editor (->> editor/initial-state
                    (set-buffer-lines ["Line 1" "Line 2" "Line 3"])
                    (set-cursor [0 1])
                    (set-size [80 2])
                    (set-offset [0 1]))
        expected-next-editor (->> editor
                                  (set-cursor [0 0])
                                  (set-offset [0 0]))]
    (is (= expected-next-editor (move-cursor-up editor)))))

(deftest test-move-cursor-up-not-beyond-document
  (let [editor (->> editor/initial-state (set-buffer-lines ["This is some text" "Second line is nice"]) (set-cursor [5 0]))]
    (is (= editor (move-cursor-up editor)))))

(deftest test-move-cursor-up-not-beyond-line
  (let [editor (->> editor/initial-state (set-buffer-lines ["Abc" "Loooooong line"]) (set-cursor [10 1]))
        expected-next-editor (->> editor (set-cursor [3 0]))]
    (is (= expected-next-editor (move-cursor-up editor)))))

(deftest test-move-cursor-down
  (let [editor (->> editor/initial-state (set-buffer-lines ["Abc" "Def"]) (set-cursor [2 0]))
        expected-next-editor (->> editor (set-cursor [2 1]))]
    (is (= expected-next-editor (move-cursor-down editor)))))

(deftest test-move-cursor-down-scrolls-editor
  (let [editor (->> editor/initial-state
                    (set-buffer-lines ["Line 1" "Line 2" "Line 3"])
                    (set-cursor [0 1])
                    (set-size [80 2])
                    (set-offset [0 0]))
        expected-next-editor (->> editor
                                  (set-cursor [0 2])
                                  (set-offset [0 1]))]
    (is (= expected-next-editor (move-cursor-down editor)))))

(deftest test-move-cursor-down-not-beyond-document
  (let [editor (->> editor/initial-state (set-buffer-lines ["This is some text" "Second line is nice"]) (set-cursor [5 1]))]
    (is (= editor (move-cursor-down editor)))))

(deftest test-move-cursor-down-not-beyond-line
  (let [editor (->> editor/initial-state (set-buffer-lines ["This is some text" "Short"]) (set-cursor [10 0]))
        expected-next-editor (->> editor (set-cursor [5 1]))]
    (is (= expected-next-editor (move-cursor-down editor)))))

(deftest test-move-cursor-right
  (let [editor (->> editor/initial-state (set-buffer-lines ["Abc"]) (set-cursor [1 0]))
        expected-next-editor (->> editor (set-cursor [2 0]))]
    (is (= expected-next-editor (move-cursor-right editor)))))

(deftest test-move-cursor-right-not-beyond-document
  (let [editor (->> editor/initial-state (set-buffer-lines  ["Abc"]) (set-cursor [3 0]))]
    (is (= editor (move-cursor-right editor)))))

(deftest test-move-cursor-right-to-next-line
  (let [editor (->> editor/initial-state (set-buffer-lines ["Abc" "Def"]) (set-cursor [3 0]))
        expected-next-editor (->> editor (set-cursor [0 1]))]
    (is (= expected-next-editor (move-cursor-right editor)))))

(deftest test-move-cursor-left
  (let [editor (->> editor/initial-state (set-buffer-lines ["Abc"]) (set-cursor [2 0]))
        expected-next-editor (->> editor (set-cursor [1 0]))]
    (is (= expected-next-editor (move-cursor-left editor)))))

(deftest test-move-cursor-left-not-beyond-document
  (let [editor (->> editor/initial-state (set-buffer-lines ["Abc"]) (set-cursor [0 0]))]
    (is (= editor (move-cursor-left editor)))))

(deftest test-move-cursor-left-to-previous-line
  (let [editor (->> editor/initial-state (set-buffer-lines  ["Abc" "Def"]) (set-cursor [0 1]))
        expected-next-editor (->> editor (set-cursor [3 0]))]
    (is (= expected-next-editor (move-cursor-left editor)))))

(deftest test-insert-character-at-cursor
  (let [editor (->> editor/initial-state (set-buffer-lines ["Abc" "Def"]) (set-cursor [1 1]))
        expected-next-editor (->> editor (set-buffer-lines ["Abc" "Dtef"]) (set-cursor [2 1]))]
    (is (= expected-next-editor (insert-character-at-cursor \t editor)))))

(deftest test-insert-newline-at-cursor
  (let [editor (->> editor/initial-state (set-buffer-lines ["Abc" "Def"]) (set-cursor [2 1]))
        expected-next-editor (->> editor (set-buffer-lines ["Abc" "De" "f"]) (set-cursor [0 2]))]
    (is (= expected-next-editor (insert-newline-at-cursor editor)))))

(deftest test-page-down
  (let [editor (->> editor/initial-state
                    (set-buffer-lines ["Line 1" "Line 2" "Line 3" "Line 4"])
                    (set-cursor [0 0])
                    (set-size [80 2])
                    (set-offset [0 0]))
        expected-next-editor (->> editor
                                  (set-cursor [0 2])
                                  (set-offset [0 2]))]
    (is (= expected-next-editor (page-down editor)))))

(deftest test-page-down-not-beyond-document
  (let [editor (->> editor/initial-state
                    (set-buffer-lines ["Line 1" "Line 2" "Line 3" "Line 4"])
                    (set-cursor [0 2])
                    (set-size [80 2])
                    (set-offset [0 1]))
        expected-next-editor (->> editor
                                  (set-cursor [0 3])
                                  (set-offset [0 2]))]
    (is (= expected-next-editor (page-down editor)))))

(deftest test-page-down-not-beyond-line
  (let [editor (->> editor/initial-state
                    (set-buffer-lines ["Long Line 1" "Line 2" "Line 3" "Line 4"])
                    (set-cursor [10 0])
                    (set-size [80 5])
                    (set-offset [0 0]))
        expected-next-editor (->> editor (set-cursor [6 3]))]
    (is (= expected-next-editor (page-down editor)))))

(deftest test-page-up
  (let [editor (->> editor/initial-state
                    (set-buffer-lines ["Line 1" "Line 2" "Line 3" "Line 4"])
                    (set-cursor [0 3])
                    (set-size [80 2])
                    (set-offset [0 2]))
        expected-next-editor (->> editor
                                  (set-cursor [0 1])
                                  (set-offset [0 0]))]
    (is (= expected-next-editor (page-up editor)))))

(deftest test-page-up-not-beyond-document
  (let [editor (->> editor/initial-state
                    (set-buffer-lines ["Line 1" "Line 2" "Line 3" "Line 4"])
                    (set-cursor [0 2])
                    (set-size [80 2])
                    (set-offset [0 1]))
        expected-next-editor (->> editor
                                  (set-cursor [0 0])
                                  (set-offset [0 0]))]
    (is (= expected-next-editor (page-up editor)))))

(deftest test-page-up-not-beyond-line
  (let [editor (->> editor/initial-state
                    (set-buffer-lines ["Line 1" "Line 2" "Line 3" "Long Line 4"])
                    (set-cursor [10 3])
                    (set-size [80 5])
                    (set-offset [0 0]))
        expected-next-editor (->> editor (set-cursor [6 0]))]
    (is (= expected-next-editor (page-up editor)))))

(deftest test-move-cursor-to-beginning-of-line
  (let [editor (->> editor/initial-state (set-buffer-lines ["Abc"]) (set-cursor [2 0]))
        expected-next-editor (->> editor (set-cursor [0 0]))]
    (is (= expected-next-editor (move-cursor-to-beginning-of-line editor)))))

(deftest test-move-cursor-to-end-of-line
  (let [editor (->> editor/initial-state (set-buffer-lines ["Abc"]) (set-cursor [1 0]))
        expected-next-editor (->> editor (set-cursor [3 0]))]
    (is (= expected-next-editor (move-cursor-to-end-of-line editor)))))

(deftest test-delete-line
  (let [editor (->> editor/initial-state (set-buffer-lines ["Line 1" "Line 2" "Line 3"]) (set-cursor [1 1]))
        expected-next-editor (->> editor (set-buffer-lines ["Line 1" "Line 3"]))]
    (is (= expected-next-editor (delete-line editor)))))

(deftest test-delete-line-only-line
  (let [editor (->> editor/initial-state (set-buffer-lines ["Line 1"]) (set-cursor [0 0]))
        expected-next-editor (->> editor (set-buffer-lines [""]))]
    (is (= expected-next-editor (delete-line editor)))))

(deftest test-delete-line-and-fix-x-position
  (let [editor (->> editor/initial-state (set-buffer-lines ["Line 1" "Loooooong Line 2" "Line 3"]) (set-cursor [10 1]))
        expected-next-editor (->> editor (set-buffer-lines ["Line 1" "Line 3"]) (set-cursor [6 1]))]
    (is (= expected-next-editor (delete-line editor)))))

(deftest test-delete-line-last-line
  (let [editor (->> editor/initial-state (set-buffer-lines ["Line 1" "Line 2" ""]) (set-cursor [0 2]))
        expected-next-editor (->> editor (set-buffer-lines ["Line 1" "Line 2"]) (set-cursor [0 1]))]
    (is (= expected-next-editor (delete-line editor)))))

(deftest test-delete-line-last-visible-line
  (let [editor (->> editor/initial-state (set-buffer-lines ["Line 1" "Line 2" "Line 3"]) (set-cursor [0 2]) (set-offset [0 2]))
        expected-next-editor (->> editor (set-buffer-lines ["Line 1" "Line 2"]) (set-cursor [0 1]) (set-offset [0 1]))]
    (is (= expected-next-editor (delete-line editor)))))

(deftest test-delete-until-end-of-line
  (let [editor (->> editor/initial-state (set-buffer-lines ["Line 1" "Line 2"]) (set-cursor [3 1]))
        expected-next-editor (->> editor (set-buffer-lines ["Line 1" "Lin"]))]
    (is (= expected-next-editor (delete-until-end-of-line editor)))))

(deftest test-delete-from-beginning-of-line
  (let [editor (->> editor/initial-state (set-buffer-lines ["Line 1" "Line 2"]) (set-cursor [3 1]))
        expected-next-editor (->> editor (set-buffer-lines ["Line 1" "e 2"]) (set-cursor [0 1]))]
    (is (= expected-next-editor (delete-from-beginning-of-line editor)))))

(deftest test-duplicate-line
  (let [editor (->> editor/initial-state (set-buffer-lines ["Line 1" "Line 2"]) (set-cursor [2 1]))
        expected-next-editor (->> editor (set-buffer-lines ["Line 1" "Line 2" "Line 2"]) (set-cursor [2 2]))]
    (is (= expected-next-editor (duplicate-line editor)))))

(deftest test-set-size
  (let [editor (->> editor/initial-state (set-buffer-lines ["Line 1" "Line 2" "Line 3" "Line 4"]) (set-size [80 2]))
        expected-next-editor (->> editor (set-size [80 4]))]
    (is (= expected-next-editor (set-size [80 4] editor)))))

(deftest test-set-size-beyond-document-end
  (let [editor (->> editor/initial-state
                    (set-buffer-lines ["Line 1" "Line 2" "Line 3" "Line 4"])
                    (set-cursor [0 2])
                    (set-size [80 2])
                    (set-offset [0 2]))
        expected-next-editor (->> editor
                                  (set-size [80 3])
                                  (set-offset [0 1]))]
    (is (= expected-next-editor (set-size [80 3] editor)))))

(deftest test-set-size-bigger-than-document
  (let [editor (->> editor/initial-state
                    (set-buffer-lines ["Line 1" "Line 2" "Line 3" "Line 4"])
                    (set-cursor [0 2])
                    (set-size [80 2])
                    (set-offset [0 2]))
        expected-next-editor (->> editor
                                  (set-size [80 5])
                                  (set-offset [0 0]))]
    (is (= expected-next-editor (set-size [80 5] editor)))))

(deftest test-set-key-modifier
  (let [editor (editor-with {:key-modifiers #{}})
        expected-next-editor (merge editor {:key-modifiers #{:mode-x}})]
    (is (= ((set-key-modifier :mode-x) editor) expected-next-editor))))

(deftest test-unset-key-modifier
  (let [editor (editor-with {:key-modifiers #{:mode-x :mode-y}})
        expected-next-editor (merge editor {:key-modifiers #{:mode-x}})]
    (is (= ((unset-key-modifier :mode-y) editor) expected-next-editor))))

(deftest test-add-key-binding-just-key
  (let [editor (editor-with {:key-bindings {}})
        expected-next-editor (merge editor {:key-bindings {{:key \w :modifiers #{}} do-nothing}})]
    (is (= ((add-key-binding \w do-nothing) editor) expected-next-editor))))

(deftest test-add-key-binding-key-and-one-modifier
  (let [editor (editor-with {:key-bindings {}})
        expected-next-editor (merge editor {:key-bindings {{:key \x :modifiers #{:ctrl}} do-nothing}})]
    (is (= ((add-key-binding \x :ctrl do-nothing) editor) expected-next-editor))))

(deftest test-add-key-binding-key-and-two-modifier
  (let [editor (editor-with {:key-bindings {}})
        expected-next-editor (merge editor {:key-bindings {{:key \x :modifiers #{:ctrl :alt}} do-nothing}})]
    (is (= ((add-key-binding \x :ctrl :alt do-nothing) editor) expected-next-editor))))

(deftest test-remove-key-binding-just-key
  (let [editor (editor-with {:key-bindings {{:key \w :modifiers #{}} do-nothing}})
        expected-next-editor (merge editor {:key-bindings {}})]
    (is (= ((remove-key-binding \w) editor) expected-next-editor))))

(deftest test-remove-key-binding-key-and-one-modifier
  (let [editor (editor-with {:key-bindings {{:key \x :modifiers #{:ctrl}} do-nothing}})
        expected-next-editor (merge editor {:key-bindings {}})]
    (is (= ((remove-key-binding \x :ctrl) editor) expected-next-editor))))

(deftest test-remove-key-binding-key-and-two-modifier
  (let [editor (editor-with {:key-bindings {{:key \x :modifiers #{:ctrl :alt}} do-nothing}})
        expected-next-editor (merge editor {:key-bindings {}})]
    (is (= ((remove-key-binding \x :ctrl :alt) editor) expected-next-editor))))

(deftest test-set-character-handler
  (let [editor (editor-with {:character-handlers {}})
        expected-next-editor (merge editor {:character-handlers {#{:mode-x} do-nothing}})]
    (is (= ((set-character-handler :mode-x do-nothing) editor) expected-next-editor))))

(deftest test-set-character-handler-multiple-modifiers
  (let [editor (editor-with {:character-handlers {}})
        expected-next-editor (merge editor {:character-handlers {#{:mode-x :ctrl} do-nothing}})]
    (is (= ((set-character-handler :mode-x :ctrl do-nothing) editor) expected-next-editor))))

(deftest test-unset-character-handler
  (let [editor (editor-with {:character-handlers {#{:mode-x} do-nothing}})
        expected-next-editor (merge editor {:character-handlers {}})]
    (is (= ((unset-character-handler :mode-x) editor) expected-next-editor))))

(deftest test-unset-character-handler-multiple-modifiers
  (let [editor (editor-with {:character-handlers {#{:mode-x :ctrl} do-nothing}})
        expected-next-editor (merge editor {:character-handlers {}})]
    (is (= ((unset-character-handler :mode-x :ctrl) editor) expected-next-editor))))

(deftest execute-command-without-args-test
  (let [editor (-> editor/initial-state (set-buffer-lines-new ["Line 1"]))
        command-f (fn [editor] (set-buffer-lines-new editor ["Different" "Lines"]))
        updated-editor (-> editor (execute-command command-f))]
    (is (= ["Different" "Lines"] (get-buffer-lines updated-editor)))))

(deftest execute-command-with-args-test
  (let [editor (-> editor/initial-state (set-buffer-lines-new ["Line 1"]))
        command-f (fn [editor new-lines] (set-buffer-lines-new editor new-lines))
        updated-editor (-> editor (execute-command (list command-f ["Different" "Lines"])))]
    (is (= ["Different" "Lines"] (get-buffer-lines updated-editor)))))
