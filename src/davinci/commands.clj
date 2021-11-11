(ns davinci.commands
  (:require [clojure.spec.alpha :as s]
            [davinci.editor :refer [->Buffer]]
            [davinci.lines :as lines]
            [davinci.queries :refer :all]))

(s/def ::editor #(instance? davinci.editor.EditorState %))

(s/def ::command (s/fspec :args (s/cat :editor ::editor)
                          :ret ::editor))

(s/check-asserts true)

(defn execute-command [editor command]
  (if (list? command)
    (let [command-f (peek command)
          command-args (pop command)]
      (eval (conj command-args editor command-f)))
    (command editor)))

; (println (s/conform ::command execute-command))

(defn set-buffer-lines [editor lines]
  (assoc-in editor [:buffer :lines] lines))

(defmacro bind-input [editor input command]
  (let [normalized-command (if (list? command) (conj command 'list) command)]
    `(assoc-in ~editor [:key-bindings ~input] ~normalized-command)))
