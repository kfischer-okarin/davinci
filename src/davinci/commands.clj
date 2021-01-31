(ns davinci.commands
  (:require [davinci.editor :refer [->Buffer]]
            [davinci.lines :as lines]
            [davinci.queries :refer :all]))



(defn execute-command [editor command]
  (if (list? command)
    (let [command-f (peek command)
          command-args (pop command)]
      (eval (conj command-args editor command-f)))
    (command editor)))

(defn set-buffer-lines [editor lines]
  (assoc-in editor [:buffer :lines] lines))

(defmacro bind-input [editor input command]
  (let [normalized-command (if (list? command) (conj command 'list) command)]
    `(assoc-in ~editor [:key-bindings ~input] ~normalized-command)))
