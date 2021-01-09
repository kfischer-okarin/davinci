(ns davinci.editor
  (:require [clojure.set :as set]))

(def initial-state {:buffer []
                    :path nil
                    :cursor [0 0]
                    :size [80 24]
                    :offset [0 0]
                    :key-bindings {}
                    :key-modifiers #{}
                    :character-handlers {} ; active-modifiers -> handler
                    :running true})

(defn- with-constant-modifiers [editor key]
  (update key :modifiers #(set/union % (:key-modifiers editor))))

(defn get-action-for-key [editor key]
  (get-in editor [:key-bindings (with-constant-modifiers editor key)]))

(defn get-character-handler-action-for-key [editor key]
  (let [key-without-modifiers (:key key)
        character-handler (get-in editor [:character-handlers (:modifiers (with-constant-modifiers editor key))])]
    (if (and (char? key-without-modifiers) character-handler)
      (character-handler key-without-modifiers))))
