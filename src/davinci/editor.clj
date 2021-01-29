(ns davinci.editor
  (:require [clojure.string :as string]
            [clojure.set :as set]))

(defrecord Buffer [lines path type])

(defrecord EditorState [buffer cursor size offset key-bindings key-modifiers character-handlers file-type-matchers running])

(def initial-state (->EditorState nil      ; buffer
                                  [0 0]    ; cursor
                                  [80 24]  ; size
                                  [0 0]    ; offset
                                  {}       ; key-bindings
                                  #{}      ; key-modifiers
                                  {}       ; character-handlers (active-modifiers -> handler)
                                  '()      ; file-type-matchers
                                  true     ; running
                                  ))

(defn- with-constant-modifiers [editor key]
  (update key :modifiers #(set/union % (:key-modifiers editor))))

(defn get-action-for-key [editor key]
  (get-in editor [:key-bindings (with-constant-modifiers editor key)]))

(defn get-character-handler-action-for-key [editor key]
  (let [key-without-modifiers (:key key)
        character-handler (get-in editor [:character-handlers (:modifiers (with-constant-modifiers editor key))])]
    (if (and (char? key-without-modifiers) character-handler)
      (character-handler key-without-modifiers))))
