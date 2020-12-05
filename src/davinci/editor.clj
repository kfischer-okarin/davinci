(ns davinci.editor)

(def initial-state {:buffer []
                    :path nil
                    :cursor [0 0]
                    :size [80 24]
                    :offset [0 0]
                    :key-bindings {}
                    :key-modifiers #{}
                    :character-handlers {} ; active-modifiers -> handler
                    :running true})

(def state (atom initial-state))

(defn get-value [query]
  (query @state))

(defn execute-action [action]
  (swap! state action))

(defn execute-actions [& actions]
  (doseq [action actions] (execute-action action)))

(defn get-action-for-key [key]
  (get-in @state [:key-bindings key]))

(defn get-character-handler-action-for-key [key]
  (let [key-without-modifiers (:key key)
        character-handler (get-in @state [:character-handlers (:modifiers key)])]
    (if (and (char? key-without-modifiers) character-handler)
      (character-handler key-without-modifiers))))
