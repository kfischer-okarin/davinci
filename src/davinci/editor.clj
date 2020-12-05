(ns davinci.editor)

(def initial-state {:buffer []
                    :path nil
                    :cursor [0 0]
                    :size [80 24]
                    :offset [0 0]
                    :key-bindings {}
                    :key-modifiers #{}
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
