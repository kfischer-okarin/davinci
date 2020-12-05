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

(defn bind-key [key action]
  (swap! state #(assoc-in % [:key-bindings (if (map? key) key {:key key :modifiers #{}})] action)))

(defn get-action-for-key [key]
  (get-in @state [:key-bindings key]))
