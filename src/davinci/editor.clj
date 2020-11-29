(ns davinci.editor)

(def ^:private editor (atom {:buffer []
                             :cursor [0 0]
                             :key-bindings {}
                             :running true}))

(defn execute-action [action]
  (swap! editor action))

(defn is-running []
  (:running @editor))

(defn bind-key [key action]
  (swap! editor #(assoc-in % [:key-bindings (if (map? key) key {:key key :modifiers #{}})] action)))

(defn get-action-for-key [key]
  (get-in @editor [:key-bindings key]))

(defn get-buffer []
  (:buffer @editor))

(defn replace-buffer [new-buffer]
  (execute-action #(assoc % :buffer new-buffer)))

(defn get-cursor []
  (:cursor @editor))
