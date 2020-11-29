(ns davinci.actions)

(defn quit-editor [editor]
  (assoc editor :running false))

(defn- fix-cursor-position [editor [x y]]
  (let [max-y (- (count (:buffer editor)) 1)
        fixed-y (min (max 0 y) max-y)
        current-line (get-in editor [:buffer fixed-y])
        max-x (count current-line)
        fixed-x (min (max 0 x) max-x)]
    [fixed-x fixed-y]))

(defn- move-cursor [dx dy editor]
  (update editor :cursor
          (comp
           (partial fix-cursor-position editor)
           (fn [[x y]]  [(+ x dx) (+ y dy)]))))

(def move-cursor-left
  (partial move-cursor -1 0))

(def move-cursor-right
  (partial move-cursor 1 0))

(def move-cursor-up
  (partial move-cursor 0 -1))

(def move-cursor-down
  (partial move-cursor 0 1))

(defn delete-previous-character [editor]
  (let [[x y] (:cursor editor) prev-x (- x 1)]
    (-> editor
        (update-in [:buffer y] #(str (subs % 0 prev-x) (subs % x)))
        move-cursor-left)))

(def do-nothing identity)
