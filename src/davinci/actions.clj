(ns davinci.actions
  (:require [davinci.queries :refer :all]))

(defmacro deftransform [name args & body]
  "Defines a parametrized transformation and automatically adds an partial arity which returns an 1-ary function which can be used
  as parameterless transformation"
  (let [args-without-last (pop args)
        partial-args (into [name] args-without-last)]
    `(def ~name
       (fn
         (~args ~@body)
         (~args-without-last (apply partial ~partial-args))))))

(defn quit-editor [editor]
  (assoc editor :running false))

(deftransform set-cursor [cursor editor]
  (assoc editor :cursor cursor))

(defn- scroll-to-cursor [editor]
  (let [[x y] (get-cursor editor) [w h] (:size editor) [ox oy] (:offset editor)]
    (cond
      (>= y (+ h oy)) (assoc-in editor [:offset 1] (- y (dec h)))
      (< y oy) (assoc-in editor [:offset 1] y)
      :else editor)))

(defn- clamp-cursor [editor]
  (let [[x y] (get-cursor editor)
        fixed-y (min (get-max-y editor) (max 0 y))
        line-length (count (get-in editor [:buffer fixed-y]))
        fixed-x (min line-length x)]
    (set-cursor [fixed-x fixed-y] editor)))

(defn- clamp-offset [editor]
  (update-in editor [:offset 1] #(min (get-max-y editor) %)))

(defn- clamp-offset-strict [editor]
  (update-in editor [:offset 1] #(min (get-max-y-offset editor) %)))

(deftransform move-cursor-to [position editor]
  (->> editor
       (set-cursor position)
       scroll-to-cursor
       clamp-cursor))

(defn move-cursor-left [editor]
  (move-cursor-to (get-position-left-of-cursor editor) editor))

(defn move-cursor-right [editor]
  (move-cursor-to (get-position-right-of-cursor editor) editor))

(defn move-cursor-up [editor]
  (move-cursor-to (get-position-up-of-cursor editor) editor))

(defn move-cursor-down [editor]
  (move-cursor-to (get-position-down-of-cursor editor) editor))

(defn replace-lines [[start end] new-lines]
  (fn [editor]
    (let [new-lines-vector (if (vector? new-lines) new-lines (vector new-lines))]
      (update editor :buffer #(into [] cat [(take start %) new-lines-vector (drop end %)])))))

(defn replace-current-line [new-lines]
  (fn [editor]
    (let [[_ y] (get-cursor editor)]
      ((replace-lines [y (inc y)] new-lines) editor))))

(defn update-current-line [f]
  (fn [editor]
    (let [[_ y] (get-cursor editor) current-line (get-current-line editor)]
      ((replace-lines [y (inc y)] (f current-line)) editor))))

(defn delete-previous-character [editor]
  (let [[x y] (get-cursor editor)]
    (if (pos? x)
      (-> editor
          ((update-current-line #(str (subs % 0 (dec x)) (subs % x))))
          move-cursor-left)
      (if (pos? y)
        (let [previous-line (get-previous-line editor)
              merged-with-previous-line (str previous-line (get-current-line editor))
              merge-lines (replace-lines [(dec y) (inc y)] merged-with-previous-line)]
          (->> editor
               merge-lines
               (move-cursor-to [(count previous-line) (dec y)])))
        editor))))

(defn insert-newline [editor]
  (let [[x _] (get-cursor editor)
        current-line (get-current-line editor)
        before-cursor (subs current-line 0 x)
        after-cursor (subs current-line x)
        split-line-at-cursor (replace-current-line [before-cursor after-cursor])]
    (-> editor
        split-line-at-cursor
        move-cursor-right)))

(def do-nothing identity)

(defn replace-buffer-with [string]
  #(assoc % :buffer (conj (clojure.string/split string #"\n") "")))

(defn open-file [filename]
  (fn [editor]
    (-> editor
        ((replace-buffer-with (slurp filename)))
        (assoc :path filename))))

(defn save-file-to [filename]
  (fn [editor]
    (spit filename (get-buffer-as-string editor))
    editor))

(defn save-file [editor]
  ((save-file-to (:path editor)) editor))

(defn insert-string [string]
  (fn [editor]
    (let [[x y] (get-cursor editor)]
      (-> editor
          ((update-current-line #(str (subs % 0 x) string (subs % x))))
          ((move-cursor-to [(+ x (count string)) y]))))))

(defn insert-character [character]
  (insert-string (str character)))

(defn delete-line [editor]
  (let [line-count (get-line-count editor)]
    (-> editor
        ((replace-current-line (if (> line-count 1) [] "")))
        (clamp-cursor)
        (clamp-offset))))

(defn delete-until-end-of-line [editor]
  (let [[x _] (get-cursor editor)]
    ((update-current-line #(subs % 0 x)) editor)))

(defn delete-from-beginning-of-line [editor]
  (let [[x y] (get-cursor editor)]
    (-> editor
        ((update-current-line #(subs % x)))
        ((move-cursor-to [0 y])))))

(defn duplicate-line [editor]
  (let [current-line (get-current-line editor)]
    (-> editor
        ((replace-current-line [current-line current-line]))
        move-cursor-down)))

(defn set-size [size]
  (fn [editor]
    (-> editor
        (assoc :size size)
        (clamp-offset-strict))))

(defn page-down [editor]
  (let [[x y] (get-cursor editor)
        [_ h] (:size editor)
        [_ oy] (:offset editor)]
    (-> editor
        ((set-cursor [x (+ y h)]))
        (clamp-cursor)
        (assoc-in [:offset 1] (min (+ oy h) (get-max-y-offset editor))))))

(defn page-up [editor]
  (let [[x y] (get-cursor editor)
        [_ h] (:size editor)
        [_ oy] (:offset editor)]
    (-> editor
        ((set-cursor [x (- y h)]))
        (clamp-cursor)
        (assoc-in [:offset 1] (max (- oy h) 0)))))

(defn move-cursor-to-beginning-of-line [editor]
  (let [[x y] (get-cursor editor)]
    (move-cursor-to [0 y] editor)))

(defn move-cursor-to-end-of-line [editor]
  (let [[x y] (get-cursor editor)]
    (move-cursor-to [(count (get-current-line editor)) y] editor)))

(defn set-key-modifier [modifier]
  (fn [editor]
    (update editor :key-modifiers #(conj % modifier))))

(defn unset-key-modifier [modifier]
  (fn [editor]
    (update editor :key-modifiers #(disj % modifier))))

(defn add-key-binding [key & modifiers-and-action]
  (fn [editor]
    (let [modifiers (set (butlast modifiers-and-action))
          action (last modifiers-and-action)]
      (update editor :key-bindings #(assoc % {:key key :modifiers modifiers} action)))))

(defn remove-key-binding [key & modifiers]
  (fn [editor]
    (update editor :key-bindings #(dissoc % {:key key :modifiers (set modifiers)}))))

(defn set-character-handler [& modifiers-and-handler]
  (fn [editor]
    (let [modifiers (set (butlast modifiers-and-handler))
          handler (last modifiers-and-handler)]
      (update editor :character-handlers #(assoc % modifiers handler)))))

(defn unset-character-handler [& modifiers]
  (fn [editor]
    (update editor :character-handlers #(dissoc % (set modifiers)))))
