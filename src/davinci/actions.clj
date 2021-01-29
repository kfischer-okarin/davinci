(ns davinci.actions
  (:require [davinci.editor :refer [->Buffer]]
            [davinci.lines :as lines]
            [davinci.queries :refer :all]))

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

(deftransform set-buffer [lines path editor]
  (assoc editor :buffer (->Buffer lines path nil)))

(deftransform set-buffer-type [type editor]
  (assoc-in editor [:buffer :type] type))

(defn detect-buffer-type [editor]
  (let [path (get-buffer-path editor)
        matching-rules (filter (fn [[matcher _]] (re-find matcher path)) (:file-type-matchers editor))
        first-matching-rule (first matching-rules)]
    (if first-matching-rule
      (set-buffer-type (second first-matching-rule) editor)
      editor)))

(deftransform set-buffer-lines [lines editor]
  (if (:buffer editor)
    (assoc-in editor [:buffer :lines] lines)
    (set-buffer lines nil editor)))

(deftransform set-offset [offset editor]
  (assoc editor :offset offset))

(defn- clamp-offset-strict [editor]
  (let [[ox oy] (get-offset editor)]
    (set-offset [ox (min (get-max-y-offset editor) oy)] editor)))

(deftransform set-size [size editor]
  (-> editor
      (assoc :size size)
      (clamp-offset-strict)))

(defn- scroll-to-cursor [editor]
  (let [[x y] (get-cursor editor)
        [w h] (get-size editor)
        [ox oy] (get-offset editor)]
    (cond
      (>= y (+ h oy)) (set-offset [ox (- y (dec h))] editor)
      (< y oy) (set-offset [ox y] editor)
      :else editor)))

(defn- clamp-cursor [editor]
  (let [[x y] (get-cursor editor)
        lines (get-buffer-lines editor)
        fixed-y (min (lines/max-y lines) (max 0 y))
        line-length (lines/get-length (get-buffer-lines editor) fixed-y)
        fixed-x (min line-length x)]
    (set-cursor [fixed-x fixed-y] editor)))

(defn- clamp-offset [editor]
  (let [[ox oy] (get-offset editor)
        lines (get-buffer-lines editor)]
    (set-offset [ox (min (lines/max-y lines) oy)] editor)))

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

(deftransform replace-lines [[start end] new-lines editor]
  (let [lines (get-buffer-lines editor)
        new-lines-vector (if (vector? new-lines) new-lines (vector new-lines))
        new-buffer (into [] cat [(take start lines) new-lines-vector (drop end lines)])]
    (set-buffer-lines new-buffer editor)))

(deftransform replace-line [line-no new-lines editor]
  (replace-lines [line-no (inc line-no)] new-lines editor))

(deftransform replace-current-line [new-lines editor]
  (let [[_ y] (get-cursor editor)]
    (replace-line y new-lines editor)))

(deftransform update-line [line-no f editor]
  (let [lines (get-buffer-lines editor)
        line (lines/get-line lines line-no)]
    (replace-line line-no (f line) editor)))

(deftransform delete-character [[x y] editor]
  (let [lines (get-buffer-lines editor)
        max-y (lines/max-y lines)
        line (lines/get-line lines y)
        line-length (lines/get-length lines y)]
    (if (< y max-y)
      (cond
        (<= 0 x (dec line-length)) (update-line y #(str (subs % 0 x) (subs % (inc x))) editor)
        (and (= x line-length) (< y max-y)) (let [next-line (lines/get-line lines (inc y))
                                                  merged-with-next-line (str line next-line)]
                                              (replace-lines [y (+ y 2)] merged-with-next-line editor))
        :else editor)
      editor)))

(defn delete-previous-character [editor]
  (let [[x y] (get-cursor editor)
        lines (get-buffer-lines editor)
        deleted-position (cond (pos? x) [(dec x) y]
                               (pos? y) [(lines/get-length lines (dec y)) (dec y)])]
    (if deleted-position
      (->> editor
           (delete-character deleted-position)
           (move-cursor-to deleted-position))
      editor)))

(deftransform insert-newline [position editor]
  (let [[x y] position
        lines (get-buffer-lines editor)
        current-line (lines/get-line lines y)
        before-cursor (subs current-line 0 x)
        after-cursor (subs current-line x)]
    (replace-line y [before-cursor after-cursor] editor)))

(defn insert-newline-at-cursor [editor]
  (->> editor
       (insert-newline (get-cursor editor))
       move-cursor-right))

(def do-nothing identity)

(deftransform open-file [filename editor]
  (->> editor
       (set-buffer (lines/->lines (slurp filename)) filename)
       detect-buffer-type))

(deftransform save-file-to [filename editor]
  (spit filename (lines/->string (get-buffer-lines editor)))
  editor)

(defn save-file [editor]
  (save-file-to (get-buffer-path editor) editor))

(deftransform insert-string [string position editor]
  (let [[x y] position]
    (update-line y #(str (subs % 0 x) string (subs % x)) editor)))

(deftransform insert-character [character position editor]
  (insert-string (str character) position editor))

(deftransform insert-string-at-cursor [string editor]
  (let [[x y] (get-cursor editor)]
    (->> editor
         (insert-string string [x y])
         (move-cursor-to [(+ x (count string)) y]))))

(deftransform insert-character-at-cursor [character editor]
  (insert-string-at-cursor (str character) editor))

(defn delete-line [editor]
  (let [line-count (count (get-buffer-lines editor))]
    (->> editor
         (replace-current-line (if (> line-count 1) [] ""))
         clamp-cursor
         (clamp-offset))))

(defn delete-until-end-of-line [editor]
  (let [[x y] (get-cursor editor)]
    (update-line y #(subs % 0 x) editor)))

(defn delete-from-beginning-of-line [editor]
  (let [[x y] (get-cursor editor)]
    (->> editor
         (update-line y #(subs % x))
         (move-cursor-to [0 y]))))

(defn duplicate-line [editor]
  (let [[_ y] (get-cursor editor)
        lines (get-buffer-lines editor)
        current-line (lines/get-line lines y)]
    (->> editor
         (replace-current-line [current-line current-line])
         move-cursor-down)))

(defn page-down [editor]
  (let [[x y] (get-cursor editor)
        [_ h] (get-size editor)
        [ox oy] (get-offset editor)]
    (->> editor
         (set-cursor [x (+ y h)])
         clamp-cursor
         (set-offset [ox (min (+ oy h) (get-max-y-offset editor))]))))

(defn page-up [editor]
  (let [[x y] (get-cursor editor)
        [_ h] (get-size editor)
        [ox oy] (get-offset editor)]
    (->> editor
         (set-cursor [x (- y h)])
         clamp-cursor
         (set-offset [ox (max (- oy h) 0)]))))

(defn move-cursor-to-beginning-of-line [editor]
  (let [[x y] (get-cursor editor)]
    (move-cursor-to [0 y] editor)))

(defn move-cursor-to-end-of-line [editor]
  (let [[x y] (get-cursor editor)
        lines (get-buffer-lines editor)]
    (move-cursor-to [(lines/get-length lines y) y] editor)))

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

(deftransform recognize-file-type [file-type matcher editor]
  (update editor :file-type-matchers #(conj % [matcher file-type])))
