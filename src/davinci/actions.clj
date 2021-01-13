(ns davinci.actions
  (:require [clojure.string :as string]
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

(deftransform set-buffer [buffer editor]
  (assoc editor :buffer buffer))

(deftransform set-path [path editor]
  (assoc editor :path path))

(deftransform set-buffer-to-string [string editor]
  (let [lines (clojure.string/split string #"\n")]
    (set-buffer (if (string/ends-with? string "\n") (conj lines "") lines) editor)))

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
        fixed-y (min (get-max-y editor) (max 0 y))
        line-length (count (get (get-buffer editor) fixed-y))
        fixed-x (min line-length x)]
    (set-cursor [fixed-x fixed-y] editor)))

(defn- clamp-offset [editor]
  (let [[ox oy] (get-offset editor)]
    (set-offset [ox (min (get-max-y editor) oy)] editor)))

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
  (let [buffer (get-buffer editor)
        new-lines-vector (if (vector? new-lines) new-lines (vector new-lines))
        new-buffer (into [] cat [(take start buffer) new-lines-vector (drop end buffer)])]
    (set-buffer new-buffer editor)))

(deftransform replace-line [line-no new-lines editor]
  (replace-lines [line-no (inc line-no)] new-lines editor))

(deftransform replace-current-line [new-lines editor]
  (let [[_ y] (get-cursor editor)]
    (replace-line y new-lines editor)))

(deftransform update-line [line-no f editor]
  (let [line (get-line line-no editor)]
    (replace-line line-no (f line) editor)))

(deftransform delete-character [[x y] editor]
  (let [lines-count (count (get-buffer editor))
        line (get-line y editor)
        line-length (count line)]
    (if (<= 0 y (dec lines-count))
      (cond
        (<= 0 x (dec line-length)) (update-line y #(str (subs % 0 x) (subs % (inc x))) editor)
        (and (= x line-length) (< y (dec lines-count))) (let [next-line (get-line (inc y) editor)
                                                              merged-with-next-line (str line next-line)]
                                                          (replace-lines [y (+ y 2)] merged-with-next-line editor))
        :else editor)
      editor)))

(defn delete-previous-character [editor]
  (let [[x y] (get-cursor editor)
        deleted-position (cond (pos? x) [(dec x) y]
                               (pos? y) [(count (get-previous-line editor)) (dec y)])]
    (if deleted-position
      (->> editor
           (delete-character deleted-position)
           (move-cursor-to deleted-position))
      editor)))

(deftransform insert-newline [position editor]
  (let [[x y] position
        current-line (get-line y editor)
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
       (set-buffer-to-string (slurp filename))
       (set-path filename)))

(deftransform save-file-to [filename editor]
  (spit filename (get-buffer-as-string editor))
  editor)

(defn save-file [editor]
  (save-file-to (get-path editor) editor))

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
  (let [line-count (get-line-count editor)]
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
  (let [current-line (get-current-line editor)]
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
