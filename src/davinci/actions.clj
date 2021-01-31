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

(defn update-buffer-lines [editor f]
  (update-in editor [:buffer :lines] f))

(deftransform set-buffer-type [type editor]
  (assoc-in editor [:buffer :type] type))

(defn detect-buffer-type [editor]
  (let [path (get-buffer-path editor)
        matching-rules (filter (fn [[matcher _]] (re-find matcher path)) (:file-type-matchers editor))
        first-matching-rule (first matching-rules)]
    (if first-matching-rule
      (set-buffer-type (second first-matching-rule) editor)
      editor)))

(defn set-buffer-lines-new [editor lines]
  (assoc-in editor [:buffer :lines] lines))

(deftransform set-buffer-lines [lines editor]
  (set-buffer-lines-new editor lines))

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
  (let [cursor (get-cursor editor)
        lines (get-buffer-lines editor)]
    (set-cursor (lines/clamp-position lines cursor) editor)))

(defn- clamp-offset [editor]
  (let [[ox oy] (get-offset editor)
        lines (get-buffer-lines editor)]
    (set-offset [ox (min (lines/max-y lines) oy)] editor)))

(deftransform move-cursor-to [position editor]
  (->> editor
       (set-cursor position)
       scroll-to-cursor))

(defn- update-cursor-in-bounds [editor f]
  (let [cursor (get-cursor editor)]
    (if-let [new-position (-> editor get-buffer-lines (f cursor))]
      (move-cursor-to new-position editor)
      editor)))

(defn move-cursor-left [editor]
  (update-cursor-in-bounds editor lines/position-left-of))

(defn move-cursor-right [editor]
  (update-cursor-in-bounds editor lines/position-right-of))

(defn move-cursor-up [editor]
  (update-cursor-in-bounds editor lines/position-up-of))

(defn move-cursor-down [editor]
  (update-cursor-in-bounds editor lines/position-down-of))

(defn delete-previous-character [editor]
  (let [[x y] (get-cursor editor)
        lines (get-buffer-lines editor)]
    (if-let [deleted-position (cond (pos? x) [(dec x) y]
                                    (pos? y) [(lines/get-length lines (dec y)) (dec y)])]
      (-> editor
          (update-buffer-lines #(lines/delete-character % deleted-position))
          ((move-cursor-to deleted-position)))
      editor)))

(defn insert-newline-at-cursor [editor]
  (-> editor
      (update-buffer-lines  #(lines/insert-newline % (get-cursor editor)))
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

(deftransform insert-string-at-cursor [string editor]
  (let [[x y] (get-cursor editor)]
    (-> editor
        (update-buffer-lines #(lines/insert-string % [x y] string))
        ((move-cursor-to [(+ x (count string)) y])))))

(deftransform insert-character-at-cursor [character editor]
  (insert-string-at-cursor (str character) editor))

(defn move-cursor-to-beginning-of-line [editor]
  (let [[_ y] (get-cursor editor)]
    (move-cursor-to [0 y] editor)))

(defn move-cursor-to-end-of-line [editor]
  (let [[_ y] (get-cursor editor)
        new-x (-> editor get-buffer-lines (lines/get-length y))]
    (move-cursor-to [new-x y] editor)))

(defn delete-line [editor]
  (let [[_ y] (get-cursor editor)
        line-count (count (get-buffer-lines editor))
        replacement (if (> line-count 1) [] [""])]
    (-> editor
        (update-buffer-lines #(lines/replace-lines % y (inc y) replacement))
        clamp-cursor
        clamp-offset)))

(defn delete-until-end-of-line [editor]
  (let [[x y] (get-cursor editor)
        line (-> editor get-buffer-lines (lines/get-line y))]
    (update-buffer-lines editor #(lines/replace-line % y (subs line 0 x)))))

(defn delete-from-beginning-of-line [editor]
  (let [[x y] (get-cursor editor)
        line (-> editor get-buffer-lines (lines/get-line y))]
    (-> editor
        (update-buffer-lines #(lines/replace-line % y (subs line x)))
        move-cursor-to-beginning-of-line)))

(defn duplicate-line [editor]
  (let [[_ y] (get-cursor editor)
        current-line (-> editor get-buffer-lines (lines/get-line y))]
    (-> editor
        (update-buffer-lines #(lines/replace-lines % y (inc y) [current-line current-line]))
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

(defn execute-command [editor command]
  (if (list? command)
    (let [command-f (peek command)
          command-args (pop command)]
      (eval (conj command-args editor command-f)))
    (command editor)))

(defmacro bind-input [editor input command]
  (let [normalized-command (if (list? command) (conj command 'list) command)]
    `(assoc-in ~editor [:key-bindings ~input] ~normalized-command)))
