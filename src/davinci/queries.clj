(ns davinci.queries)

(defn get-line-relative-to-cursor [dy]
  (fn [editor]
    (let [[_ y] (:cursor editor)]
      (get-in editor [:buffer (+ y dy)]))))

(def get-previous-line
  (get-line-relative-to-cursor -1))

(def get-current-line
  (get-line-relative-to-cursor 0))

(def get-next-line
  (get-line-relative-to-cursor 1))

(defn get-line-count [editor]
  (count (:buffer editor)))

(defn get-max-y [editor]
  (- (get-line-count editor) 1))

(defn get-max-y-offset [editor]
  (let [[w h] (:size editor)]
    (max (- (get-line-count editor) h) 0)))

(defn get-visible-lines [editor]
  (let [[_ h] (:size editor) [_ oy] (:offset editor)]
    (subvec (:buffer editor) oy (min (+ oy h) (get-line-count editor)))))

(defn position-left-of-cursor [editor]
  (let [[x y] (:cursor editor)]
    (if (pos? x)
      [(dec x) y]
      (let [previous-line (get-previous-line editor)]
        (if previous-line
          [(count previous-line) (dec y)]
          [x y])))))

(defn position-right-of-cursor [editor]
  (let [[x y] (:cursor editor)
        current-line (get-current-line editor)
        current-line-length (count current-line)]
    (if (< x current-line-length)
      [(inc x) y]
      (let [next-line (get-next-line editor)]
        (if next-line
          [0 (inc y)]
          [x y])))))

(defn position-up-of-cursor [editor]
  (let [[x y] (:cursor editor)
        previous-line (get-previous-line editor)]
    (if-not previous-line
      [x y]
      (let [previous-line-length (count previous-line)]
        [(min previous-line-length x) (dec y)]))))

(defn position-down-of-cursor [editor]
  (let [[x y] (:cursor editor)
        next-line (get-next-line editor)]
    (if-not next-line
      [x y]
      (let [next-line-length (count next-line)]
        [(min next-line-length x) (inc y)]))))
