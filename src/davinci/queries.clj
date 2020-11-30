(ns davinci.queries)

(defn line-relative-to-cursor [dy]
  (fn [editor]
    (let [[_ y] (:cursor editor)]
      (get-in editor [:buffer (+ y dy)]))))

(def previous-line (line-relative-to-cursor -1))

(def current-line (line-relative-to-cursor 0))

(def next-line (line-relative-to-cursor 1))

(defn position-left-of-cursor [editor]
  (let [[x y] (:cursor editor)]
    (if (pos? x)
      [(dec x) y]
      (let [previous (previous-line editor)]
        (if previous
          [(count previous) (dec y)]
          [x y])))))

(defn position-right-of-cursor [editor]
  (let [[x y] (:cursor editor)
        line (current-line editor)
        line-length (count line)]
    (if (< x line-length)
      [(inc x) y]
      (let [next (next-line editor)]
        (if next
          [0 (inc y)]
          [x y])))))

(defn position-up-of-cursor [editor]
  (let [[x y] (:cursor editor)
        previous (previous-line editor)]
    (if-not previous
      [x y]
      (let [previous-length (count previous)]
        [(min previous-length x) (dec y)]))))

(defn position-down-of-cursor [editor]
  (let [[x y] (:cursor editor)
        next (next-line editor)]
    (if-not next
      [x y]
      (let [next-length (count next)]
        [(min next-length x) (inc y)]))))
