(ns davinci.terminal
  (:import
   com.googlecode.lanterna.terminal.DefaultTerminalFactory
   com.googlecode.lanterna.terminal.Terminal
   com.googlecode.lanterna.terminal.TerminalResizeListener
   com.googlecode.lanterna.input.KeyType
   com.googlecode.lanterna.TextColor$Indexed)
  (:require [davinci.queries :as queries]
            [davinci.ui :as ui]))

(def input-events (atom nil))
(def last-key (atom nil))

(defn- get-terminal
  "Creates a text terminal."
  []
  (let [factory (DefaultTerminalFactory.)]
    (.setForceTextTerminal factory true)
    (.createTerminal factory)))

(defn- add-resize-listener [terminal f]
  "Sets the passed function as resize listener for the terminal.
   The passed function receives the new [w h] as vector."
  (let [listener (proxy [TerminalResizeListener] []
                   (onResized [_ new-size]
                     (f [(.getColumns new-size) (.getRows new-size)])))]
    (.addResizeListener terminal listener)))

(defn- start
  "Start using terminal"
  [terminal] (.enterPrivateMode terminal))

(defn- stop
  "Stop using terminal"
  [terminal] (.exitPrivateMode terminal))

(defn get-size
  "Get terminal size as [cols rows]"
  [terminal] (let [terminal-size (.getTerminalSize terminal)]
               [(.getColumns terminal-size) (.getRows terminal-size)]))

(defn- move-cursor
  "Move the cursor to x y"
  [terminal x y] (.setCursorPosition terminal x y))

(defn- clear
  "Clears the terminal and set cursor to [0 0]"
  [terminal]
  (get-size terminal) ; Get size because of performance  ???
  (.clearScreen terminal)
  (move-cursor terminal 0 0))

(defn- put-character
  "Puts character at current terminal position"
  [terminal character]
  (.putCharacter terminal character))

(defn- flush-terminal [terminal]
  (.flush terminal))

(defn- parse-color [color-name]
  (case color-name
    :red (TextColor$Indexed. 1)
    :default (TextColor$Indexed. 8)
    :white (TextColor$Indexed. 15)))

(defn- set-foreground-color [terminal color-name]
  (.setForegroundColor terminal (parse-color color-name)))

(defn- set-background-color [terminal color-name]
  (.setBackgroundColor terminal (parse-color color-name)))

(defn- reset-color-and-style [terminal]
  (.resetColorAndSGR terminal))

(defn- put-string
  "Puts string at the current terminal position"
  ([terminal string]
   (doseq [character string] (put-character terminal character)))
  ([terminal string fg-color]
   (set-foreground-color terminal fg-color)
   (put-string terminal string)
   (reset-color-and-style terminal))
  ([terminal string fg-color bg-color]
   (set-background-color terminal bg-color)
   (put-string terminal string fg-color)))

(defn- get-key-raw
  "Gets the raw Key object from the terminal"
  [terminal] (.pollInput terminal))

(defn- parse-key [key]
  (case (.name (.getKeyType key))
    "Character" (.getCharacter key)
    "ArrowDown" :down
    "ArrowLeft" :left
    "ArrowRight" :right
    "ArrowUp" :up
    "Backspace" :backspace
    "Delete" :delete
    "End" :end
    "Enter" :enter
    "Escape" :escape
    "F1" :f1
    "F2" :f2
    "F3" :f3
    "F4" :f4
    "F5" :f5
    "F6" :f6
    "F7" :f7
    "F8" :f8
    "F9" :f9
    "F10" :f10
    "F11" :f11
    "F12" :f12
    "F13" :f13
    "F14" :f14
    "F15" :f15
    "F16" :f16
    "F17" :f17
    "F18" :f18
    "F19" :f19
    "Home" :home
    "Insert" :insert
    "PageDown" :page-down
    "PageUp" :page-up
    "ReverseTab" :reverse-tab
    "Tab" :tab
    "Unknown" :unknown
    :unknown))

(defn- get-key-modifiers [key]
  (cond-> #{}
    (.isAltDown key) (conj :alt)
    (.isCtrlDown key) (conj :ctrl)
    (and (.isShiftDown key) (keyword? (parse-key key))) (conj :shift)))

(defn- get-key
  [terminal]
  (let [key (get-key-raw terminal)]
    (if key
      {:key (parse-key key)
       :modifiers (get-key-modifiers key)})))

(defn- emit-input-event [event]
  (swap! input-events #(conj % event)))

(defn- emit-resize-event [size]
  (emit-input-event {:type :resize :payload size}))

(defn- render-two-part-status-bar [terminal editor left-content right-content]
  (let [[w _] (queries/get-size editor)
        left-w (max (quot w 2) 1)
        right-w (max (- w left-w) 1)]
    (put-string terminal (format (str "%-" left-w "s") left-content) :white :red)
    (put-string terminal (format (str "%" right-w "s") right-content) :white :red)))

(defn- render-status-bar [terminal editor]
  (let [[_ h] (queries/get-size editor)
        [x y] (queries/get-cursor editor)
        position (str (queries/get-buffer-path editor) ":" (inc y) ":" (inc x))]
    (move-cursor terminal 0 h)
    (if (contains? (:key-modifiers editor) :command-mode)
      (render-two-part-status-bar terminal editor position "COMMAND MODE")
      (render-two-part-status-bar terminal editor position (str "Last key: " @last-key)))))

(defn- render-editor-in-terminal [terminal editor]
  (clear terminal)
  (doseq [line (queries/get-visible-lines editor)]
    (put-string terminal (str line \newline)))
  (let [[x y] (queries/get-cursor editor)
        [ox oy] (queries/get-offset editor)]
    (render-status-bar terminal editor)
    (move-cursor terminal (- x ox) (- y oy)))
  (flush-terminal terminal))

(defn- get-input-from-terminal [terminal]
  (while (not (peek @input-events))
    (let [key (get-key terminal)]
      (if key (emit-input-event key))))
  (let [input-event (peek @input-events)]
    (if (contains? input-event :key) (reset! last-key input-event))
    (swap! input-events pop)
    input-event))

(defn init-terminal []
  (reset! input-events clojure.lang.PersistentQueue/EMPTY)
  (reset! last-key nil)
  (let [terminal (get-terminal)]
    (start terminal)
    (add-resize-listener terminal
                         (fn [size]
                           (if (not= size [1 1]) (emit-resize-event size))))
    terminal))

(extend-type Terminal ui/EditorUI
             (render-editor [terminal editor] (render-editor-in-terminal terminal editor))
             (get-input [terminal] (get-input-from-terminal terminal))
             (shutdown [terminal] (stop terminal)))
