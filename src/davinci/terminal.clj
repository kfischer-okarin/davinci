(ns davinci.terminal
  (:import
   com.googlecode.lanterna.terminal.DefaultTerminalFactory
   com.googlecode.lanterna.terminal.TerminalResizeListener
   com.googlecode.lanterna.input.KeyType
   com.googlecode.lanterna.TextColor$Indexed))

(defn get-terminal
  "Creates a text terminal."
  []
  (let [factory (DefaultTerminalFactory.)]
    (.setForceTextTerminal factory true)
    (.createTerminal factory)))

(defn add-resize-listener [terminal f]
  "Sets the passed function as resize listener for the terminal.
   The passed function receives the new [w h] as vector."
  (let [listener (proxy [TerminalResizeListener] []
                   (onResized [_ new-size]
                     (f [(.getColumns new-size) (.getRows new-size)])))]
    (.addResizeListener terminal listener)))

(defn start
  "Start using terminal"
  [terminal] (.enterPrivateMode terminal))

(defn stop
  "Stop using terminal"
  [terminal] (.exitPrivateMode terminal))

(defn get-size
  "Get terminal size as [cols rows]"
  [terminal] (let [terminal-size (.getTerminalSize terminal)]
               [(.getColumns terminal-size) (.getRows terminal-size)]))

(defn move-cursor
  "Move the cursor to x y"
  [terminal x y] (.setCursorPosition terminal x y))

(defn clear
  "Clears the terminal and set cursor to [0 0]"
  [terminal]
  (get-size terminal) ; Get size because of performance  ???
  (.clearScreen terminal)
  (move-cursor terminal 0 0))

(defn put-character
  "Puts character at current terminal position"
  [terminal character]
  (.putCharacter terminal character))

(defn flush-terminal [terminal]
  (.flush terminal))

(defn- parse-color [color-name]
  (case color-name
    :red (TextColor$Indexed. 1)
    :default (TextColor$Indexed. 8)
    :white (TextColor$Indexed. 15)))

(defn set-foreground-color [terminal color-name]
  (.setForegroundColor terminal (parse-color color-name)))

(defn set-background-color [terminal color-name]
  (.setBackgroundColor terminal (parse-color color-name)))

(defn reset-color-and-style [terminal]
  (.resetColorAndSGR terminal))

(defn put-string
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

(defn get-key-raw
  "Gets the raw Key object from the terminal (blocking)"
  [terminal] (.readInput terminal))

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

(defn get-key
  [terminal]
  (let [key (get-key-raw terminal)]
    {:key (parse-key key)
     :modifiers (get-key-modifiers key)}))

(defmacro in-terminal [terminal & body]
  `(do
     (start ~terminal)
     (try
       ~@body
       (stop ~terminal)
       (catch Exception e#
         (stop ~terminal)
         (.printStackTrace e#)))))
