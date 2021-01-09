(ns davinci.ui)

(defprotocol EditorUI
  "Interface for rendering the editor"
  (render-editor [ui editor] "Renders editor")
  (get-input [ui] "Returns input from UI")
  (shutdown [ui] "Perform cleanup operation when Editor quits."))
