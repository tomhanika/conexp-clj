(ns conexp.gui
  (:import [javax.swing UIManager])
  (:use conexp.gui.base))

(defn conexp-gui
  "Starts the standard gui for conexp-clj."
  []
  (. UIManager (setLookAndFeel (. UIManager (getSystemLookAndFeelClassName))))
  (let [frame (conexp-main-frame)]
    (.setVisible frame true)
    frame))

; for convenience and debugging
(def show-in-frame conexp.gui.util/show-in-frame)

nil
