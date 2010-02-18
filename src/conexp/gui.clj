(ns conexp.gui
  (:import [javax.swing UIManager])
  (:use conexp.gui.base))


(defn gui
  "Starts the standard gui for conexp-clj."
  []
  (. UIManager (setLookAndFeel (. UIManager (getSystemLookAndFeelClassName))))
  (let [frame (conexp-main-frame)]
    (.setVisible frame true)
    frame))

nil
