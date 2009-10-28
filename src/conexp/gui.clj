(ns conexp.gui
  (:import [javax.swing UIManager])
  (:use conexp.gui.base))

(defn conexp-gui []
  (. UIManager (setLookAndFeel (. UIManager (getSystemLookAndFeelClassName))))
  (let [frame (conexp-main-frame)]
    (.setVisible frame true)
    frame))
