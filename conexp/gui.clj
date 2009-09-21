(ns conexp.gui
  (:import [javax.swing UIManager])
  (:use conexp.gui.base
	conexp.gui.util))

(. UIManager (setLookAndFeel (. UIManager (getSystemLookAndFeelClassName))))

(defn conexp-gui []
  (let [frame (conexp-main-frame)]
    (.setVisible frame true)
    frame))