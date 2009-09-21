(ns conexp.gui
  (:use conexp.gui.base
	conexp.gui.util))

(defn conexp-gui []
  (let [frame (conexp-main-frame)]
    (.setVisible frame true)
    frame))