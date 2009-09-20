(ns conexp.gui
  (:use conexp.gui.base))

(defn conexp-gui []
  (let [frame (conexp-main-frame)]
    (add-additional-menus frame [{:name "Hallo", :content []}])
    (.setVisible frame true)))