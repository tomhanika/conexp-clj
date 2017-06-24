(ns conexp.gui.draw.control.snapshots
  (:require [conexp.base :refer :all]
            [conexp.gui.draw.control.util :refer :all]
            [conexp.gui.draw.scene-layouts :refer :all]
            [conexp.gui.draw.scenes :refer :all]
            [conexp.gui.util :refer :all])
  (:import [javax.swing JButton JComboBox]))

;;;

(defn snapshot-saver
  "Installs a snapshot saver, which, whenever a node has been moved,
  saves the image."
  [_ scn buttons]
  (let [saved-layouts (atom {}),
        ^JComboBox
        combo         (make-combo-box buttons @saved-layouts),
        save-layout   (fn [_]
                        (let [layout (get-layout-from-scene scn),
                              key    (now)]
                          (swap! saved-layouts assoc key layout)
                          (.addItem combo key))),
        ^JButton
        snapshot      (make-button buttons "Snapshot")]
    (add-scene-callback scn :move-stop save-layout)
    (listen combo :action
      (fn [evt]
        (let [selected (.getSelectedItem
                        ^JComboBox (.getSource ^java.awt.event.ActionEvent evt)),
              layout   (@saved-layouts selected)]
          (update-layout-of-scene scn layout)
          (fit-scene-to-layout scn layout))))
    (listen snapshot :action
      (fn [_] (save-layout nil)))
    (save-layout nil)))

;;;

nil
