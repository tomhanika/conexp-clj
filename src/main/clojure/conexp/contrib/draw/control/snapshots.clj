;; Copyright ⓒ the conexp-clj developers; all rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.contrib.draw.control.snapshots
  (:use [conexp.base :only (now)]
        conexp.contrib.draw.control.util
        conexp.contrib.gui.util
        conexp.contrib.draw.scenes
        conexp.contrib.draw.scene-layouts)
  (:use seesaw.core)
  (:import [javax.swing JComboBox JButton]))

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
