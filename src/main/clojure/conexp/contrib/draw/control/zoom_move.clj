;; Copyright ⓒ the conexp-clj developers; all rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.contrib.draw.control.zoom-move
  (:use conexp.contrib.draw.control.util
        conexp.contrib.gui.util
        conexp.contrib.draw.scenes
        conexp.contrib.draw.nodes-and-connections)
  (:use seesaw.core)
  (:import [javax.swing JButton JLabel]))

;;;

(defn toggle-zoom-move
  "Install zoom-move-toggler."
  [_ scn buttons]
  (let [zoom-factors (fn []
                       (let [[zoom-x zoom-y] (get-zoom-factors scn)]
                         (with-out-str
                           (printf "%1.2f" zoom-x)
                           (print " x ")
                           (printf "%1.2f" zoom-y)))),
        ^JButton zoom-move (make-button buttons "Move"),
        ^JLabel  zoom-info (make-label buttons " -- ")]
    (listen zoom-move :action
            (fn [_]
              (if (= "Move" (.getText zoom-move))
                (do
                  (start-interaction scn zoom-interaction)
                  (.setText zoom-move "Zoom"))
                (do
                  (start-interaction scn move-interaction)
                  (.setText zoom-move "Move")))))
    (add-scene-callback scn :image-changed
                        (fn []
                          (.setText zoom-info (zoom-factors)))))
  nil)

;;;

nil
