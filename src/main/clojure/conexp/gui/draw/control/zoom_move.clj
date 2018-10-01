(ns conexp.gui.draw.control.zoom-move
  (:require [conexp.gui.draw.control.util :refer :all]
            [conexp.gui.draw.nodes-and-connections :refer :all]
            [conexp.gui.draw.scenes :refer :all]
            [seesaw.core :refer [listen]])
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
