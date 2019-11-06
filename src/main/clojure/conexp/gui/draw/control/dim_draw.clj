(ns conexp.gui.draw.control.dim-draw
  (:require [conexp.gui.draw.control.util :refer :all]
            [conexp.gui.draw.scene-layouts :refer :all]
            [conexp.gui.draw.scenes :refer :all]
            [conexp.gui.util :refer :all]
            [conexp.layouts.base :refer :all]
            [conexp.layouts.dim-draw :refer :all]
            [seesaw.core :refer [listen]])
  (:import [javax.swing JButton JSpinner]))

;;; DimDraw layout

(defn dimdraw
  "Installs DimDraw Layout control."
  [frame scn buttons]
  (let [^JButton exact    (make-button buttons "Exact")
        ^JLabel l1 (make-label buttons "Individuums:")
        ^JSpinner ind   (make-spinner buttons 0 1000 20 1)
        ^JLabel l2 (make-label buttons "Generations:")
        ^JSpinner gen   (make-spinner buttons 0 1000 20 1)
        ^JButton genetic    (make-button buttons "Genetic")
        ^JButton greedy    (make-button buttons "Greedy")]
    (listen exact :action
            (fn [_]
              (update-layout-of-scene scn (dim-draw-layout (lattice (get-layout-from-scene scn))))
              (fit-scene-to-layout scn)))
    (listen genetic :action
            (fn [_]
              (update-layout-of-scene scn (dim-draw-layout
                                           (lattice (get-layout-from-scene scn))
                                           "genetic"
                                           (int (.getValue ind))
                                           (int (.getValue gen))))
              (fit-scene-to-layout scn)))
    (listen greedy :action
            (fn [_]
              (update-layout-of-scene scn (dim-draw-layout (lattice (get-layout-from-scene scn))
                                                           "greedy"))
              (fit-scene-to-layout scn)))))

;;;

nil
