(ns conexp.gui.draw.control.dim-draw
  (:require [conexp.gui.draw.control.util :refer :all]
            [conexp.gui.draw.scene-layouts :refer :all]
            [conexp.gui.draw.scenes :refer :all]
            [conexp.gui.util :refer :all]
            [conexp.layouts.base :refer :all]
            [conexp.layouts.dim-draw :refer :all]
            [seesaw.core :refer [listen]])
  (:import [javax.swing JButton]))

;;; DimDraw layout

(defn dimdraw
  "Installs DimDraw Layout control."
  [frame scn buttons]
  (let [^JButton btn    (make-button buttons "DimDraw")]
    (listen btn :action
            (fn [_]
              (update-layout-of-scene scn (dim-draw-layout (lattice (get-layout-from-scene scn))))
              (fit-scene-to-layout scn)))))

;;;

nil
