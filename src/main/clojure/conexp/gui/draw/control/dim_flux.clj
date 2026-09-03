;; Copyright ⓒ the conexp-clj developers; all rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.gui.draw.control.dim-flux
  (:require [conexp.gui.draw.control.util :refer :all]
            [conexp.gui.draw.scene-layouts :refer :all]
            [conexp.gui.draw.scenes :refer :all]
            [conexp.gui.util :refer :all]
            [conexp.layouts.base :refer :all]
            [conexp.layouts.dim-flux :refer :all]
            [seesaw.core :refer [listen]])
  (:import [javax.swing JButton JLabel JSpinner]))

;;; DimFlux layout

(defn dimflux
  "Installs DimFlux Layout control."
  [frame scn buttons]
  (let [^JButton  draw       (make-button buttons "DimFlux")
        ^JLabel   l1         (make-label buttons "Iterations:")
        ^JSpinner iterations (make-spinner buttons 0 10000 1000 100)
        ^JButton  project    (make-button buttons "Additive")]
    (listen draw :action
            (fn [_]
              (update-layout-of-scene
               scn
               (dim-flux-layout (poset (get-layout-from-scene scn))
                                {:iterations (int (.getValue iterations))}))
              (call-scene-hook scn :update-grid)
              (fit-scene-to-layout scn)))
    (listen project :action
            (fn [_]
              (update-layout-of-scene
               scn
               (additive-layout (get-layout-from-scene scn)))
              (call-scene-hook scn :update-grid)
              (fit-scene-to-layout scn)))))

;;;

nil
