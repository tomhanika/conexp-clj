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
  (:import [javax.swing JButton JComboBox JLabel JSpinner]))

;;; DimFlux layout

(def ^:private starting-diagrams
  "The diagram the refinement starts from, in the order they are offered.  The
  first one is the algorithm as published and the default; the others trade the
  two-dimension extension of DimDraw for speed and are what makes DimFlux usable
  on lattices of more than about 30 concepts."
  (array-map "DimDraw (exact)"     :dim-draw
             "DimDraw (greedy)"    :greedy
             "Planarity enhancer"  :planarity-enhancer
             "Layered"             :layered))

(def ^:private start-tooltips
  {"DimDraw (exact)"
   (str "<html>The algorithm as published.  Gives the best diagrams, but the"
        "<br>two-dimension extension of DimDraw dominates the running time"
        "<br>and grows very steeply: roughly a second at 16 concepts and"
        "<br>half a minute at 23.</html>")
   "DimDraw (greedy)"
   (str "<html>Fast.  Uses the greedy two-dimension extension instead of the"
        "<br>exact one, giving up its minimality.  Still costs seconds once"
        "<br>the lattice passes about 50 concepts, and the result varies"
        "<br>between runs.</html>")
   "Planarity enhancer"
   (str "<html>Fast.  The alternative offered by the DimFlux paper: it orders"
        "<br>the irreducible elements by their sup-inf distances and places"
        "<br>them on two parabolas, using no SAT search at all.  Its cost"
        "<br>depends on the number of irreducible elements rather than on the"
        "<br>number of concepts, so it stays near a tenth of a second even at"
        "<br>a hundred concepts.  Usually the best choice above 30.</html>")
   "Layered"
   (str "<html>Fastest and crudest.  Skips DimDraw entirely and starts from"
        "<br>the layered layout, leaving all the work to the refinement."
        "<br>Worth trying when even the planarity enhancer is too slow.</html>")})

(defn dimflux
  "Installs DimFlux Layout control."
  [frame scn buttons]
  (make-label buttons "Start from")
  (let [^JComboBox start   (make-combo-box buttons (keys starting-diagrams))
        ^JLabel   l1       (make-label buttons "Iterations:")
        ^JSpinner iters    (make-spinner buttons 0 10000 1000 100)
        ^JLabel   l2       (make-label buttons "Spring steps:")
        ^JSpinner spring   (make-spinner buttons 0 2000 200 50)
        ^JButton  draw     (make-button buttons "Redraw")
        ^JButton  project  (make-button buttons "Make Additive")
        enhancer-only      [l2 spring]
        update-options!    (fn []
                             (let [selected (.getSelectedItem start)
                                   enhancer? (= :planarity-enhancer
                                                (starting-diagrams selected))]
                               (.setToolTipText start (start-tooltips selected))
                               (doseq [^javax.swing.JComponent c enhancer-only]
                                 (.setEnabled c enhancer?))))]
    (.setToolTipText iters
                     (str "<html>Upper bound on the conjugate gradient steps of the"
                          "<br>refinement.  Below about 30 concepts it converges long"
                          "<br>before the bound, so this costs nothing; above it, 100"
                          "<br>to 250 buys nearly all the improvement there is.</html>"))
    (.setToolTipText spring
                     (str "<html>Upper bound on the steps of the spring model that"
                          "<br>orders the irreducible elements."
                          "<br>Only used by the planarity enhancer.</html>"))
    (.setToolTipText draw "Lay the diagram out again with DimFlux, using the settings above.")
    (.setToolTipText project
                     (str "<html>In a doubly-additive diagram every node sits at the sum"
                          "<br>of one vector per irreducible object below it and per"
                          "<br>irreducible attribute above it, which is what fills the"
                          "<br>diagram with parallelograms and makes it easy to read."
                          "<br>Dragging a node by hand destroys that."
                          "<br><br>This button moves every node the least distance that"
                          "<br>restores it, leaving the diagram additive again.  It does"
                          "<br>not run the refinement, and it works on whatever is on"
                          "<br>screen, whether DimFlux drew it or not.</html>"))
    (update-options!)
    (listen start :action (fn [_] (update-options!)))
    (listen draw :action
            (fn [_]
              (update-layout-of-scene
               scn
               (dim-flux-layout (poset (get-layout-from-scene scn))
                                {:initial           (starting-diagrams
                                                     (.getSelectedItem start))
                                 :iterations        (int (.getValue iters))
                                 :spring-iterations (int (.getValue spring))}))
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
