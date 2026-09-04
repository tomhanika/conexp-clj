;; Copyright ⓒ the conexp-clj developers; all rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.gui.draw.control.dim-draw
  (:require [conexp.gui.draw.control.util :refer :all]
            [conexp.gui.draw.scene-layouts :refer :all]
            [conexp.gui.draw.scenes :refer :all]
            [conexp.gui.util :refer :all]
            [conexp.layouts.base :refer :all]
            [conexp.layouts.dim-draw :refer :all]
            [seesaw.core :refer [listen]])
  (:import [javax.swing JButton JComboBox JLabel JSpinner]))

;;; DimDraw layout

(def ^:private extension-modes
  "How DimDraw extends the order relation until it has dimension two, in the
  order they are offered.  The first one is the exact algorithm and the default."
  (array-map "Exact"   :exact
             "Greedy"  :greedy
             "Genetic" :genetic))

(def ^:private mode-tooltips
  {"Exact"
   (str "<html>Smallest possible extension of the order to dimension two,"
        "<br>found with a SAT solver.  Gives the best diagrams, but the cost"
        "<br>grows very steeply: roughly a second at 16 concepts and half a"
        "<br>minute at 23.</html>")
   "Greedy"
   (str "<html>Drops vertices of the conflict graph greedily until it becomes"
        "<br>bipartite, in random order.  Far faster than Exact, but the"
        "<br>extension is no longer minimal, so the diagram is usually worse."
        "<br>Results vary between runs.</html>")
   "Genetic"
   (str "<html>Searches for a small extension with a genetic algorithm."
        "<br>Slower than Greedy and also random.  Its population size and"
        "<br>number of generations are set below.</html>")})

(defn dimdraw
  "Installs DimDraw Layout control."
  [frame scn buttons]
  (make-label buttons "Extension")
  (let [^JComboBox mode    (make-combo-box buttons (keys extension-modes))
        ^JLabel   l1       (make-label buttons "Individuums:")
        ^JSpinner ind      (make-spinner buttons 0 1000 20 1)
        ^JLabel   l2       (make-label buttons "Generations:")
        ^JSpinner gen      (make-spinner buttons 0 1000 20 1)
        ^JButton  draw     (make-button buttons "Redraw")
        genetic-only       [l1 ind l2 gen]
        update-options!    (fn []
                             (let [selected (.getSelectedItem mode)
                                   genetic? (= :genetic (extension-modes selected))]
                               (.setToolTipText mode (mode-tooltips selected))
                               (doseq [^javax.swing.JComponent c genetic-only]
                                 (.setEnabled c genetic?))))]
    (.setToolTipText ind (str "<html>How many candidate extensions the genetic"
                              "<br>search keeps in each generation."
                              "<br>Only used by the Genetic extension.</html>"))
    (.setToolTipText gen (str "<html>How many generations the genetic search runs."
                              "<br>Only used by the Genetic extension.</html>"))
    (.setToolTipText draw "Lay the diagram out again with DimDraw, using the settings above.")
    (update-options!)
    (listen mode :action (fn [_] (update-options!)))
    (listen draw :action
            (fn [_]
              (let [poset (poset (get-layout-from-scene scn))
                    layout (case (extension-modes (.getSelectedItem mode))
                             :greedy  (dim-draw-layout poset "greedy")
                             :genetic (dim-draw-layout poset "genetic"
                                                       (int (.getValue ind))
                                                       (int (.getValue gen)))
                             (dim-draw-layout poset))]
                (update-layout-of-scene scn layout)
                (call-scene-hook scn :update-grid)
                (fit-scene-to-layout scn))))))

;;;

nil
