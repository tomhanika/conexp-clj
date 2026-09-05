;; Copyright ⓒ the conexp-clj developers; all rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.gui.draw.control.layout-controls-test
  "Tests for the DimDraw and DimFlux controls of the lattice editor.  Both offer
  a mode, options that only some modes use, and a tooltip per mode, so both are
  checked the same way: the menu has to agree with what the layout accepts, and
  the options have to follow the selection."
  (:require [clojure.test :refer :all]
            [conexp.fca.contexts :refer [make-context-from-matrix]]
            [conexp.fca.lattices :refer [concept-lattice]]
            [conexp.gui.draw.control.dim-draw :refer [dimdraw]]
            [conexp.gui.draw.control.dim-flux :refer [dimflux]]
            [conexp.layouts.base :as lay]
            [conexp.layouts.dim-draw :refer [dim-draw-layout]]
            [conexp.layouts.dim-flux :refer [dim-flux-layout]])
  (:import [javax.swing BoxLayout JComboBox JComponent JPanel JSpinner]))

(def ^:private starting-diagrams
  @#'conexp.gui.draw.control.dim-flux/starting-diagrams)

(def ^:private start-tooltips
  @#'conexp.gui.draw.control.dim-flux/start-tooltips)

(def ^:private extension-modes
  @#'conexp.gui.draw.control.dim-draw/extension-modes)

(def ^:private mode-tooltips
  @#'conexp.gui.draw.control.dim-draw/mode-tooltips)

(def ^:private test-lattice
  (concept-lattice
   (make-context-from-matrix ["Ceres" "Pluto" "Eris" "Haumea" "Makemake"]
                             ["asteroid-belt" "kuiper-belt" "scattered-disc" "moons"]
                             [1 0 0 0
                              0 1 0 1
                              0 0 1 1
                              0 1 0 1
                              0 1 0 0])))

(defn- line-diagram?
  [layout]
  (let [pos (lay/positions layout)]
    (every? (fn [[a b]] (< (second (pos a)) (second (pos b))))
            (lay/connections layout))))

(defn- install
  "Installs a control on a panel and returns `[panel combo-box spinners]`.  The
  frame and the scene are only touched from the button callbacks, so building
  the widgets needs neither."
  [installer]
  (let [panel (JPanel.)]
    (.setLayout panel (BoxLayout. panel BoxLayout/Y_AXIS))
    (installer nil nil panel)
    (let [components (seq (.getComponents panel))]
      [panel
       (first (filter #(instance? JComboBox %) components))
       (vec (filter #(instance? JSpinner %) components))])))

(defn- enabled-per-mode
  "For every entry of the mode menu, `[label enabled-flags-of-the-spinners]`."
  [^JComboBox combo spinners]
  (vec (for [i (range (.getItemCount combo))]
         (do (.setSelectedIndex combo i)
             [(.getSelectedItem combo)
              (mapv #(.isEnabled ^JComponent %) spinners)]))))

;;; The menus agree with the layouts

(deftest test-dim-flux-menu-matches-the-layout
  (testing "the editor offers every starting diagram the layout knows"
    (is (= #{:dim-draw :greedy :planarity-enhancer :layered}
           (set (vals starting-diagrams)))))
  (testing "and offers the published algorithm first, as the default selection"
    (is (= :dim-draw (second (first starting-diagrams)))))
  (testing "each entry is a keyword the layout actually accepts, which a typo in
            either place would break"
    (doseq [[label initial] starting-diagrams]
      (let [layout (dim-flux-layout test-lattice {:initial initial :iterations 10})]
        (is (seq (lay/positions layout)) (str label " draws nothing"))
        (is (line-diagram? layout) (str label " does not give a line diagram"))))))

(deftest test-dim-draw-menu-matches-the-layout
  (testing "the editor offers the three two-dimension extensions"
    (is (= #{:exact :greedy :genetic} (set (vals extension-modes)))))
  (testing "and offers the exact one first, as the default selection"
    (is (= :exact (second (first extension-modes)))))
  (testing "each entry draws, with the arguments the control passes on"
    (doseq [[label mode] extension-modes]
      (let [layout (case mode
                     :greedy  (dim-draw-layout test-lattice "greedy")
                     :genetic (dim-draw-layout test-lattice "genetic" 5 5)
                     (dim-draw-layout test-lattice))]
        (is (seq (lay/positions layout)) (str label " draws nothing"))
        (is (line-diagram? layout) (str label " does not give a line diagram"))))))

;;; Every mode explains itself

(deftest test-every-mode-has-a-tooltip
  (testing "DimFlux"
    (is (= (set (keys starting-diagrams)) (set (keys start-tooltips))))
    (is (every? seq (vals start-tooltips))))
  (testing "DimDraw"
    (is (= (set (keys extension-modes)) (set (keys mode-tooltips))))
    (is (every? seq (vals mode-tooltips))))
  (testing "the cheap starting diagrams say so, since that is why one picks them"
    (doseq [label ["DimDraw (greedy)" "Planarity enhancer" "Layered"]]
      (is (re-find #"(?i)fast" (start-tooltips label))
          (str label " does not mention that it is fast")))))

;;; The options follow the selected mode

(deftest test-options-follow-the-selected-mode
  (testing "DimDraw enables the population settings only for the genetic search"
    (let [[_ combo spinners] (install dimdraw)]
      (is (= 2 (count spinners)))
      (is (= [["Exact"   [false false]]
              ["Greedy"  [false false]]
              ["Genetic" [true  true]]]
             (enabled-per-mode combo spinners)))))
  (testing "DimFlux always allows bounding the refinement, and enables the spring
            steps only for the planarity enhancer that uses them"
    (let [[_ combo spinners] (install dimflux)]
      (is (= 2 (count spinners)))
      (is (= [["DimDraw (exact)"    [true false]]
              ["DimDraw (greedy)"   [true false]]
              ["Planarity enhancer" [true true]]
              ["Layered"            [true false]]]
             (enabled-per-mode combo spinners))))))

(deftest test-selecting-a-mode-updates-its-tooltip
  (doseq [[installer tooltips] [[dimdraw mode-tooltips] [dimflux start-tooltips]]]
    (let [[_ ^JComboBox combo _] (install installer)]
      (dotimes [i (.getItemCount combo)]
        (.setSelectedIndex combo i)
        (is (= (tooltips (.getSelectedItem combo)) (.getToolTipText combo)))))))

;;;

nil
