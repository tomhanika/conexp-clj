;; Copyright ⓒ the conexp-clj developers; all rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.contrib.draw.control.parameters
  (:use [conexp.math.util                  :only (with-doubles)]
        [conexp.layouts                    :only (standard-layout,
                                                  inf-additive-layout)]
        [conexp.layouts.util               :only (scale-layout)]
        [conexp.layouts.base               :only (lattice)]
        [conexp.layouts.layered            :only (simple-layered-layout,
                                                  as-chain)]
        conexp.contrib.gui.util
        conexp.contrib.draw.scenes
        conexp.contrib.draw.scene-layouts
        conexp.contrib.draw.nodes-and-connections
        conexp.contrib.draw.control.util)
  (:use seesaw.core)
  (:import [javax.swing JButton JTextField JComboBox]))

;;;

(declare single-move-mode, ideal-move-mode, filter-move-mode, chain-move-mode,
         infimum-additive-move-mode, supremum-additive-move-mode)

;;;

(defn change-parameters
  "Installs parameter list which influences lattice drawing."
  [_ scn buttons]
  ;; node radius
  (let [^JTextField
        node-radius (make-labeled-text-field buttons
                                             "radius"
                                             (str default-node-radius))]
    (add-scene-callback scn :image-changed
                        (fn []
                          (let [new-radius (Double/parseDouble (.getText node-radius)),
                                length     (min (scene-height scn) (scene-width scn))]
                            (do-nodes [n scn]
                              (set-node-radius! n (/ (* length new-radius) 400))))))
    (listen node-radius :action
            (fn [_] (call-scene-hook scn :image-changed))))

  (make-padding buttons)

  ;; labels
  (let [^JButton label-toggler (make-button buttons "No Labels")]
    (show-labels scn false)
    (listen label-toggler :action
            (fn [_]
              (if (= "Labels" (.getText label-toggler))
                (do
                  (show-labels scn false)
                  (.setText label-toggler "No Labels"))
                (do
                  (show-labels scn true)
                  (.setText label-toggler "Labels")))
              (redraw-scene scn))))

  ;; layouts
  (let [layouts {"standard"     standard-layout,
                 "inf-add"      inf-additive-layout,
                 "simple-layer" simple-layered-layout,
                 "as-chain"     as-chain},
        ^JButton fit (make-button buttons "Fit"),
        ^JComboBox combo-box (make-combo-box buttons (keys layouts))]
    (listen fit :action
            (fn [_]
              (fit-scene-to-layout scn)))
    (listen combo-box :action
            (fn [evt]
              (let [selected  (.getSelectedItem ^JComboBox (.getSource ^java.awt.event.ActionEvent evt)),
                    layout-fn (get layouts selected),
                    layout    (scale-layout [0.0 0.0]
                                            [100.0 100.0]
                                            (layout-fn (lattice (get-layout-from-scene scn))))]
                (update-layout-of-scene scn layout)
                (fit-scene-to-layout scn layout)))))

  ;; move mode
  (let [move-modes {"single" (single-move-mode),
                    "ideal"  (ideal-move-mode),
                    "filter" (filter-move-mode),
                    "chain"  (chain-move-mode),
                    "inf"    (infimum-additive-move-mode),
                    "sup"    (supremum-additive-move-mode)}
        ^JComboBox combo-box (make-combo-box buttons (keys move-modes)),
        current-move-mode (atom (move-modes "single"))]
    (add-scene-callback scn :move-drag
                        (fn [node dx dy]
                          (@current-move-mode node dx dy)))
    (listen combo-box :action
            (fn [evt]
              (let [selected (.getSelectedItem
                              ^JComboBox (.getSource ^java.awt.event.ActionEvent evt)),
                    move-mode (get move-modes selected)]
                (reset! current-move-mode move-mode)))))

  nil)

;;; Move modes

(defn- single-move-mode
  "Moves the single node only."
  []
  (fn [node dx dy]
    nil))

(defn- neighbor-move-mode
  "Moves nodes neighbored to node by [dx dy]."
  [neighbors]
  (fn [node dx dy]
    (doseq [n (neighbors node)]
      (move-node-by n dx dy))))

(defn- ideal-move-mode
  "Moves all nodes below the current node."
  []
  (neighbor-move-mode (memoize all-nodes-below)))

(defn- filter-move-mode
  "Moves all nodes above the current node."
  []
  (neighbor-move-mode (memoize all-nodes-above)))

(defn- chain-move-mode
  "Combined ideal and filter move mode."
  []
  (let [ideal (ideal-move-mode),
        filter (filter-move-mode)]
    (fn [node dx dy]
      (ideal node dx dy)
      (filter node dx dy))))

(defn- additive-move-mode
  "Abstract move mode for moving nodes according to additive
  influence."
  [influenced-nodes]
  (fn [node dx dy]
    (doseq [[n weight] (influenced-nodes node)]
      (with-doubles [dx dy weight]
        (move-node-by n (* dx weight) (* dy weight))))))

(defn- infimum-additive-move-mode
  "Moves all nodes infimum-additively with node."
  []
  (additive-move-mode (memoize all-inf-add-influenced-nodes)))

(defn- supremum-additive-move-mode
  "Moves all nodes supremum-additively with node."
  []
  (additive-move-mode (memoize all-sup-add-influenced-nodes)))

;;;

nil
