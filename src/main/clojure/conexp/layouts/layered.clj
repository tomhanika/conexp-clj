;; Copyright ⓒ the conexp-clj developers; all rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.layouts.layered
  "Layered lattice layouts."
  (:use conexp.base
        conexp.layouts.util
        conexp.layouts.base
        conexp.fca.lattices))

;;; Simple Layered Layout

(defn- layer-coordinates
  "Assigns coordinates to a given layer such that it is centerer
  around 0 at height given by number."
  [number layer]
  (let [start  (double (- (/ (- (count layer) 1) 2))),
        number (double number)]
    (interleave layer
                (map #(vector % number)
                     (iterate inc start)))))

(defn simple-layered-layout
  "Simple layered layout for lattice visualization."
  [lattice]
  (make-layout-nc lattice
                  (apply hash-map
                         (mapcat layer-coordinates
                                 (iterate inc 0)
                                 (layers lattice)))
                  (edges lattice)))

(defn as-chain
  "Returns the layout of lattice as a simple chain."
  [lattice]
  (make-layout-nc lattice
                  (into {}
                        (mapcat (fn [i layer]
                                  (map (fn [x] [x [0, i]])
                                       layer))
                                (iterate inc 0)
                                (layers lattice)))
                  (edges lattice)))

;;;

nil
