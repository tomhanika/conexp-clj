;; Copyright ⓒ the conexp-clj developers; all rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.layouts.common
  "Implements common layout algorithm."
  (:use conexp.base
        conexp.math.algebra
        conexp.fca.lattices
        conexp.fca.posets
        conexp.layouts.util
        conexp.layouts.layered
        conexp.layouts.base)
  (:import [conexp.fca.posets Poset]
           [conexp.fca.lattices Lattice]))

;;; inf-irreducible additive layout

(defn placement-by-initials
  "Computes placement for all elements by positions of some initial
  nodes. Top element will be at top."
  [poset top placement]
  (let [ord (order poset),
        pos (fn pos [v]
              (get placement v
                   (reduce (fn [p w]
                             (if (ord v w)
                               (let [p-w (placement w)]
                                 [(+ (first p) (- (first p-w) (first top))),
                                  (+ (second p) (- (second p-w) (second top)))])
                               p))
                           top
                           (keys placement))))]
    (map-by-fn pos (base-set poset))))

(defmulti to-inf-additive-layout
  "Returns an infimum additive layout from given layout."
  (fn [layout] (type (poset layout))))

(defmethod to-inf-additive-layout Lattice
  ;; Returns an infimum additive layout from given layout, taking the
  ;; positions of the infimum irreducible elements as initial positions for
  ;; the resulting additive layout.
  ;; this is stupid, do it better!
  [layout]
  (let [lattice       (poset layout),
        old-positions (positions layout),
        top-pos       (old-positions (lattice-one lattice)),
        inf-irr       (set (inf-irreducibles layout)), 
        elements      (filter inf-irr (top-down-elements-in-layout layout))]
    (loop [positions (select-keys old-positions inf-irr),
           nodes     elements]
      (if (empty? nodes)
        (update-positions layout (placement-by-initials lattice top-pos positions))
        (let [next          (first nodes),
              [x-old y-old] (positions next),
              [x-new y-new] (reduce (fn [p w]
                                      (if (and ((order lattice) [next w])
                                               (not= next w))
                                        (let [p-w (positions w)]
                                          [(+ (first p) (- (first p-w) (first top-pos))),
                                           (+ (second p) (- (second p-w) (second top-pos)))])
                                        p))
                                    top-pos
                                    (keys positions))]
          (recur (assoc positions next
                        [x-old (min y-old y-new)])
                 (rest nodes)))))))

(defmethod to-inf-additive-layout Poset
  ;; Returns an infimum additive layout from given poset layout.
  [layout]
  (layout-fn-on-poset to-inf-additive-layout layout))

;;;

(defn layout-by-placement
  "Computes additive layout of ordered set by given positions of the keys
  of placement. The values of placement should be the positions of the
  corresponding keys. Top element will be at top."
  [poset top placement]
  (make-layout-nc poset
                  (placement-by-initials poset top placement)
                  (edges poset)))

;;; Valued layout stuff

(defn to-valued-layout
  [layout val-fn]
  (let [poset   (poset layout)
        elements  (base-set poset)]
    (update-valuations layout val-fn)))

;;;

nil
