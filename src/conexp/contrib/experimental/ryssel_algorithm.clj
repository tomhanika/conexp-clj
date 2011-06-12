;; Copyright (c) Daniel Borchmann. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.contrib.experimental.ryssel-algorithm
  (:use conexp.base
        conexp.fca.contexts
        conexp.fca.implications))

(ns-doc "An implementation of Ryssels Algorithm")

;;; Collapsing implications with equal premise

(defn- collapse-equal-premises [implications]
  (let [impl-map (reduce! (fn [map implication]
                            (assoc! map (premise implication)
                                    (into (get map (premise implication) #{})
                                          (conclusion implication))))
                          {}
                          implications)]
    (set-of (make-implication (pair 0) (pair 1)) | pair impl-map)))


;;; The actual algorithm

(defn- cover [base-set candidates A]
  (let [object-covers (minimum-set-covers
                       (difference base-set A)
                       (set-of (difference base-set N) | N candidates))]
    (map (fn [cover]
           (map #(difference base-set %) cover))
         object-covers)))

(defn ryssel-base
  "Returns the set of implications computed by Ryssels Algorithm."
  [ctx]
  (let [oprime (memoize #(oprime ctx %)),
        gens   (reduce! (fn [map x]     ;generating elements of attribute extents
                          (let [extent (aprime ctx #{x})]
                            (assoc! map extent
                                    (conj (get map extent #{}) x))))
                        {}
                        (attributes ctx)),
        M      (set (keys gens))]       ;all attribute extents
    (loop [implications #{},
           extents      M]
      (if (empty? extents)
        (collapse-equal-premises implications)
        (let [A            (first extents),
              implications (into implications
                                 (for [N M
                                       :when (subset? A N),
                                       m (gens A),
                                       :when (not= (gens N) #{m})]
                                   (make-implication #{m} (gens N)))),
              candidates   (set-of U | U (disj M A),
                                       :let [UcapA (intersection U A)]
                                       :when (not (exists [V M]
                                                    (and (proper-subset? V A)
                                                         (subset? UcapA V))))),
              candidates   (difference candidates
                                       (set-of (intersection X Y) | X candidates, Y candidates
                                                                    :when (and (not (subset? X Y))
                                                                               (not (subset? Y X))))),
              covers       (cover (objects ctx) candidates A),
              B            (oprime A),
              implications (into implications
                                 (for [X covers]
                                   (make-implication (set-of m | Y X, m (gens Y)) B)))]
          (recur implications (rest extents)))))))

;;;

nil
