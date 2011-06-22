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

;;;

(defn- cover [base-set candidates A]
  (let [object-covers (minimum-set-covers
                       (difference base-set A)
                       (set-of (difference base-set N) | N candidates))]
    (map (fn [cover]
           (map #(difference base-set %) cover))
         object-covers)))

(defn ryssel-base
  "Returns the implications computed by Ryssels Algorithm, as a lazy sequence."
  [ctx]
  (let [gens        (reduce! (fn [map x]      ;generating elements of attribute extents
                               (let [extent (aprime ctx #{x})]
                                 (assoc! map extent
                                         (conj (get map extent #{}) x))))
                             {}
                             (attributes ctx)),
        all-extents (set (keys gens)),        ;all attribute extents
        irr-extents (set-of (aprime ctx #{m}) ;attribute extents of irreducible attributes
                            | m (attributes (reduce-attributes ctx))),
        empty-prime (adprime ctx #{})]
    (->> (reduce into
                 (for [m (attributes ctx)
                       :when (not= (adprime ctx #{m})
                                   (conj empty-prime m))]
                   #{m})
                 (pmap (fn [A]
                         (let [candidates (set-of U | U (disj irr-extents A),
                                                      :let [U-cap-A (intersection U A)]
                                                      :when (not (exists [V all-extents]
                                                                   (and (proper-subset? V A)
                                                                        (subset? U-cap-A V))))),
                               covers     (cover (objects ctx) candidates A)]
                           (for [X covers]
                             (set-of m | Y X, m (gens Y)))))
                       all-extents))
         distinct
         (map #(make-implication % (adprime ctx %))))))

;;;

(defn stem-base-from-base*
  "For a given set of implications returns its stem-base."
  [implications]
  (loop [stem-base    #{},
         implications implications,
         all          (vec implications)]
    (if (empty? implications)
      stem-base
      (let [A->B         (first implications),
            implications (rest implications),
            all          (subvec all 1)
            A*           (close-under-implications all (premise A->B)),
            A*->B        (make-implication A* (conclusion A->B))]
        (if (not-empty (conclusion A*->B))
          (recur (conj stem-base A*->B)
                 implications
                 (conj all A*->B))
          (recur stem-base
                 implications
                 all))))))

;;;

nil
