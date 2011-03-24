;; Copyright (c) Daniel Borchmann. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.contrib.experimental.ryssel_algorithm
  (:use conexp.base
        conexp.fca.contexts
        conexp.fca.implications))

(ns-doc "An implementation of Ryssels Algorithm")

;;;

(defn- covers? [base-set sets]
  (subset? base-set (reduce union sets)))

(defn- minimum-covers [base-set sets]
  (let [covers (if (covers? base-set sets)
                 #{sets}
                 #{})]
    (loop [covers         covers,
           minimum-covers #{}]
      (if (empty? covers)
        minimum-covers
        (let [next-cover     (first covers),
              smaller-covers (filter #(covers? base-set %)                                      
                                     (map #(disj next-cover %) next-cover))]
          (if (not-empty smaller-covers)
            (recur (into (disj covers next-cover)
                         smaller-covers)
                   minimum-covers)
            (let [covers (disj covers next-cover),
                  covers (set (mapcat #(if (subset? next-cover %)
                                         (filter (fn [X]
                                                   (covers? base-set X))
                                                 (map (fn [x] (disj % x))
                                                      next-cover))
                                         (list %))
                                      covers))]
              (recur covers
                     (conj minimum-covers next-cover)))))))))

(defn- cover [ctx candidates A]
  (let [base-set      (objects ctx),
        object-covers (minimum-covers (difference base-set A)
                                      (set-of (difference base-set N) | N candidates))]
    (map (fn [cover]
           (map #(difference base-set %) cover))
         object-covers)))

(defn ryssel-base
  "Returns the set of implications computed by Ryssels Algorithm."
  [ctx]
  (let [oprime (memoize #(object-derivation ctx %)),
        gens   (reduce! (fn [map x]
                          (let [extent (attribute-derivation ctx #{x})]
                            (assoc! map extent
                                    (conj (get map extent #{}) x))))
                        {}
                        (attributes ctx)),
        M      (set (keys gens))]
    (loop [implications #{},
           extents      M]
      (if (empty? extents)
        implications
        (let [A            (first extents),
              B            (oprime A),
              implications (into implications
                                 (set-of (make-implication #{m} (oprime N))
                                         [N (disj M A)
                                          :when (subset? A N),
                                          m (gens A)])),
              candidates   (set-of U
                                   [U M,
                                    :when (not (exists [V M]
                                                 (and (subset? (intersection U A) V)
                                                      (proper-subset? V A))))]),
              candidates   (disj candidates A),
              covers       (cover ctx candidates A),
              implications (union implications
                                  (set-of (make-implication premise B)
                                          [X covers,
                                           :let [premise (set-of m | Y X, m (gens Y))]
                                           :when (not (subset? B premise))]))]
          (recur implications (rest extents)))))))

;;;

nil
