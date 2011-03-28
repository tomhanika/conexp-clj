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
  (subset? base-set (reduce union #{} sets)))

(defn- redundant? [cover]
  (exists [set cover]
    (covers? set (disj cover set))))

(defn- minimum-covers [base-set sets]
  (if-not (covers? base-set sets)
    []
    (let [sets   (vec (sort #(>= (count %1) (count %2)) sets)),
          result (atom (transient [])),
          search (fn search [rest-base-set current-cover i]
                   (cond
                    (redundant? current-cover)
                    nil,
                    (covers? base-set current-cover)
                    (swap! result conj! current-cover),
                    (>= i (count sets))
                    nil,
                    :else (do
                            (search (difference base-set (sets i))
                                    (conj current-cover (sets i))
                                    (inc i))
                            (when (covers? rest-base-set (drop (inc i) sets))
                              (search rest-base-set current-cover (inc i))))))]
      (search base-set #{} 0)
      (persistent! @result))))
    
(defn- cover [ctx candidates A]
  (let [base-set      (objects ctx),
        candidates    (difference candidates
                                  (set-of (intersection X Y) | X candidates, Y candidates
                                                               :when (and (not (subset? X Y))
                                                                          (not (subset? Y X))))),
        object-covers (minimum-covers (difference base-set A)
                                      (set-of (difference base-set N) | N candidates))]
    (map (fn [cover]
           (map #(difference base-set %) cover))
         object-covers)))

(defn- collapse-equal-premises [implications]
  (let [impl-map (reduce! (fn [map implication]
                            (assoc! map (premise implication)
                                    (into (get map (premise implication) #{})
                                          (conclusion implication))))
                          {}
                          implications)]
    (set-of (make-implication (pair 0) (pair 1)) | pair impl-map)))

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
        (collapse-equal-premises implications)
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
