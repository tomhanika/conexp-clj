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

;;; Searching for minimum covers

(defn- covers? [base-set sets]
  (if (empty? base-set)
    true
    (loop [rest (transient base-set),
           sets sets]
      (if-not sets
        false
        (let [new-rest (reduce disj! rest (first sets))]
          (if (zero? (count new-rest))
            true
            (recur new-rest (next sets))))))))

(defn- redundant? [cover]
  (exists [set cover]
    (forall [x set]
      (exists [other-set (disj cover set)]
        (contains? other-set x)))))

(defn- minimum-covers [base-set sets]
  (let [drop    (memoize drop),
        sets    (vec (sort #(>= (count %1) (count %2)) sets)),
        nr-sets (count sets),
        result  (atom (transient [])),
        search  (fn search [rest-base-set current-cover i]
                  (cond
                   (redundant? current-cover)  nil,
                   (empty? rest-base-set)      (swap! result conj! current-cover),
                   (>= i nr-sets)              nil,
                   :else
                   (when (covers? rest-base-set (drop i sets))
                     (when (exists [x (sets i)]
                             (contains? rest-base-set x))
                       (search (difference rest-base-set (sets i))
                               (conj current-cover (sets i))
                               (inc i)))
                     (search rest-base-set current-cover (inc i)))))]
    (search base-set #{} 0)
    (persistent! @result)))


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
  (let [candidates    (difference candidates
                                  (set-of (intersection X Y) | X candidates, Y candidates
                                                               :when (and (not (subset? X Y))
                                                                          (not (subset? Y X))))),
        object-covers (minimum-covers (difference base-set A)
                                      (set-of (difference base-set N) | N candidates))]
    (map (fn [cover]
           (map #(difference base-set %) cover))
         object-covers)))

(defn ryssel-base
  "Returns the set of implications computed by Ryssels Algorithm."
  [ctx]
  (let [oprime (memoize #(object-derivation ctx %)),
        gens   (reduce! (fn [map x]     ;generating elements of attribute extents
                          (let [extent (attribute-derivation ctx #{x})]
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
                                 (set-of (make-implication #{m} #{n})
                                         [N M
                                          :when (subset? A N),
                                          m (gens A),
                                          n (gens N),
                                          :when (not= m n)])),
              candidates   (set-of U
                                   [U M,
                                    :when (not (exists [V M]
                                                 (and (subset? (intersection U A) V)
                                                      (proper-subset? V A))))]),
              candidates   (disj candidates A),
              covers       (cover (objects ctx) candidates A),
              B            (oprime A),
              implications (union implications
                                  (set-of (make-implication premise B)
                                          [X covers,
                                           :let [premise (set-of m | Y X, m (gens Y))]]))]
          (recur implications (rest extents)))))))

;;;

nil
