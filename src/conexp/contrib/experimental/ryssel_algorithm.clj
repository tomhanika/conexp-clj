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

(defn- covers? [sets base-set]
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

(defn- redundant? [base-set cover count]
  (exists [set cover]
    (forall [x set]
      (=> (contains? base-set x)
          (<= 2 (get count x))))))

(defn- minimum-covers [base-set sets]
  (let [sets    (sort #(>= (count %1) (count %2)) sets),
        result  (atom []),
        search  (fn search [rest-base-set current-cover cover-count sets]
                  (cond
                   (redundant? base-set current-cover cover-count)
                   nil,

                   (empty? rest-base-set)
                   (swap! result conj current-cover),

                   (empty? sets)
                   nil,

                   :else
                   (when (covers? sets rest-base-set)
                     (search (difference rest-base-set (first sets))
                             (conj current-cover (first sets))
                             (reduce! (fn [map x]
                                        (if (contains? base-set x)
                                          (assoc! map x (inc (get map x)))
                                          map))
                                      cover-count
                                      (first sets))
                             (rest sets))
                     (search rest-base-set
                             current-cover
                             cover-count
                             (rest sets)))))]
    (search base-set
            #{}
            (map-by-fn (constantly 0) base-set)
            sets)
    @result))


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
  (let [object-covers (minimum-covers (difference base-set A)
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
                                       n (gens N),
                                       :when (not= m n)]
                                   (make-implication #{m} #{n}))),
              M-down-A     (set-of V | V M :when (proper-subset? V A)),
              candidates   (set-of U | U (disj M A),
                                       :when (not (exists [V M-down-A]
                                                    (subset? (intersection U A) V)))),
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
