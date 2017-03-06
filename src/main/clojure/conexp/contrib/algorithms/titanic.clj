;; Copyright ⓒ the conexp-clj developers; all rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.contrib.algorithms.titanic
  "Implements the TITANIC algorithm. Note that this implementation is not tuned for speed
  but for flexibility."
  (:use [conexp.base
         :only (union difference exists subset? set-of forall map-by-fn)]
        [conexp.fca.contexts
         :only (objects object-derivation attributes attribute-derivation)]))

;;; Pragmatics

(defn- minimum
  "Computes the minimum of weights in a given sequence of
  weights. order must be a linear order on elements plus max-val and
  max-val must be the maximal element to be considered."
  [order max-val elements]
  (loop [current-min max-val,
         elements    elements]
    (if (empty? elements)
      current-min
      (recur (if (order (first elements) current-min)
               (first elements)
               current-min)
             (rest elements)))))

;;; TITANIC Implementation

(defn- titanic-closure
  "Computes the closure of X by the weights of its subsets. The
  following restrictions apply:

    - keys is the sequence of pairs of already computed key-sets
      together with their weights, ordered by their weights,
    - X is a member of keys.

  "
  [X X-weight base-set keys set-weight closure]
  (let [keys  (doall (map first (take-while #(not= (second %) X-weight) keys))),
        start (reduce #(union %1 (closure (disj X %2)))
                      X X)]
    (loop [Y        (transient start),
           elements (difference base-set start)]
      (if-not (empty? elements)
        (let [m    (first elements),
              X+m  (conj X m),
              add? (if-let [s (set-weight X+m)]
                     (= s X-weight)
                     (not (exists [K keys]
                            (and (contains? K m) (subset? K X+m)))))]
          (recur (if add? (conj! Y m) Y)
                 (rest elements)))
        (persistent! Y)))))

(defn- titanic-generate
  "Computes the next candidate set."
  [base-set key-set]
  (set-of next [A key-set,
                m (difference base-set A),
                :let [next (conj A m)]
                :when (forall [x A]
                        (contains? key-set (disj next x)))]))

(defn titanic
  "Implements the titanic algorithm. weigh must return for a
  collection of sets a hash-map mapping every set to its weight, with
  max-weight being the maximal weight possible. weight-order denotes
  the order on the weights. Note that this order has to be linear for
  this implementation to work."
  [base-set weigh max-weight weight-order]
  (loop [closure    {},
         set-weight (weigh [#{}]),
         key-set    #{[#{}, (set-weight #{})]}, ;keys are pairs of sets and weights here
         keys       key-set]
    (let [candidates   (titanic-generate base-set (set-of (first k) [k key-set])),
          set-weight   (into set-weight (weigh candidates)),
          closure      (into closure (map (fn [[X, X-weight]]
                                            [X (titanic-closure X X-weight base-set keys set-weight closure)])
                                          key-set)),
          next-key-set (set-of [X w] [X candidates,
                                      :let [w (set-weight X)]
                                      :when (not= w
                                                  ;;(subset-weight X)
                                                  (minimum weight-order max-weight
                                                           (map #(set-weight (disj X %)) X)))])]
      (if (seq next-key-set)            ;i.e. (not (empty? next-key-set))
        (recur closure,
               set-weight,
               next-key-set,
               (sort #(weight-order (second %1) (second %2))
                     (concat next-key-set keys)))
        (distinct (vals closure))))))

(defn titanic-keys
  "Nearly the same as titanic, but returns the key sets only."
  [base-set weigh max-weight weight-order]
  (loop [set-weight (weigh [#{}]),
         key-set    #{#{}},
         keys       [#{}]]
    (let [candidates   (titanic-generate base-set key-set),
          set-weight   (into set-weight (weigh candidates)),
          next-key-set (set-of X [X candidates,
                                  :when (not= (set-weight X)
                                              ;;(subset-weight X)
                                              (minimum weight-order max-weight
                                                       (map #(set-weight (disj X %)) X)))])]
      (if-not (empty? next-key-set)
        (recur set-weight
               next-key-set
               (concat next-key-set keys))
        keys))))

;;; Common use cases

(defn supports
  "Returns a function of one argument being a collection of sets of attributes,
  returning a hash-map from those sets to their supports in the given
  context, if they have at least minsupp elements. Otherwise they are
  associated with -1."
  [context minsupp]
  (let [num-of-objects (count (objects context)),
        minnum         (* minsupp num-of-objects),
        obj-deriv      (map-by-fn #(object-derivation context #{%}) (objects context))]
    (fn [att-sets]
      (map-by-fn
       (fn [att-set]
         (let [obj-count (count (filter #(subset? att-set (obj-deriv %))
                                        (objects context)))]
           (if (<= minnum obj-count)
             obj-count
             -1)))
       att-sets))))

(defn titanic-intents
  "Computes the intents of the given context via TITANIC."
  [context]
  (titanic (attributes context)
           (supports context 0)
           1.0
           <=))

(defn titanic-iceberg-intent-seq
  "Computes the iceberg intent seq for given context and minimal
  support minsupp via TITANIC."
  [context minsupp]
  (let [intents (titanic (attributes context)
                         (supports context minsupp)
                         1.0
                         <=)]
    (if (<= (* (count (objects context)) minsupp)
            (count (attribute-derivation context (attributes context))))
      intents
      (remove #{(attributes context)} intents))))

;;;

nil
