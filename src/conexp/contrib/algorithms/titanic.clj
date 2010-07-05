;; Copyright (c) Daniel Borchmann. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.contrib.algorithms.titanic
  (:use conexp.main))

(ns-doc
 "Implements the TITANIC algorithm. Note that this implementation is
 not tuned for speed but for flexibility.")

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
  (let [keys (map first (take-while #(not= (second %) X-weight) keys))]
    (loop [Y        (apply union X (map #(closure (disj X %)) X)),
           elements (difference base-set Y)]
      (if-not (empty? elements)
        (let [m    (first elements),
              X+m  (conj X m),
              add? (if-let [s (set-weight X+m)]
                     (= s X-weight)
                     (not (first (for [K keys,
                                       :when (and (contains? K m)
                                                  (subset? K X+m))]
                                   K))))]
          (recur (if add? (conj Y m) Y)
                 (rest elements)))
        Y))))

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
               next-key-set
               (sort (fn [X Y]
                       (weight-order (second X) (second Y)))
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
        minnum (* minsupp num-of-objects)]
    (fn [coll]
      (into {} (for [atts coll]
                 [atts (let [num (count (attribute-derivation context atts))]
                         (if (< num minnum)
                           -1
                           (/ num num-of-objects)))])))))

(defn titanic-context-intents
  "Computes the intents of the given context via TITANIC."
  [context]
  (titanic (attributes context)
           (supports context 0)
           1.0
           <=))

(defn titanic-iceberg-intent-set
  "Computes the iceberg intent set for given context and minimal
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
