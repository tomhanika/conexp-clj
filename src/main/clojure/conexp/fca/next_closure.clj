(ns conexp.fca.next-closure
  (:require [clojure.math.combinatorics :refer [combinations]]
            [conexp.base :refer [difference
                                 illegal-argument
                                 subset?
                                 topological-sort
                                 union]]
            [conexp.fca.contexts :refer [attributes
                                         context-attribute-closure
                                         objects
                                         object-derivation]]
            [conexp.fca.lattices :refer [inf
                                         lattice-base-set
                                         lattice-order
                                         sup]]))

(defn- implication-operator
  "Converts a set of implications into an implication-operator function."
  [implications]
  (fn implication-operator [intent]
    (let [impl-function (fn [x] 
                          (apply union x
                                 (map second
                                      (filter #(subset? (first %) x) implications))))]
      (loop [old-conclusion intent
             new-conclusion (impl-function old-conclusion)]
        (if (= old-conclusion new-conclusion)
          old-conclusion
          (recur new-conclusion (impl-function new-conclusion)))))))

(defn- join-operator
  "Join operator of a given lattice for an input set of arbitrary length."
  [lattice]
  (fn [concept-set]
    (if (= 0 (count concept-set))
      ;; bottom element
      (reduce (inf lattice) (lattice-base-set lattice))
      (if (= 1 (count concept-set))
        (first concept-set)
        (reduce (sup lattice) concept-set)))))

(defn- not>=t-operator
  [lattice t]
  (fn [concept]
    (not ((lattice-order lattice) t concept))))

(defn- t-simplex-operator
  [lattice t]
  (fn [concept-set]
    ((not>=t-operator lattice t) ((join-operator lattice) concept-set))))

(defn- pairwise-subsets?
  [attribute-sets]
  (let [attribute-set-combinations (combinations attribute-sets 2)]
    (every? 
     #(or (subset? (first %) (second %))
          (subset? (second %) (first %)))
     attribute-set-combinations)))

(defn- ordinal-operator
  [context]
  (fn [object-set]
    (if (< (count object-set) 2)
      true
      (let [attribute-sets (mapv #(object-derivation context %) (mapv #(hash-set %) object-set))]
        (pairwise-subsets? attribute-sets)))))

(defn- next-closure-with-operator
  "Computes the next closure of an element A given a sorted base-set and closure operator."
  [sorted-base-set operator A]
  (loop [A A
         M sorted-base-set
         i (last M)]
    (if (contains? A i)
      (let [A (disj A i)
            M (remove #{i} M)
            i (last M)]
        (recur A M i))
      (let [B (operator (conj A i))
            X (difference B A)
            elements-smaller-than-i (filter #(< (.indexOf sorted-base-set %)
                                                (.indexOf sorted-base-set i)) X)]
        (if (empty? elements-smaller-than-i)
          B
          (let [M (remove #{i} M)
                i (last M)]
            (recur A M i)))))))

(defn operator-pseudo-intents
  "Compute the pseudo intents of a given closure operator and base-set."
  [sorted-base-set closure-operator]
  (let [base-set (set sorted-base-set)]
    (loop [implications #{}
           A #{}
           simplicial-complex #{}]
      (if (= A base-set)
        [(set (map first implications)) simplicial-complex]
        (if (closure-operator A)
          (recur implications
                 (next-closure-with-operator
                  sorted-base-set (implication-operator implications) A)
                 (conj simplicial-complex A))
          (if (= ((implication-operator implications) A) base-set)
            [(set (map first implications)) simplicial-complex]
            (let [new-implications
                  (conj implications [A base-set])]
              (recur new-implications
                     (next-closure-with-operator
                      sorted-base-set (implication-operator new-implications) A)
                     simplicial-complex))))))))

(defn t-simplex-pseudo-intents
  "Compute the pseudo intents of a given closure operator and lattice."
  [lattice t]
  (let [sorted-base-set (topological-sort (lattice-order lattice)
                                          (lattice-base-set lattice))
        closure-operator (t-simplex-operator lattice t)]
    (operator-pseudo-intents sorted-base-set closure-operator)))

(defn ordinal-motifs-pseudo-intents-ordinal-scale
  "Compute ordinal motifs for ordinal scale over next closure algorithm."
  [context]
  (let [sorted-base-set (sort (objects context))]
    (let [closure-operator (ordinal-operator context)]
      (operator-pseudo-intents sorted-base-set closure-operator))))
