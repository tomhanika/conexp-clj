(ns conexp.fca.next-closure
  (:require [conexp.base :refer [difference
                                 subset?
                                 topological-sort
                                 union]]
            [conexp.fca.contexts :refer [attributes
                                         context-attribute-closure]]
            [conexp.fca.lattices :refer [inf
                                         lattice-base-set
                                         lattice-order
                                         sup]]))

;; Exercise 8 - Next Closure
(defn first-closure
  "Computes the closure of the empty set on a given operator."
  [operator base-set]
  (comment (assert (closure-operator? operator base-set) "Operator needs to be a closure operator."))
  (operator #{}))

(defn next-closure
  "Computes the next closure of a given set A on a given operator."
  [operator base-set A]
  (let [base-set (sort base-set)]
    (loop [A A
           M base-set
           i (last M)]
      (if (contains? A i)
        (let [A (disj A i)
              M (sort (disj (set M) i))
              i (last M)]
          (recur A M i))
        (let [B (operator (conj A i))
              X (difference B A)
              elements-smaller-than-i (filter #(< (.indexOf base-set %)
                                                  (.indexOf base-set i)) X)]
          (if (empty? elements-smaller-than-i)
            B
            (let [M (sort (disj (set M) i))
                  i (last M)]
              (recur A M i))))))))

(defn all-closures
  "Computes all closures of a given closure operator and set, or of a given context."
  ([context]
   (all-closures (partial context-attribute-closure context)
                    (attributes context)))
  ([operator base-set]
   (loop [A (first-closure operator base-set)
          closures [A]]
     (if (= A base-set)
       closures
       (let [A (next-closure operator base-set A)]
         (recur A (conj closures A)))))))

(defn implication-operator
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

(defn labor4-pseudo-intents
  "Compute the pseudo intents of a given context."
  ([context]
   (labor4-pseudo-intents (partial context-attribute-closure context)
                          (attributes context)))
  ([closure-operator base-set]
   (loop [implications #{}
          A #{}]
     (if (= A base-set)
       (set (map first implications))
       (if (= A (closure-operator A))
         (recur implications 
                (next-closure 
                 (implication-operator implications) base-set A))
         (if (= ((implication-operator implications) A) base-set)
           (set (map first implications))
           (let [new-implications 
                 (conj implications [A (closure-operator A)])]
             (recur new-implications 
                    (next-closure 
                     (implication-operator new-implications)
                     base-set A)))))))))

(defn join-operator
  "Join operator of a given lattice for an input set of arbitrary length."
  [lattice]
  (fn [concept-set]
    (if (= 0 (count concept-set))
      ;; bottom element
      (reduce (inf lattice) (lattice-base-set lattice))
      (if (= 1 (count concept-set))
        (first concept-set)
        (reduce (sup lattice) concept-set)))))

(defn not>=t-operator
  [lattice t]
  (fn [concept]
    (not ((lattice-order lattice) t concept))))

(defn t-simplex-operator
  [lattice t]
  (fn [concept-set]
    ((not>=t-operator lattice t) ((join-operator lattice) concept-set))))

(defn next-closure-for-simplices
  "Computes the next closure of a given set A on a given operator."
  [operator lattice A]
  (let [base-set (topological-sort (lattice-order lattice)
                                   (lattice-base-set lattice))]
    (loop [A A
           M base-set
           i (last M)]
      (if (contains? A i)
        (let [A (disj A i)
              M (remove #{i} M) 
              i (last M)]
          (recur A M i))
        (let [B (operator (conj A i))
              X (difference B A)
              elements-smaller-than-i (filter #(< (.indexOf base-set %)
                                                  (.indexOf base-set i)) X)]
          (if (empty? elements-smaller-than-i)
            B
            (let [M (remove #{i} M)
                  i (last M)]
              (recur A M i))))))))

(defn t-simplex-pseudo-intents
  "Compute the pseudo intents of a given closure operator and lattice."
  ([closure-operator lattice]
   (let [base-set (lattice-base-set lattice)]
     (loop [implications #{}
            A #{}
            t-simplex #{}]
       (if (= A base-set)
         [(set (map first implications)) t-simplex]
         (if (closure-operator A)
           (recur implications 
                  (next-closure-for-simplices 
                   (implication-operator implications) lattice A)
                  (conj t-simplex A))
           (if (= ((implication-operator implications) A) base-set)
             [(set (map first implications)) t-simplex]
             (let [new-implications 
                   (conj implications [A base-set])]
               (println A)
               (recur new-implications 
                      (next-closure-for-simplices 
                       (implication-operator new-implications)
                       lattice A)
                      t-simplex)))))))))
