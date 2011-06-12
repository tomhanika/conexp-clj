;; Copyright (c) Daniel Borchmann. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.base)

(load "util")

(ns-doc "Basic definitions for conexp-clj.")

;;; Very Basic Set Theory

(defn cross-product
  "Returns cross product of set-1 and set-2."
  [& sets]
  (if (empty? sets)
    #{[]}
    (set-of (conj t x) [t (apply cross-product (butlast sets))
                        x (last sets)])))

(defn disjoint-union
  "Computes the disjoint union of sets by joining the cross-products of the
  sets with natural numbers."
  [& sets]
  (apply union (map #(cross-product %1 #{%2}) sets (iterate inc 0))))

(defn set-of-range
  "Returns a set of all numbers from start upto (but not including) end,
  by step if provided."
  ([end]
     (set-of-range 0 end 1))
  ([start end]
     (set-of-range start end 1))
  ([start end step]
     (set (range start end step))))

(defn to-set
  "Converts given argument «thing» to a set. If it is a number,
  returns the set {0, ..., thing-1}. If it is a collection,
  returns (set thing). Otherwise raises an error."
  [thing]
  (cond
   (integer? thing) (set-of-range thing),
   (coll? thing)    (set thing),
   :else            (illegal-argument "Cannot create set from " thing)))


;;; Next Closure

(defn lectic-<_i
  "Implements lectic < at position i. The basic order is given by the ordering
  of base which is interpreted as increasing order.

  A and B have to be sets."
  [base i A B]
  (and (contains? B i) (not (contains? A i)) ; A and B will always be sets
       (loop [elements base]
         (let [j (first elements)]
           (if (= j i)
             true
             (if (identical? (contains? B j) (contains? A j))
               (recur (rest elements))
               false))))))

(defn lectic-<
  "Implements lectic ordering. The basic order is given by the ordering of base
  which is interpreted as increasing order."
  [base A B]
  (exists [i base] (lectic-<_i base i A B)))

(defn next-closed-set-in-family
  "Computes next closed set as with next-closed-set, which is in the
  family F of all closed sets satisfing predicate. predicate has to
  satisfy the condition

    A in F and i in base ==> clop(A union {1, ..., i-1}) in F.
  "
  [predicate base clop A]
  (loop [i-s (reverse base),
         A   (set A)]
    (if (empty? i-s)
      nil
      (let [i (first i-s)]
        (if (contains? A i)
          (recur (rest i-s) (disj A i))
          (let [clop-A (clop (conj A i))]
            (if (and (lectic-<_i base i A clop-A)
                     (predicate clop-A))
              clop-A
              (recur (rest i-s) A))))))))

(defn improve-basic-order
  "Improves basic order on the sequence base, where the closure operator
  clop operates on."
  [base clop]
  (let [base (seq base),
        clop (memoize clop)]
    (sort (fn [x y]
            (or (subset? (clop #{y}) (clop #{x}))
                (and (not (subset? (clop #{x}) (clop #{y})))
                     (lectic-< base (clop #{y}) (clop #{x})))))
          base)))

(defn all-closed-sets-in-family
  "Computes all closed sets of a given closure operator on a given set
  base contained in the family described by predicate. See
  documentation of next-closed-set-in-family for more details. Uses
  initial as first closed set if supplied."
  ([predicate base clop]
     (all-closed-sets-in-family predicate base clop #{}))
  ([predicate base clop initial]
     (let [base (if (set? base) (improve-basic-order base clop) base),
           initial (clop initial),
           start   (if (predicate initial)
                     initial
                     (next-closed-set-in-family predicate base clop initial)),
           runner  (fn runner [X]
                     (lazy-seq
                      (if (nil? X)
                        nil
                        (cons X (runner (next-closed-set-in-family predicate base clop X))))))]
         (runner initial))))

(defn next-closed-set
  "Computes next closed set of the closure operator clop after A with
  the Next Closure algorithm. The order of elements in base,
  interpreted as increasing, is taken to be the basic order of the
  elements."
  [base clop A]
  (next-closed-set-in-family (constantly true) base clop A))

(defn all-closed-sets
  "Computes all closed sets of a given closure operator on a given
  set. Uses initial as first closed set if supplied."
  ([base clop]
     (all-closed-sets base clop #{}))
  ([base clop initial]
     (all-closed-sets-in-family (constantly true) base clop initial)))

;;; Common Math Algorithms

(defn subsets
  "Returns all subsets of set."
  [set]
  (all-closed-sets set identity))

(defn transitive-closure
  "Computes transitive closure of a given set of pairs."
  [pairs]
  (let [pairs  (set pairs),
        runner (fn runner [new old]
                 (if (= new old)
                   new
                   (recur (union new
                                 (set-of [x y]
                                         [[x z_1] (difference new old)
                                          [z_2 y] pairs
                                          :when (= z_1 z_2)]))
                          new)))]
    (runner pairs #{})))

(defn reflexive-transitive-closure
  "Computes the reflexive, transitive closure of a given set of pairs
  on base-set."
  [base-set pairs]
  (transitive-closure (union (set pairs)
                             (set-of [x x] [x base-set]))))

(defn transitive-reduction
  "Returns for a set of pairs its transitive reduction. Alternatively,
  the relation can be given as a base set and a predicate p which
  returns true in (p x y) iff [x y] is in the relation in question.

  Note that if the relation given is not acyclic, the transitive
  closure of the reduction may not yield the transitive closure of the
  original relation anymore, since the reduction itself can be empty."
  ([pairs]
     (let [result (atom (transient #{}))]
       (doseq [[x y] pairs]
         (when (not (exists [[a b] pairs,
                             [c d] pairs]
                      (and (= a x)
                           (= b c)
                           (= d y))))
           (swap! result conj! [x y])))
       (persistent! @result)))
  ([base pred]
     (let [result (atom (transient #{}))]
       (doseq [x base, y base]
         (when (and (pred x y)
                    (not (exists [z base]
                           (and (not= x z)
                                (not= z y)
                                (pred x z)
                                (pred z y)))))
           (swap! result conj! [x y])))
       (persistent! @result))))

(defn graph-of-function?
  "Returns true iff relation is the graph of a function from source to target."
  [relation source target]
  (and (= (set-of x [[x y] relation]) source)
       (subset? (set-of y [[x y] relation]) target)
       (= (count source) (count relation))))

(defn minimal-generating-subsets
  "Given a set A and a closure operator clop returns all subsets B of
  A such that (= (clop A) (clop B))."
  [clop A]
  (let [clop-A (clop A)]
    (loop [left     [A],                ;yet to consider
           minimals []]                 ;minimal elements already found
      (if (empty? left)
        (distinct minimals)
        (let [next (first left)]
          (if (empty? next)
            (recur (rest left) (conj minimals next))
            (let [generating-subsets (set-of X [x next,
                                                :let [X (disj next x)]
                                                :when (= clop-A (clop X))])]
              (if (empty? generating-subsets)
                (recur (rest left) (conj minimals next))
                (recur (into (rest left) generating-subsets) minimals)))))))))

(defn partial-min
  "For a given partial order <= and given elements returns the minimal
  among them."
  [<= xs]
  (let [runner (fn runner [left minimals]
                 (if (empty? left)
                   minimals
                   (let [next (first left),
                         new-minimals (remove #(<= next %) minimals)]
                     (if (not= (count minimals) (count new-minimals))
                       (recur (rest left) (conj new-minimals next))
                       (if (some #(<= % next) minimals)
                         (recur (rest left) minimals)
                         (recur (rest left) (conj minimals next)))))))]
    (runner xs ())))

(defn partial-max
  "For a given partial order <= and given elements returns the maximal
  among them."
  [<= xs]
  (partial-min #(<= %2 %1) xs))

;;;

nil
