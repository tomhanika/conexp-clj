;; Copyright ⓒ the conexp-clj developers; all rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.fca.closure-systems
  "Next closure algorithm."
  (:require [conexp.base :refer :all]
            [clojure.set :refer [difference intersection union subset?]]))

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
            (if (= (clop #{y}) (clop #{x}))
              0
              (if (lectic-< base (clop #{y}) (clop #{x}))
                -1
                1)))
          base)))

(defn all-closed-sets-in-family
  "Computes all closed sets of a given closure operator on a given set
  base contained in the family described by predicate. See
  documentation of next-closed-set-in-family for more details. Uses
  initial as first closed set if supplied."
  ([predicate base clop]
     (all-closed-sets-in-family predicate base clop #{}))
  ([predicate base clop initial]
     (lazy-seq
      (let [base    (if (set? base) (improve-basic-order base clop) base),
            initial (clop initial),
            start   (if (predicate initial)
                      initial
                      (next-closed-set-in-family predicate base clop initial)),
            runner  (fn runner [X]
                      (lazy-seq
                       (if (nil? X)
                         nil
                         (cons X (runner (next-closed-set-in-family predicate base clop X))))))]
        (runner start)))))

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

(defn parallel-closures
  "Returns the set of all closures of the closure operator on the given base set.
  Computes the closures in parallel, to the extent possible."
  [base clop]
  (loop [n        0
         closures #{(clop #{})}
         current  #{(clop #{})}]
    (if (< (count base) n)
      closures
      (let [next-current (atom current)]
        (dopar [C current]
          (when (not= (count C) n)
            (swap! next-current #(disj % C))
            (doseq [x base :when (not (contains? C x))]
              (swap! next-current #(conj % (clop (conj C x)))))))
        (recur (inc n) (into closures @next-current) @next-current)))))

;;; Extension

(defn non-closed-elements
  "Given a closure operator (c) and a set (X) returns the subset
   {x in X | c({x}) != {x}}."
  [base clop]
  (set (filter #(not= #{%} (clop #{%})) base)))

(defn exclusive-closure
  "Given a closure operator (c) and a set (s) returns
   c(s)/s"
  [set clop]
  (difference (clop set) set))

(defn- extendable-set
  "Given a closure system and a element, return the subset of the closure
   system whom the element closure is not a subset of."
  [closure clop element]
  (let [x-closure (exclusive-closure #{element} clop)]
    (filter #(not (subset? x-closure %)) closure)))

(defn extension-set
  "Adds the given element to each element of a closure system."
  [closure clop element]
  (for [F (extendable-set closure clop element)]
    (conj F element)))

(defn extend-closure
  "Extents the closure system by an additional given element."
  [closure clop element]
  (union closure (extension-set closure clop element)))

;;; Common Math Algorithms

(defn transitive-closure
  "Computes transitive closure of a given set of pairs."
  ;; Inspired by the corresponding code from the graph library by
  ;; Jeffrey Straszheim
  [pairs]
  (let [pairs-as-map (loop [pairs pairs
                            map   {}]
                       (if (not (seq pairs))
                         map
                         (let [[x y] (first pairs)]
                           (recur (rest pairs)
                                  (update map x conj y)))))
        runner (fn runner [to-be-visited already-visited]
                 (lazy-seq
                  (let [not-yet-visited (seq (drop-while #(contains? already-visited %)
                                                         to-be-visited))
                        unseen-node     (first not-yet-visited)]
                    (when (seq not-yet-visited)
                      (cons unseen-node
                            (runner (concat (get pairs-as-map unseen-node)
                                            (rest not-yet-visited))
                                    (conj already-visited
                                          unseen-node)))))))]
    (set-of [x y] [x (keys pairs-as-map)
                   y (runner (get pairs-as-map x) #{})])))

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
