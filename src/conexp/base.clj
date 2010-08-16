;; Copyright (c) Daniel Borchmann. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.base
  (:require clojure.set
	    clojure.contrib.set
	    clojure.contrib.math
	    clojure.contrib.lazy-seqs
	    clojure.contrib.def)
  (:use [clojure.contrib.ns-utils :only (immigrate)]))

(immigrate 'clojure.set
	   'clojure.contrib.set
	   'clojure.contrib.math
	   'clojure.contrib.lazy-seqs
	   'clojure.contrib.def)

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

(defn prime?
  "Returns true iff n is prime with a certainty of (- 1 (/ 1 (expt 2 1000)))"
  [n]
  (let [actual-number (if (instance? BigInteger n)
			n
			(BigInteger. (str n)))]
    (.isProbablePrime #^java.math.BigInteger actual-number 1000)))

(defn crossfoot
  "Returns the crossfoot of n."
  [n]
  (reduce + (map #(Integer/parseInt (str %)) (str n))))

(defn factorial
  "Returns n!."
  [n]
  (reduce * (range 2 (inc n))))


;;; Next Closure

(defn subelts
  "Returns a subsequence of seq up to index i."
  [seq i]
  (take-while #(not= % i) seq))

(defn lectic-<_i
  "Implements lectic < at position i. The basic order is given by the ordering
  of G which is interpreted as increasing order.

  A and B have to be sets."
  [G i A B]
  (and (contains? B i) (not (contains? A i)) ; A and B will always be sets
       (forall [j (subelts G i)]
         (<=> (contains? B j) (contains? A j)))))

(defn lectic-<
  "Implements lectic ordering. The basic order is given by the ordering of G
  which is interpreted as increasing order."
  [G A B]
  (exists [i G] (lectic-<_i G i A B)))

(defn oplus
  "Implements oplus from the Next Closure Algorithm."
  [G clop A i]
  (clop (set (conj (filter #(contains? A %) (subelts G i))
		   i))))

(defn next-closed-set-in-family
  "Computes next closed set as with next-closed-set, which is in the
  family $\\mathcal{F}$ of all closed sets satisfing
  predicate. predicate has to satisfy the condition
  \\[
     A \\in \\mathcal{F} \text{ and } i \\in G
     \\implies
     \\mathrm{clop}(A \\cap \\set{1, \\ldots, i-1}) \\in \\mathcal{F}.
  \\]"
  [predicate G clop A]
  (loop [i-s (reverse G),
         A   (set A)]
    (if (empty? i-s)
      nil
      (let [i (first i-s)]
        (if (contains? A i)
          (recur (rest i-s) (disj A i))
          (let [clop-A (clop (conj A i))]
            (if (and (lectic-<_i G i A clop-A)
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

(defn next-closed-set
  "Computes next closed set with the Next Closure Algorithm. The order of elements in G,
  interpreted as increasing, is taken to be the basic order of the elements."
  [G clop A]
  (next-closed-set-in-family (constantly true) G clop A))

(defn all-closed-sets
  "Computes all closed sets of a given closure operator on a given
  set. Uses initial as first closed set it if supplied."
  ([G clop]
     (all-closed-sets G clop #{}))
  ([G clop initial]
     (let [G (if (set? G)
	       (improve-basic-order G clop)
	       G)]
       (binding [subelts (memoize subelts)]
	 (take-while identity
		     (iterate (partial next-closed-set G clop)
			      (clop initial)))))))

(defn all-closed-sets-in-family
  "Computes all closed sets of a given closure operator on a given set
  contained in the family described by predicate. See documentation of
  next-closed-set-in-family for more details. Uses initial as first
  closed set if supplied."
  ([predicate G clop]
     (all-closed-sets-in-family predicate G clop #{}))
  ([predicate G clop initial]
     (let [G (if (set? G)
	       (improve-basic-order G clop)
	       G)]
       (binding [subelts (memoize subelts)]
	 (let [start (first (filter predicate (all-closed-sets G clop initial)))]
	   (take-while identity
		       (iterate (partial next-closed-set-in-family predicate G clop)
				start)))))))

;;; Common Math Algorithms

(defn subsets
  "Returns all subsets of set."
  [set]
  (all-closed-sets set identity))

(defn transitive-closure
  "Computes transitive closure of a given set of pairs."
  ([set-of-pairs]
     (transitive-closure set-of-pairs set-of-pairs #{}))
  ([set-of-pairs new old]
     (if (= new old)
       new
       (recur set-of-pairs
	      (union new
		     (set-of [x y]
			     [[x z_1] (difference new old)
			      [z_2 y] set-of-pairs
                             :when (= z_1 z_2)]))
	      new))))
	      
(defn reflexive-transitive-closure
  "Computes the reflexive, transitive closure of a given set of pairs
  on base-set."
  [base-set set-of-pairs]
  (transitive-closure (union set-of-pairs
			     (set-of [x x] [x base-set]))))

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
           minimals []]                 ;minimals elements already found
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
  [<= & xs]
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
  [<= & xs]
  (apply partial-min #(<= %2 %1) xs))

;;;

nil
