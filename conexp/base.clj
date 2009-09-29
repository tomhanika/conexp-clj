(ns conexp.base
  (:use [clojure.contrib.ns-utils :only (immigrate)]))

(immigrate 'clojure.set
	   'clojure.contrib.set
	   'conexp.util)


;;; Set Theory

(defn cross-product
  "Returns cross product of set-1 and set-2."
  [set-1 set-2]
  (set-of [x y] [x set-1 y set-2]))


;;; Next Closure

(defn subelts
  "Returns a subsequence of seq up to index i."
  [seq i]
  (take-while #(not= % i) seq))

(defn lectic-<_i
  "Implements lectic < at position i. The basic order is given by the ordering
  of G which is interpreted as increasing order."
  [G i A B]
  (and (contains? B i) (not (contains? A i))
       (forall [j (subelts G i)]
         (<=> (contains? B j) (contains? A j)))))

(defn lectic-<
  "Implements lectic ordering. The basic order is given by the ordering of G
  which is interpreted as increasing order."
  [G A B]
  (exists [i G] (lectic-<_i G i A B)))

(defn oplus
  "Implements oplus of the Next Closure Algorithm."
  [G clop A i]
  (clop 
   (conj (intersection (set (subelts G i)) A)
	 i)))

(defn next-closed-set
  "Computes next closed set with the Next Closure Algorithm. The order of elements in G,
  interpreted as increasing, is taken to be the basic order of the elements."
  [G clop A]
  (let [oplus-A (partial oplus G clop A)]
    (first
     (for [i (reverse G) :when (lectic-<_i G i A (oplus-A i))] 
       (oplus-A i)))))

(defn all-closed-sets
  "Computes all closed sets of a given closure operator on a given set."
  [G clop]
  (take-while identity (iterate (partial next-closed-set G clop) (clop #{}))))


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

(defn graph-of-function?
  "Returns true iff relation is the graph of a function from source to target."
  [relation source target]
  (and (= (set-of x [[x y] relation]) source)
       (subset? (set-of y [[x y] relation]) target)
       (= (count source) (count relation)))) ; this works because everything is finite
