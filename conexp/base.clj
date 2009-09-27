(ns conexp.base
  (:use [clojure.contrib.ns-utils :only (immigrate)]))

(immigrate 'clojure.set
	   'conexp.util)

;;; Set Theory

(defn subset?
  "Returns true iff set-1 \\subseteq set-2."
  [set-1 set-2]
  (every? #(set-2 %) set-1))

(defn proper-subset?
  "Returns true iff (not= set-1 set-2) and set-1 \\subseteq set-2."
  [set-1 set-2]
  (and (not= set-1 set-2)
       (subset? set-1 set-2)))

(defn cross-product
  "Returns cross product of set-1 and set-2."
  [set-1 set-2]
  (set-of [x y] [x set-1 y set-2]))


;;; Next Closure

(defn subelts
  "Returns a subsequence of G until i is reached."
  ;; better implementation with take-while?
  [G i]
  (if (or (empty? G) (= (first G) i))
    (empty G)
    (conj (subelts (rest G) i)
	  (first G))))

(defn lectic-<_i
  "Implements lectic < at position i."
  [G i A B]
  (and (B i) (not (A i))
       (forall [j (subelts G i)]
	       (<=> (B j) (A j)))))

(defn lectic-<
  "Implements lectic ordering."
  [G A B]
  (exists [i G] (lectic-<_i G i A B)))

(defn oplus
  "Implements oplus of the Next Closure Algorithm."
  [G clop A i]
  (clop 
   (conj (intersection (set (subelts G i)) A)
	 i)))

(defn next-closed-set
  "Computes next closed set with the Next Closure Algorithm."
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
       (recur set-of-pairs (union new
				  (set-of [x y]
					  [[x z_1] (difference new old)
					   [z_2 y] set-of-pairs
					   :when (= z_1 z_2)]))
	      new))))
