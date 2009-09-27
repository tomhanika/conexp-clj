(ns conexp.base
  (:use [clojure.contrib.ns-utils :only (immigrate)]))

(immigrate 'clojure.set
	   'conexp.util)

;;; Next Closure

(defn lectic-<_i [G i A B]
  (and (B i) (not (A i))
       (forall [j (subelts G i)]
	       (<=> (B j) (A j)))))

(defn lectic-< [G A B]
  (exists [i G] (lectic-<_i G i A B)))

(defn oplus [G clop A i]
  (clop 
   (conj (intersection (set (subelts G i)) A)
	 i)))

(defn next-closed-set [G clop A]
  (let [oplus-A (partial oplus G clop A)]
    (first
     (for [i (reverse G) :when (lectic-<_i G i A (oplus-A i))] 
       (oplus-A i)))))

(defn all-closed-sets [G clop]
  (take-while identity (iterate (partial next-closed-set G clop) (clop #{}))))


;;;

(defn subsets [set]
  (all-closed-sets set identity))

(defn transitive-closure
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
