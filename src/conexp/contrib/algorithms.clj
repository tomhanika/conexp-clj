(ns conexp.contrib.algorithms
  (:use conexp.base))

;; ParallelNextClosure (experimental)

(defn all-closed-sets-in-interval
  "Returns all closed sets of clop within the interval between
  start (inclusive) and end (exclusive), taking the order of the
  elements of G as basic order."
  [G clop [start end]]
  (let [start (set start)
	end   (set end)]
    (binding [subelts (memoize subelts)]
      (let [sqn (take-while (fn [closure]
			      (proper-subset? closure end))
			    (iterate (partial next-closed-set G clop)
				     start))]
	(if (= start (clop start))
	  sqn
	  (rest sqn))))))

(defn all-closed-sets-in-parallel
  "Part of ParallelNextClosure (experimental): Computes closures of
  clop on G in parallel, partition of search space is not very good by
  now."
  [G clop]
  (let [G (seq G)
	intervals (partition 2 1 (reverse (tails G)))]
    (concat (reduce concat
		    (pmap (partial all-closed-sets-in-interval G clop) intervals))
	    (list (clop (set G))))))

nil
