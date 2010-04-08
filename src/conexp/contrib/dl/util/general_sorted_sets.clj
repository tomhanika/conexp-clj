;; Copyright (c) Daniel Borchmann. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.contrib.dl.util.general-sorted-sets
  (:use conexp.main)
  (:use [clojure.contrib.seq :only (seq-on)]))

(update-ns-meta! conexp.contrib.dl.util.general-sorted-sets
  :doc "Playground for some more or less creative work for DL.")

;;; sorted-set for non-total orderings

(deftype General-Sorted-Set [order-fn maximal-elements minimal-elements])

(declare add-to-gss! remove-from-gss!)

(defn make-general-sorted-set
  "Constructs a general sorted set for the given order
  function. order-fn must represent a reflexiv and transitive
  relation. Elements being equal in the sense of this order relation
  are considered the same."
  [order-fn]
  (General-Sorted-Set order-fn (ref #{}) (ref #{})))

(defn- sort-gss
  "Does topological sort on a given gss."
  [gss]
  (let [runner (fn runner [have]
		 (let [next-have (distinct (for [x have,
						 y @(:uppers x),
						 :when (and (not (some #(= y %) have))
							    (forall [z @(:lowers y)]
								    (some #(= z %) have)))]
					     y))]
		   (if (empty? next-have)
		     have
		     (recur (concat have next-have)))))]
    (runner (vec @(:minimal-elements gss)))))

(defmethod print-method ::General-Sorted-Set [gss out]
  (.write out (with-out-str
		(println "General Sorted Set")
		(doseq [x (sort-gss gss)]
		  (println "Node:" (:node x) ", Lowers:" (map :node @(:lowers x)) ", Uppers:" (map :node @(:uppers x)))))))

(defmethod seq-on ::General-Sorted-Set [gss]
  (map :node (sort-gss gss)))

;;

(defn- find-neighbours
  "Starting from nodes tries to improve all nodes satisfying test-fn
  with neigh-fn, i.e. applies neigh-fn to every node that passes
  test-fn and replaces it with elements from the result which satisfy
  test-fn if there are some. Expects neigh-fn to be cheap and test-fn
  to be expensive."
  [gss test-fn neigh-fn nodes]
  (loop [fluids (filter test-fn nodes),
	 fixed  #{}]
    (if (empty? fluids)
      fixed
      (let [next (first fluids),
	    next-neighs (filter (fn [x] (and (not (contains? fixed x))
					     (test-fn x)))
				(neigh-fn next))]
	(if (empty? next-neighs)
	  (if (some fixed (neigh-fn next))
	    (recur (rest fluids) fixed)
	    (recur (rest fluids) (conj fixed next)))
	  (recur (into (rest fluids) next-neighs) fixed))))))

(defn- find-upper-neighbours
  "Finds all upper neighbours of x in gss."
  [gss x]
  (find-neighbours gss
		   #((:order-fn gss) x (:node %))
		   (comp deref :lowers)
		   @(:maximal-elements gss)))

(defn- find-lower-neighbours
  "Finds all lower neighbours of x in gss."
  [gss x]
  (find-neighbours gss
		   #((:order-fn gss) (:node %) x)
		   (comp deref :uppers)
		   @(:minimal-elements gss)))

(defn add-to-gss!
  "Adds the element elt to the general sorted set gss returning the
  result. Note that this function modifies gss."
  [gss elt]
  (dosync
   (let [order-fn (:order-fn gss),

	 uppers (find-upper-neighbours gss elt),
	 lowers (find-lower-neighbours gss elt),

	 new-elt {:node elt,
		  :uppers (ref uppers),
		  :lowers (ref lowers)}]

     ;; check if element is already there
     (when-not (or (some #(order-fn (:node %) elt) uppers)
		   (some #(order-fn elt (:node %)) lowers))

       ;; update neighbours
       (doseq [u uppers]
	 (alter (:lowers u) conj new-elt))
       (doseq [l lowers]
	 (alter (:uppers l) conj new-elt))

       ;; remove redundant edges
       (doseq [u uppers,
	       l lowers]
	 (when (contains? @(:uppers l) u)
	   (alter (:uppers l) disj u)
	   (alter (:lowers u) disj l)))

       ;; update maximal and minimal elements
       (ref-set (:maximal-elements gss)
		(set-of x [x (conj @(:maximal-elements gss) new-elt)
			   :when (empty? @(:uppers x))]))
       (ref-set (:minimal-elements gss)
		(set-of x [x (conj @(:minimal-elements gss) new-elt)
			   :when (empty? @(:lowers x))])))

     gss)))

(defn remove-from-gss!
  "Removes the element elt from the general sorted set gss."
  [gss elt]
  (unsupported-operation "Removing elements from a general sorted set has not been thought of!"))

;;;

(defn contained-in-gss?
  "Checks whether an element elt in contained in a general sorted set
  gss, i.e. whether there exists an element in gss which is equal (in
  the sense of the underlying order relation) to elt."
  [gss elt]
  (let [order-fn (:order-fn gss),
	uppers (find-upper-neighbours gss elt)]
    (and (= 1 (count uppers))
	 (order-fn elt (:node (first uppers)))
	 (order-fn (:node (first uppers)) elt))))

(defn hasse-graph
  "Returns the Hasse Graph of the general sorted set gss."
  [gss]
  (for [x (sort-gss gss),
	y @(:uppers x)]
    [(:node x) (:node y)]))

;;;

nil
