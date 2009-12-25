(ns conexp.contrib.algorithms.concepts
  (:use conexp.contrib.algorithms.bitwise)
  (:use [conexp.fca.contexts :only (objects attributes incidence)])
  (:import [java.util BitSet])
  (:import [java.util.concurrent SynchronousQueue]))


(set! *warn-on-reflection* true)

;;; Concept Calculation

(defmulti concepts
  "Computes concepts with various algorithms, given as first argument."
  (fn [& args] (first args)))


;; NextClosure with BitSets (:next-closure)

(defn lectic-<_i
  ""
  [i #^BitSet A #^BitSet B]
  (let [i (int i)]
    (and (.get B i)
	 (not (.get A i))
	 (loop [j (int (dec i))]
	   (cond
	     (< j 0)
	     true,
	     (not= (.get A j) (.get B j))
	     false,
	     :else
	     (recur (dec j)))))))

(defn oplus
  ""
  [object-count attribute-count incidence-matrix #^BitSet A i]
  (let [#^BitSet A-short (.clone A)
	#^BitSet B (BitSet.)
	i (int i)]
    (.set A-short i true)
    (.set A-short (inc i) (int attribute-count) false)
    (dotimes [obj object-count]
      (if (forall-in-bitset [att A-short]
	    (== 1 (deep-aget ints incidence-matrix obj att)))
	(.set B obj)))
    (dotimes [att attribute-count]
      (if (and (not (.get A-short att))
	       (forall-in-bitset [obj B]
		 (== 1 (deep-aget ints incidence-matrix obj att))))
	(.set A-short att)))
    A-short))

(defn next-closed-set
  ""
  [object-count attribute-count incidence-matrix #^BitSet A]
  (loop [i (dec (int attribute-count))]
    (cond
      (== -1 i)
      nil,
      (.get A i)
      (recur (dec i)),
      :else
      (let [A_i (oplus object-count attribute-count incidence-matrix A i)]
	(if (lectic-<_i i A A_i)
	  A_i
	  (recur (dec i)))))))

(defmethod concepts :next-closure [_ context]
  (let [object-vector (vec (objects context))
	attribute-vector (vec (attributes context))
	incidence-matrix (to-binary-matrix object-vector attribute-vector (incidence context))

	object-count (int (count object-vector))
	attribute-count (int (count attribute-vector))

	o-prime (partial bitwise-object-derivation incidence-matrix object-count attribute-count)
	a-prime (partial bitwise-attribute-derivation incidence-matrix object-count attribute-count)
	start (o-prime (a-prime (BitSet.)))

	intents (take-while identity
			    (iterate #(next-closed-set object-count
						       attribute-count
						       incidence-matrix
						       %)
				     start))]
    (map (fn [bitset]
	   [(to-hashset object-vector (a-prime bitset))
	    (to-hashset attribute-vector bitset)])
	 intents)))


;; Vychodil (:vychodil)

(defn compute-closure
  ""
  [object-count attribute-count incidence-matrix rows #^BitSet A #^BitSet B y]
  (let [#^BitSet C (BitSet.)
	#^BitSet D (BitSet.)
	#^BitSet E (BitSet.)
	y (int y)]
    (.set D 0 (int attribute-count))
    (.or E A)
    (.and E (aget #^objects rows y))
    (dobits [i E]
      (.set C i)
      (dotimes [j attribute-count]
	(if (== 0 (deep-aget ints incidence-matrix i j))
	  (.set D j false))))
    [C, D]))

(defn generate-from
  ""
  [object-count, attribute-count, incidence-matrix, rows, #^BitSet A, #^BitSet B, y, #^SynchronousQueue queue]
  (.put queue [A, B])
  (if (or (== attribute-count (.cardinality B))
	  (>= (int y) (int attribute-count)))
    nil
    (doseq [j (range (int y) (int attribute-count))
	    :when (not (.get B j))
	    :let [[#^BitSet C, #^BitSet D] (compute-closure object-count attribute-count,
							    incidence-matrix rows,
							    A B j)
		  skip (loop [k (int 0)]
			 (cond
			   (== k j)
			   false,
			   (not= (.get D k) (.get B k))
			   true,
			   :else
			   (recur (inc k))))]
	    :when (not skip)]
      (generate-from object-count attribute-count,
		     incidence-matrix rows,
		     C D (inc j)
		     queue))))

(defmethod concepts :vychodil [_ context]
  (let [[object-vector attribute-vector
	 object-count attribute-count
	 incidence-matrix]
	(to-binary-context context)

	rows (into-array (map (fn [y]
				(let [#^BitSet bs (BitSet.)]
				  (.set bs y)
				  (bitwise-attribute-derivation incidence-matrix
								object-count
								attribute-count
								bs)))
			      (range attribute-count)))

	empty-down (bitwise-attribute-derivation incidence-matrix
						 object-count
						 attribute-count
						 (BitSet.))
	empty-down-up (bitwise-object-derivation incidence-matrix
						 object-count
						 attribute-count
						 empty-down)
	queue (SynchronousQueue.)
	worker (Thread. (fn []
			  (generate-from object-count attribute-count,
					 incidence-matrix rows,
					 empty-down empty-down-up 0
					 queue)
			  (.put queue 1)))]
    (.start worker)
    (map (fn [pair]
	   [(to-hashset object-vector (first pair))
	    (to-hashset attribute-vector (second pair))])
	 (take-while #(not= 1 %)
		     (repeatedly #(.take queue))))))


;; In-Close (:in-close)

(defn in-close
  ""
  [object-count attribute-count incidence-matrix A B r y]
  )

(defn cannonical?
  ""
  [attribute-count incidence-matrix A B y]
  )

(defmethod concepts :in-close [_ context]
  )


;; Nourine, Bordat, Lindig?

nil
