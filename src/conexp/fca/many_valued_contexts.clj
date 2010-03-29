;; Copyright (c) Daniel Borchmann. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.fca.many-valued-contexts
  (:use conexp.base
	conexp.fca.contexts))

;;;

(deftype ManyValuedContext [objects attributes incidence]
  :as this
  Object
  (equals [other]
    (and (= (type other) ::ManyValuedContext)
	 (= (objects this) (objects other))
	 (= (attributes this) (attributes other))
	 (let [inz-this (incidence this)
	       inz-other (incidence other)]
	   (forall [g (objects this)
		    m (attributes this)]
		   (= (inz-this [g m])
		      (inz-other [g m])))))))

(defmethod objects ::ManyValuedContext [mv-ctx]
  (:objects mv-ctx))

(defmethod attributes ::ManyValuedContext [mv-ctx]
  (:attributes mv-ctx))

(defmethod incidence ::ManyValuedContext [mv-ctx]
  (:incidence mv-ctx))

(defn print-mv-context
  "Prints the given many-valued context mv-ctx as a value-table."
  [mv-ctx]
  (let [objs (objects mv-ctx)
	atts (attributes mv-ctx)
	inz (incidence mv-ctx)

	str #(if (nil? %) "nil" (str %))

	max-obj-len (reduce #(max %1 (count (str %2))) 0 objs)
	max-att-lens (loop [lens (apply hash-map (flatten (for [att atts]
							    [att (count (str att))])))
			    triples (for [g objs,
					  m atts]
				      [g m (inz [g m])])]
		       (if (empty? triples)
			 lens
			 (let [[g m w] (first triples)
			       len (count (str w))]
			   (if (> len (lens m))
			     (recur (assoc lens m len) (rest triples))
			     (recur lens (rest triples))))))]
    (with-str-out
      (ensure-length "" max-obj-len " ") " |" (for [att atts]
						[(ensure-length (str att) (max-att-lens att) " ") " "])
      "\n"
      (ensure-length "" max-obj-len "-") "-+" (for [att atts]
						(ensure-length "" (inc (max-att-lens att)) "-"))
      "\n"
      (for [obj objs]
	[(ensure-length (str obj) max-obj-len)
	 " |"
	 (for [att atts]
	   [(ensure-length (str (inz [obj att]))
			   (max-att-lens att))
	    " "])
	 "\n"]))))

(defmethod print-method ::ManyValuedContext [mv-ctx out]
  (.write out (print-mv-context mv-ctx)))

;;;

(defmulti make-mv-context
  "Constructs a many-valued context from a set of objects, a set of
  attributes and an incidence relation, given as set of triples [g m w]
  or as a function from two arguments [g m] to values w."
  {:arglists '([objects attributes incidence])}
  (fn [& args] (map math-type args)))

(defmethod make-mv-context [:conexp.util/set :conexp.util/set :conexp.util/set]
  [objs atts inz]
  (make-mv-context (set objs) (set atts)
		   (loop [hash {}
			  items inz]
		     (if (empty? items)
		       hash
		       (let [[g m w] (first items)]
			 (recur (assoc hash [g m] w) (rest items)))))))

(defmethod make-mv-context [:conexp.util/set :conexp.util/set :conexp.util/fn]
  [objs atts inz-fn]
  (ManyValuedContext objs atts (fn [[g m]] (inz-fn g m))))

(defmethod make-mv-context :default [objs atts inz]
  (illegal-argument "No method defined for types "
		    (math-type objs) ", "
		    (math-type atts) ", "
		    (math-type vals) ", "
		    (math-type inz) "."))

;;;

(defn scale-mv-context
  "Scales given many-valued context mv-ctx with given scales. scales
  must be a map from attributes m to contexts K, where all possible
  values of m in mv-ctx are among the objects in K."
  [mv-ctx scales]
  (assert (map? scales))
  (let [inz (incidence mv-ctx)

	objs (objects mv-ctx)
	atts (set-of [m n] [m (attributes mv-ctx)
			    n (attributes (scales m))])
	inz (set-of [g [m n]] [g objs
			       [m n] atts
			       :let [w (inz [g m])]
			       :when ((incidence (scales m)) [w n])])]
    (make-context objs atts inz)))

(defn nominal-scale
  "Returns the nominal scale on the set base."
  [base]
  (diag-context base))

(defn ordinal-scale
  "Returns the ordinal scale on the set base, optionally given an
  order relation <=."
  ([base]
     (ordinal-scale base <=))
  ([base <=]
     (make-context base base <=)))

(defn interordinal-scale
  "Returns the interordinal scale on the set base, optionally given
  two order relations <= and >=."
  ([base]
     (interordinal-scale base <= >=))
  ([base <= >=]
     (let [objs base
	   atts-<= (map #(str "<= " %) base)
	   atts->= (map #(str ">= " %) base)
	   inz-<= (set-of [g (str "<= " m)]
			  [g objs
			   m base
			   :when (<= g m)])
	   inz->= (set-of [g (str ">= " m)]
			  [g objs
			   m base
			   :when (>= g m)])]
       (make-context base (union atts-<= atts->=) (union inz-<= inz->=)))))

(defn biordinal-scale
  "Returns the biordinal scale on the sequence base, optionally given
  two order relations <= and >=. Note that base must be
  ordered (e.g. vector or list), because otherwise the result will be
  arbitrary."
  ([base n]
     (biordinal-scale base n <= >=))
  ([base n <= >=]
     (let [first-objs (take n base)
	   rest-objs (drop n base)

	   first-atts (map #(str "<= " %) first-objs)
	   rest-atts (map #(str ">= " %) rest-objs)

	   first-inz (set-of [g (str "<= " m)]
			     [g first-objs
			      m first-objs
			      :when (<= g m)])
	   rest-inz (set-of [g (str ">= " m)]
			    [g rest-objs
			     m rest-objs
			     :when (>= g m)])]
       (make-context base
		     (union first-atts rest-atts)
		     (union first-inz rest-inz)))))

(defn dichotomic-scale
  "Returns the dichotimic scale on the set base. Note that base must
  have exactly two arguments."
  [base]
  (assert (= 2 (count base)))
  (diag-context base))

;;;

nil
