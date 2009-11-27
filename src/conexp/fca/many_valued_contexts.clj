(ns conexp.fca.many-valued-contexts
  (:use conexp.base
	conexp.fca.contexts)
  (:gen-class
   :name conexp.fca.ManyValuedContext
   :prefix "ManyValuedContext-"
   :init init
   :constructors { [ Object Object Object ] [] }
   :state state))

(defn ManyValuedContext-init [objects attributes incidence]
  [ [] {:objects objects,
	:attributes attributes,
	:incidence incidence} ])

(defmethod objects conexp.fca.ManyValuedContext [mv-ctx]
  ((.state mv-ctx) :objects))

(defmethod attributes conexp.fca.ManyValuedContext [mv-ctx]
  ((.state mv-ctx) :attributes))

(defmethod incidence conexp.fca.ManyValuedContext [mv-ctx]
  ((.state mv-ctx) :incidence))

(defn ManyValuedContext-equals [this other]
  (and (instance? other conexp.fca.ManyValuedContext)
       (= (objects this) (objects other))
       (= (attributes this) (attributes other))
       (let [inz-this (incidence this)
	     inz-other (incidence other)]
	 (forall [g (objects this)
		  m (attributes this)]
	   (= (inz-this [g m])
	      (inz-other [g m]))))))

(defn ManyValuedContext-hashCode
  [#^conexp.fca.ManyValuedContext this]
  (+ (.hashCode (objects this))
     (.hashCode (attributes this))
     (.hashCode (incidence this))))

(defn print-mv-context [mv-ctx]
  (let [objs (objects mv-ctx)
	atts (attributes mv-ctx)
	inz (incidence mv-ctx)

	str #(if (nil? %) "nil" (str %))

	max-obj-len (reduce #(max %1 (count (str %2))) 0 objs)
	max-att-lens (loop [lens (apply hash-map (flatten (for [att atts]
							    [att (count (str att))])))
			    triples (for [g objs
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
      "\n"
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

(defn ManyValuedContext-toString [this]
  (print-mv-context this))

; (defn ManyValuedContext-hashCode [this])

;;;

(defmulti make-mv-context
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
  (conexp.fca.ManyValuedContext. objs atts inz-fn))

(defmethod make-mv-context :default [objs atts inz]
  (illegal-argument "No method defined for types "
		    (math-type objs) ", "
		    (math-type atts) ", "
		    (math-type vals) ", "
		    (math-type inz) "."))

;;;

(defn scale-mv-context [mv-ctx scales]
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

(defn nominal-scale [base]
  (diag-context base))

(defn ordinal-scale
  ([base]
     (ordinal-scale base <=))
  ([base <=]
     (make-context base base <=)))

(defn interordinal-scale
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
  "base must be ordered (e.g. vector or list). Otherwise the result will be arbitrary."
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

(defn dichotomic-scale [base]
  (diag-context base))

nil
