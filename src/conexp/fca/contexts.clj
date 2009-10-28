(ns conexp.fca.contexts
  (:gen-class
   :name conexp.fca.Context
   :prefix "Context-"
   :init init
   :constructors { [ Object Object Object ] [] }
   :state state)
  (:use [clojure.contrib.str-utils :only (str-join)]
	conexp.base))

(defn Context-init [objects attributes incidence]
  [ [] {:objects objects :attributes attributes :incidence incidence} ])

(defn objects
  "Returns the set of object of a formal context."
  [ctx]
  ((.state ctx) :objects))

(defn attributes
  "Returns the set of attributes of a formal context."
  [ctx]
  ((.state ctx) :attributes))

(defn incidence
  "Returns the incidence set of a formal context."
  [ctx]
  ((.state ctx) :incidence))

(defn- thing-order
  "Orders things for proper output of formal contexts."
  [x y]
  (if (= (class x) (class y))
    (if (instance? Comparable x)
      (> 0 (compare x y))
      (< (.hashCode x) (.hashCode y)))
    (> 0 (compare (str (class x)) (str (class y))))))

(defn- sort-by-second
  "Ensures that pairs are ordered by second entry first. This gives
  better output for context sums, products, ..."
  [x y]
  (cond
    (and (vector? x)
	 (vector? y)
	 (= 2 (count x) (count y)))
    (if (= (second x) (second y))
      (thing-order (first x) (first y))
      (thing-order (second x) (second y)))
    :else
    (thing-order x y)))

(defn print-context
  "Prints contexts in a human readable form."
  [ctx order-on-objects order-on-attributes]
  (let [str #(if (= % nil) "nil" (str %))

	attributes (vec (sort order-on-objects (attributes ctx)))
	objects    (vec (sort order-on-attributes (objects ctx)))
	incidence  (incidence ctx)

	max-att (reduce #(max %1 (count (str %2))) 0 attributes)
	max-obj (reduce #(max %1 (count (str %2))) 0 objects)]
    (with-str-out
      "\n" 
      (ensure-length "" max-obj " ") " |" (for [att attributes]
					    [(str att) " "]) "\n"
      (ensure-length "" max-obj "-") "-+" (for [att attributes]
					    (ensure-length "" (inc (count (str att))) "-")) "\n"
      (for [obj objects]
	[(ensure-length (str obj) max-obj)
	 " |"
	 (for [att attributes]
	   [(ensure-length (if (incidence [obj att]) "x" ".")
			   (count (str att)))
	    " "])
	 "\n"]))))

(defn Context-toString
  "Represents context as string by means of print-context."
  [this]
  (print-context this sort-by-second sort-by-second))

(defn Context-equals
  "Implements mathematical equality of two contexts."
  [this other]
  (and (instance? conexp.fca.Context other)
       (= (objects this) (objects other))
       (= (attributes this) (attributes other))
       (let [inz-this (incidence this)
	     inz-other (incidence other)]
	 (or (= (incidence this) (incidence other))
	     (forall [pair (cross-product (objects this) (attributes this))]
               (<=> (inz-this pair) (inz-other pair)))))))

; (defn Context-hashCode [this] ...)

(defn- type-of
  "Dispatch function for make-context. Sequences and sets are made to one thing."
  [thing]
  (cond
    (or (set? thing)
	(sequential? thing)) ::set
    (fn? thing)              ::fn
    :else                    ::invalid))

(defmulti make-context
  "Standard constructor for contexts. Takes a sequence of objects,
  a sequence of attributes and either a set of pairs or function of
  two elements being true iff its arguments are incident. Note that the
  object and attribute sequences are converted to sets and therefore have to
  not contain any douplicate elements."
  (fn [& args] (map type-of args)))

(defmethod make-context [::set ::set ::set] [objects attributes incidence]
  (conexp.fca.Context. (set objects) (set attributes) (set incidence)))

(defmethod make-context [::set ::set ::fn] [objects attributes incidence]
  (make-context objects attributes (set-of [x y] [x objects
						  y attributes
						  :when (incidence x y)])))

(defmethod make-context :default [obj att inz]
  (illegal-argument "The arguments " obj ", " att " and " inz " are not valid for a Context."))


;;; Common Operations in Contexts

(defn object-derivation [ctx objects]
  (let [{:keys [incidence attributes]} (.state ctx)]
    (set-of m [m attributes :when (forall [g objects] (incidence [g m]))])))

(defn attribute-derivation [ctx attributes]
  (let [{:keys [incidence objects]} (.state ctx)]
    (set-of g [g objects :when (forall [m attributes] (incidence [g m]))])))

(defn concept? [ctx [set-of-obj set-of-att]]
  (and (subset? set-of-obj (objects ctx))
       (subset? set-of-att (attributes ctx))
       (= set-of-obj (object-derivation ctx set-of-att))
       (= set-of-att (attribute-derivation ctx set-of-obj))))

(defn clarify-objects [ctx]
  (let [prime (memoize (partial object-derivation ctx))
	new-objs (set (distinct-by-key (objects ctx) #(prime #{%})))
	new-inz (set-of [g m] [[g m] (incidence ctx)
			       :when (contains? new-objs g)])]
    (make-context new-objs (attributes ctx) new-inz)))

(defn clarify-attributes [ctx]
  (let [prime (memoize (partial attribute-derivation ctx))
	new-atts (set (distinct-by-key (attributes ctx) #(prime #{%})))
	new-inz (set-of [g m] [[g m] (incidence ctx)
			       :when (contains? new-atts m)])]
    (make-context (objects ctx) new-atts new-inz)))

(defn clarify-context [ctx]
  (clarify-objects (clarify-attributes ctx)))

(defn object-clarified? [ctx]
  (let [prime (memoize (partial object-derivation ctx))]
    (not (exists [g (objects ctx)
		  h (objects ctx)]
	   (and (not= g h) (= (prime #{g}) (prime #{h})))))))

(defn attribute-clarified? [ctx]
  (let [prime (memoize (partial attribute-derivation ctx))]
    (not (exists [m (attributes ctx)
		  n (attributes ctx)]
	   (and (not= m n) (= (prime #{m}) (prime #{n})))))))

(defn clarified? [ctx]
  (and (object-clarified? ctx)
       (attribute-clarified? ctx)))

(defn down-arrows [ctx]
  (let [obj (objects ctx)
	att (attributes ctx)
	inz (incidence ctx)
	prime (memoize (partial object-derivation ctx))]
    (set-of [g m]
	    [g obj
	     m att
	     :when (and (not (inz [g m]))
			(forall [h obj]
			  (=> (proper-subset? (prime #{g})
					      (prime #{h}))
			      (inz [h m]))))])))

(defn up-arrows [ctx]
  (let [obj (objects ctx)
	att (attributes ctx)
	inz (incidence ctx)
	prime (memoize (partial attribute-derivation ctx))]
    (set-of [g m]
	    [g obj
	     m att
	     :when (and (not (inz [g m]))
			(forall [n att]
			  (=> (proper-subset? (prime #{m})
					      (prime #{n}))
			      (inz [g n]))))])))

(defn up-down-arrows [ctx]
  (intersection (up-arrows ctx) (down-arrows ctx)))

(defn reduce-clarified-context [ctx]
  (let [inz (incidence ctx)
	uda (up-down-arrows ctx)]
    (let [new-obj (set (map first uda))
	  new-att (set (map second uda))
	  new-inz (set-of [g m] [g new-obj m new-att :when (inz [g m])])]
    (make-context new-obj new-att (set new-inz)))))

(defn reduce-context [ctx]
  (if (clarified? ctx)
    (reduce-clarified-context ctx)
    (reduce-clarified-context (clarify-context ctx))))

(defn reduced? [ctx]
  (and (clarified? ctx)
       (let [obj (objects ctx)
	     att (attributes ctx)
	     uda (up-down-arrows ctx)]
	 (and (forall [g obj]
		(exists [ [h _] uda ]
		  (= g h)))
	      (forall [m att]
		(exists [ [_ n] uda ]
		  (= m n)))))))

(defn context-object-closure [ctx set-of-objects]
  (attribute-derivation ctx (object-derivation ctx set-of-objects)))

(defn context-extends [ctx]
  (all-closed-sets (objects ctx) (partial context-object-closure ctx)))

(defn context-attribute-closure [ctx set-of-attributes]
  (object-derivation ctx (attribute-derivation ctx set-of-attributes)))

(defn context-intents [ctx]
  (all-closed-sets (attributes ctx) (partial context-attribute-closure ctx)))

(defn concepts [ctx]
  (set-of [ objs (object-derivation ctx objs) ]
	  [ objs (context-extends ctx) ]))


;; Common Operations with Contexts

(defn dual-context [ctx]
  (make-context (attributes ctx) (objects ctx) (set-of [m g] [[g m] (incidence ctx)])))

(defn invert-context [ctx]
  (make-context (objects ctx) (attributes ctx) (set-of [g m] [g (objects ctx)
							      m (attributes ctx)
							      :when (not ((incidence ctx) [g m]))])))

(defn context-union [ctx1 ctx2]
  (let [new-objs (union (set-of [g_1 1] [g_1 (objects ctx1)])
			(set-of [g_2 2] [g_2 (objects ctx2)]))
	new-atts (union (set-of [m_1 1] [m_1 (attributes ctx1)])
			(set-of [m_2 2] [m_2 (attributes ctx2)]))
	new-inz  (set-of [[g idx-g] [m idx-m]]
			 [[g idx-g] new-objs
			  [m idx-m] new-atts
			  :when (or ((incidence ctx1) [g m])
				    ((incidence ctx2) [g m]))])]
    (make-context new-objs new-atts new-inz)))

(defn context-intersection [ctx1 ctx2]
  (make-context (intersection (objects ctx1) (objects ctx2))
		(intersection (attributes ctx1) (attributes ctx2))
		(intersection (incidence ctx1) (incidence ctx2))))

(defn context-composition [ctx-1 ctx-2]
  (make-context (objects ctx-1)
		(attributes ctx-2)
		(set-of [g m]
			[g (objects ctx-1)
			 m (attributes ctx-2)
			 :when (exists [x (intersection (attributes ctx-1)
							(objects ctx-2))]
				       (and ((incidence ctx-1) [g x])
					    ((incidence ctx-2) [x m])))])))

(defn context-apposition [ctx-1 ctx-2]
  (if (not= (objects ctx-1) (objects ctx-2))
    (illegal-argument "Cannot do context apposition, since object sets are not equal."))
  (let [new-atts (union (set-of [m 1] [m (attributes ctx-1)])
			(set-of [m 2] [m (attributes ctx-2)]))
	new-inz  (union (set-of [g [m 1]] [[g m] (incidence ctx-1)])
			(set-of [g [m 2]] [[g m] (incidence ctx-2)]))]
    (make-context (objects ctx-1) new-atts new-inz)))

(defn context-subposition [ctx-1 ctx-2]
  (if (not= (attributes ctx-1) (attributes ctx-2))
    (illegal-argument "Cannot do context subposition, since attribute sets are not equal."))
  (let [new-objs (union (set-of [g 1] [g (objects ctx-1)])
			(set-of [g 2] [g (objects ctx-2)]))
	new-inz  (union (set-of [[g 1] m] [[g m] (incidence ctx-1)])
			(set-of [[g 2] m] [[g m] (incidence ctx-2)]))]
    (make-context new-objs (attributes ctx-1) new-inz)))

(defn context-transitive-closure [ctx]
  (make-context (objects ctx) (attributes ctx) (transitive-closure (incidence ctx))))

(defn one-context [base-set]
  (make-context base-set base-set (fn [_ _] true)))

(defn null-context [base-set]
  (make-context base-set base-set (fn [_ _] false)))

(defn diag-context [base-set]
  (make-context base-set base-set =))

(defn adiag-context [base-set]
  (make-context base-set base-set not=))

(defn context-sum [ctx-1 ctx-2]
  (let [new-objs (union (set-of [g_1 1] [g_1 (objects ctx-1)])
			(set-of [g_2 2] [g_2 (objects ctx-2)]))
	new-atts (union (set-of [m_1 1] [m_1 (attributes ctx-1)])
			(set-of [m_2 2] [m_2 (attributes ctx-2)]))
	new-inz  (union (set-of [[g_1 1] [m_1 1]]
				[[g_1 m_1] (incidence ctx-1)])
			(set-of [[g_2 2] [m_2 2]]
				[[g_2 m_2] (incidence ctx-2)])
			(set-of [[g_1 1] [m_2 2]]
				[g_1 (objects ctx-1)
				 m_2 (attributes ctx-2)])
			(set-of [[g_2 2] [m_1 1]]
				[g_2 (objects ctx-2)
				 m_1 (attributes ctx-1)]))]
    (make-context new-objs new-atts new-inz)))

(defn context-product [ctx-1 ctx-2]
  (let [new-objs (cross-product (objects ctx-1) (objects ctx-2))
	new-atts (cross-product (attributes ctx-1) (attributes ctx-2))
	inz-1    (incidence ctx-1)
	inz-2    (incidence ctx-2)
	new-inz  (set-of [[g_1, g_2], [m_1, m_2]]
			 [[g_1, g_2] new-objs
			  [m_1, m_2] new-atts
			  :when (or (inz-1 [g_1, m_1])
				    (inz-2 [g_2, m_2]))])]
    (make-context new-objs new-atts new-inz)))

(defn context-semiproduct [ctx-1 ctx-2]
  (let [new-objs (cross-product (objects ctx-1) (objects ctx-2))
	new-atts (disjoint-union (attributes ctx-1) (attributes ctx-2))
	inzs     [(incidence ctx-1) (incidence ctx-2)]
	new-inz  (set-of [g, [m, idx]]
			 [g new-objs
			  [m, idx] new-atts
			  :when ((inzs idx) [(g idx) m])])]
    (make-context new-objs new-atts new-inz)))

(defn context-xia-product [ctx-1 ctx-2]
  (let [G_1 (objects ctx-1)
	G_2 (objects ctx-2)
	M_1 (attributes ctx-1)
	M_2 (attributes ctx-2)
	I_1 (incidence ctx-1)
	I_2 (incidence ctx-2)

	new-objs (cross-product G_1 G_2)
	new-atts (cross-product M_1 M_2)
	new-inz  (set-of [[g_1, g_2], [m_1, m_2]]
			 [[g_1,g_2] new-objs
			  [m_1,m_2] new-atts
			  :when (<=> (I_1 [g_1,m_1])
				     (I_2 [g_2,m_2]))])]
    (make-context new-objs new-atts new-inz)))
