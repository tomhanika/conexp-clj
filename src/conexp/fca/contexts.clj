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

(defmulti objects
  "Returns the objects of a formal context."
  class)

(defmethod objects conexp.fca.Context [#^conexp.fca.Context ctx]
  ((.state ctx) :objects))

(defmulti attributes
  "Returns the attributes of a formal context."
  class)

(defmethod attributes conexp.fca.Context [#^conexp.fca.Context ctx]
  ((.state ctx) :attributes))

(defmulti incidence
  "Returns the incidence of a formal context."
  class)

(defmethod incidence conexp.fca.Context [#^conexp.fca.Context ctx]
  ((.state ctx) :incidence))

(defn compare-order
  "Orders things for proper output of formal contexts."
  [x y]
  (if (and (= (class x) (class y))
	   (instance? Comparable x))
    (> 0 (compare x y))
    (> 0 (compare (str (class x)) (str (class y))))))

(defn sort-by-second
  "Ensures that pairs are ordered by second entry first. This gives
  better output for context sums, products, ..."
  [x y]
  (cond
    (and (vector? x)
	 (vector? y)
	 (= 2 (count x) (count y)))
    (if (= (second x) (second y))
      (compare-order (first x) (first y))
      (compare-order (second x) (second y)))
    :else
    (compare-order x y)))

(defn sort-by-first
  "Convenience function for custom context printing."
  [x y]
  (cond
    (and (vector? x)
	 (vector? y)
	 (= 2 (count x) (count y)))
    (if (= (first x) (first y))
      (compare-order (second x) (second y))
      (compare-order (first x) (first y)))
    :else
    (compare-order x y)))

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
    :else                    ::other))

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

(defn rename-objects
  "Rename objects in ctx by given hash old-to-new."
  [ctx old-to-new]
  (let [objs (map old-to-new (objects ctx))
	inz (set-of [(old-to-new g) m] [[g m] (incidence ctx)])]
    (make-context objs (attributes ctx) inz)))

(defn rename-attributes
  "Rename attributes in ctx by given hash old-to-new."
  [ctx old-to-new]
  (let [atts (map old-to-new (attributes ctx))
	inz (set-of [g (old-to-new m)] [[g m] (incidence ctx)])]
    (make-context (objects ctx) atts inz)))

(defn object-derivation
  "Computes set of attributes common to all objects in context."
  [ctx objects]
  (let [inz (incidence ctx)
	atts (attributes ctx)]
    (set-of m [m atts :when (forall [g objects] (inz [g m]))])))

(defn attribute-derivation
  "Computes set of objects common to all attributes in context."
  [ctx attributes]
  (let [inz (incidence ctx)
        objs (objects ctx)]
    (set-of g [g objs :when (forall [m attributes] (inz [g m]))])))

(defn concept?
  "Tests whether given pair is a concept in context ctx."
  [ctx [set-of-obj set-of-att]]
  (and (subset? set-of-obj (objects ctx))
       (subset? set-of-att (attributes ctx))
       (= set-of-obj (object-derivation ctx set-of-att))
       (= set-of-att (attribute-derivation ctx set-of-obj))))

(defn clarify-objects
  "Clarifies objects in context ctx."
  [ctx]
  (let [prime (memoize (partial object-derivation ctx))
	new-objs (set (distinct-by-key (objects ctx) #(prime #{%})))
	new-inz (set-of [g m] [[g m] (incidence ctx)
			       :when (contains? new-objs g)])]
    (make-context new-objs (attributes ctx) new-inz)))

(defn clarify-attributes
  "Clarifies attributes in context ctx."
  [ctx]
  (let [prime (memoize (partial attribute-derivation ctx))
	new-atts (set (distinct-by-key (attributes ctx) #(prime #{%})))
	new-inz (set-of [g m] [[g m] (incidence ctx)
			       :when (contains? new-atts m)])]
    (make-context (objects ctx) new-atts new-inz)))

(defn clarify-context
  "Clarifies context ctx."
  [ctx]
  (clarify-objects (clarify-attributes ctx)))

(defn object-clarified?
  "Tests whether given context ctx is object clarified."
  [ctx]
  (let [prime (memoize (partial object-derivation ctx))]
    (not (exists [g (objects ctx)
		  h (objects ctx)]
	   (and (not= g h) (= (prime #{g}) (prime #{h})))))))

(defn attribute-clarified?
  "Tests whether given context ctx is attribute clarified."
  [ctx]
  (let [prime (memoize (partial attribute-derivation ctx))]
    (not (exists [m (attributes ctx)
		  n (attributes ctx)]
	   (and (not= m n) (= (prime #{m}) (prime #{n})))))))

(defn clarified?
  "Tests whether given context ctx is clarified."
  [ctx]
  (and (object-clarified? ctx)
       (attribute-clarified? ctx)))

(defn down-arrows
  "Computes the down arrow relation of ctx."
  [ctx]
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

(defn up-arrows
  "Computes the up arrow relation of ctx."
  [ctx]
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

(defn up-down-arrows
  "Returns up-down-arrow relation of ctx."
  [ctx]
  (intersection (up-arrows ctx) (down-arrows ctx)))

(defn reduce-clarified-context
  "Reduces context ctx assuming it is clarified."
  [ctx]
  (let [inz (incidence ctx)
	uda (up-down-arrows ctx)]
    (let [new-obj (set (map first uda))
	  new-att (set (map second uda))
	  new-inz (set-of [g m] [g new-obj m new-att :when (inz [g m])])]
    (make-context new-obj new-att (set new-inz)))))

(defn reduce-context
  "Reduces context ctx."
  [ctx]
  (if (clarified? ctx)
    (reduce-clarified-context ctx)
    (reduce-clarified-context (clarify-context ctx))))

(defn reduced?
  "Tests whether given context ctx is reduced or not."
  [ctx]
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

(defn context-object-closure
  "Computes double prime in context ctx for the given set-of-objects."
  [ctx set-of-objects]
  (attribute-derivation ctx (object-derivation ctx set-of-objects)))

(defn context-extends
  "Computes a sequence of all extends of ctx."
  [ctx]
  (all-closed-sets (objects ctx) (partial context-object-closure ctx)))

(defn context-attribute-closure
  "Computes double prime in context ctx for the given set-of-attributes."
  [ctx set-of-attributes]
  (object-derivation ctx (attribute-derivation ctx set-of-attributes)))

(defn context-intents
  "Computes a sequence of all intents of ctx."
  [ctx]
  (all-closed-sets (attributes ctx) (partial context-attribute-closure ctx)))

(defn concepts
  "Returns a sequence of all concepts of ctx."
  [ctx]
  (binding [attribute-derivation (memoize attribute-derivation)
	    object-derivation    (memoize object-derivation)]
    (for [ objs (context-extends ctx) ]
      [ objs (object-derivation ctx objs) ])))


;; Common Operations with Contexts

(defn dual-context
  "Dualizes context ctx, that is $(G,M,I)$ gets $(M,G,I^{-1})$."
  [ctx]
  (make-context (attributes ctx) (objects ctx) (set-of [m g] [[g m] (incidence ctx)])))

(defn invert-context
  "Inverts context ctx, that is $(G,M,I)$ gets $(G,M,(G\\times M)\\setminus I)$."
  [ctx]
  (make-context (objects ctx) (attributes ctx) (set-of [g m] [g (objects ctx)
							      m (attributes ctx)
							      :when (not ((incidence ctx) [g m]))])))

(defn context-union
  "Returns context union of ctx1 and ctx2."
  [ctx1 ctx2]
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

(defn context-intersection
  "Returns context intersection of ctx1 and ctx2."
  [ctx1 ctx2]
  (make-context (intersection (objects ctx1) (objects ctx2))
		(intersection (attributes ctx1) (attributes ctx2))
		(intersection (incidence ctx1) (incidence ctx2))))

(defn context-composition
  "Returns context composition of ctx-1 and ctx-2, that is
  \\[
      (G_1,M_1,I_1)\\circ(G_2,M_2,I_2) := (G_1,M_2,I_1\\circ I_2).
  \\]."
  [ctx-1 ctx-2]
  (make-context (objects ctx-1)
		(attributes ctx-2)
		(set-of [g m]
			[g (objects ctx-1)
			 m (attributes ctx-2)
			 :when (exists [x (intersection (attributes ctx-1)
							(objects ctx-2))]
				       (and ((incidence ctx-1) [g x])
					    ((incidence ctx-2) [x m])))])))

(defn context-apposition
  "Returns context apposition of ctx-1 and ctx-2, that is
  \\[
     (G_1,M_1,I_1) | (G_1,M_2,I_2) := (G_1,M_1\\cup M_2,I_1\\cup I_2).
  \\]"
  [ctx-1 ctx-2]
  (if (not= (objects ctx-1) (objects ctx-2))
    (illegal-argument "Cannot do context apposition, since object sets are not equal."))
  (let [new-atts (union (set-of [m 1] [m (attributes ctx-1)])
			(set-of [m 2] [m (attributes ctx-2)]))
	new-inz  (union (set-of [g [m 1]] [[g m] (incidence ctx-1)])
			(set-of [g [m 2]] [[g m] (incidence ctx-2)]))]
    (make-context (objects ctx-1) new-atts new-inz)))

(defn context-subposition
  "Returns context subposition of ctx-1 and ctx-2, that is
  \\[
     \frac{(G_1,M_1,I_1)}{(G_2,M_1,I_2)} := (G_1\\cup G_2,M_1,I_1\\cup I_2).
  \\]"
  [ctx-1 ctx-2]
  (if (not= (attributes ctx-1) (attributes ctx-2))
    (illegal-argument "Cannot do context subposition, since attribute sets are not equal."))
  (let [new-objs (union (set-of [g 1] [g (objects ctx-1)])
			(set-of [g 2] [g (objects ctx-2)]))
	new-inz  (union (set-of [[g 1] m] [[g m] (incidence ctx-1)])
			(set-of [[g 2] m] [[g m] (incidence ctx-2)]))]
    (make-context new-objs (attributes ctx-1) new-inz)))

(defn context-transitive-closure
  "Transitively closes incidence relation of ctx and returns corresponding context."
  [ctx]
  (make-context (objects ctx) (attributes ctx) (transitive-closure (incidence ctx))))

(defn rand-context
  "Randomly fills context on base-set with crosses and propability fill-rate."
  [base-set fill-rate]
  (make-context base-set base-set (fn [_ _] (> fill-rate (rand)))))

(defn one-context
  "Returns context full of crosses."
  [base-set]
  (make-context base-set base-set (fn [_ _] true)))

(defn null-context
  "Returns context with no crosses."
  [base-set]
  (make-context base-set base-set (fn [_ _] false)))

(defn diag-context
  "Returns = on base-set as context."
  [base-set]
  (make-context base-set base-set =))

(defn adiag-context
  "Returns $\\neq$ on base-set as context."
  [base-set]
  (make-context base-set base-set not=))

(defn context-sum
  "Computes the context sum of ctx-1 and ctx-2."
  [ctx-1 ctx-2]
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

(defn context-product
  "Computes the context product of ctx-1 and ctx-2."
  [ctx-1 ctx-2]
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

(defn context-semiproduct
  "Computes the context semiproduct of ctx-1 and ctx-2."
  [ctx-1 ctx-2]
  (let [new-objs (cross-product (objects ctx-1) (objects ctx-2))
	new-atts (disjoint-union (attributes ctx-1) (attributes ctx-2))
	inzs     [(incidence ctx-1) (incidence ctx-2)]
	new-inz  (set-of [g, [m, idx]]
			 [g new-objs
			  [m, idx] new-atts
			  :when ((inzs idx) [(g idx) m])])]
    (make-context new-objs new-atts new-inz)))

(defn context-xia-product
  "Computes Xia's product of ctx-1 and ctx-2."
  [ctx-1 ctx-2]
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

nil
