(ns conexp.fca.contexts
  (:gen-class
   :name conexp.fca.Context
   :prefix "Context-"
   :init init
   :constructors { [ clojure.lang.PersistentHashSet clojure.lang.PersistentHashSet clojure.lang.PersistentHashSet ] [] }
   :state state)
  (:use [clojure.contrib.str-utils :only (str-join)]
	conexp.base))

(defn Context-init [objects attributes incidence]
  [ [] {:objects objects :attributes attributes :incidence incidence} ])

(defn objects [ctx]
  ((.state ctx) :objects))

(defn attributes [ctx]
  ((.state ctx) :attributes))

(defn incidence [ctx]
  ((.state ctx) :incidence))

(defn sort-by-second [x y]
  (let [< #(> 0 (compare (str %1) (str %2)))]
    (cond
      (and (vector? x)
	   (vector? y)
	   (= 2 (count x) (count y)))
      (if (= (second x) (second y))
	(< (first x) (first y))
	(< (second x) (second y)))
      :else
      (< x y))))

(defn print-context [ctx order-on-objects order-on-attributes]
  (let [attributes (vec (sort order-on-objects (attributes ctx)))
	objects    (vec (sort order-on-attributes (objects ctx)))
	incidence  (incidence ctx)

	max-att (reduce #(max %1 (count (str %2))) 0 attributes)
	max-obj (reduce #(max %1 (count (str %2))) 0 objects)]
    (with-str-out
      "\n" 
      (ensure-length "" max-obj " ") " |" (for [att attributes] [(str att) " "]) "\n"
      (ensure-length "" max-obj "-") "-+" (for [att attributes]
					    (ensure-length "" (inc (count (str att))) "-")) "\n"
      (for [obj objects]
	[ (ensure-length (str obj) max-obj) 
	  " |" 
	  (for [att attributes]
	    [ (ensure-length (if (incidence [obj att]) "x" ".") 
			     (count (str att))) 
	      " "])
	  "\n" ]))))

(defn Context-toString [this]
  (print-context this sort-by-second sort-by-second))

(defn Context-equals [this other]
  (and (instance? conexp.fca.Context other)
       (= (objects this) (objects other))
       (= (attributes this) (attributes other))
       (= (incidence this) (incidence other))))

(defn type-of [thing]
  (cond
    (set? thing) ::set
    (fn? thing) ::fn
    (seq? thing) ::seq
    :else ::invalid))

(defmulti make-context (fn [o a i] [(type-of o) (type-of a) (type-of i)]))

(defmethod make-context [::set ::set ::set] [objects attributes incidence]
  (conexp.fca.Context. objects attributes incidence))

(defmethod make-context [::set ::set ::fn] [objects attributes incidence]
  (make-context objects attributes (set-of [x y] [x objects
						  y attributes
						  :when (incidence x y)])))

(defmethod make-context :default [obj att inz]
  (illegal-argument "The arguments " obj ", " att " and " inz " are not valid for a Context."))

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

(defn down-arrows [ctx]
  (let [obj (objects ctx)
	att (attributes ctx)
	inz (incidence ctx)]
    (set-of [g m]
	    [g obj
	     m att
	     :when (and (not (inz [g m]))
			(forall [h obj]
				(=> (proper-subset? (object-derivation ctx #{g})
						    (object-derivation ctx #{h}))
				    (inz [h m]))))])))

(defn up-arrows [ctx]
  (let [obj (objects ctx)
	att (attributes ctx)
	inz (incidence ctx)]
    (set-of [g m]
	    [g obj
	     m att
	     :when (and (not (inz [g m]))
			(forall [n att]
				(=> (proper-subset? (attribute-derivation ctx #{m})
						    (attribute-derivation ctx #{n}))
				    (inz [g n]))))])))

(defn up-down-arrows [ctx] ; faster version?
  (intersection (up-arrows ctx) (down-arrows ctx)))

(defn reduce-context [ctx]
  (let [obj (objects ctx)
	att (attributes ctx)
	inz (incidence ctx)
	uda (up-down-arrows ctx)]
    (let [new-obj (set (map first uda))
	  new-att (set (map second uda))
	  new-inz (set-of [g m] [g new-obj m new-att :when (inz [g m])])]
    (make-context new-obj new-att (set new-inz)))))

(defn reduced? [ctx]
  (let [obj (objects ctx)
	att (attributes ctx)
	uda (up-down-arrows ctx)]
    (and (forall [g obj]
		 (exists [ [h _] uda ]
			 (= g h)))
	 (forall [m att]
		 (exists [ [_ n] uda ]
			 (= m n))))))

(defn transpose-context [ctx]
  (make-context (attributes ctx) (objects ctx) (set-of [m g] [[g m] (incidence ctx)])))

(defn invert-context [ctx]
  (make-context (objects ctx) (attributes ctx) (set-of [g m] [g (objects ctx)
							      m (attributes ctx)
							      :when (not ((incidence ctx) [g m]))])))

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

;; FcaFlint functionalities

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

;;;

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