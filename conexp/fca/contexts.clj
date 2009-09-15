(ns conexp.fca.contexts
  (:gen-class
   :name conexp.fca.Context
   :prefix "Context-"
   :init init
   :constructors { [ clojure.lang.PersistentHashSet clojure.lang.PersistentHashSet clojure.lang.IFn ] [] }
   :state state)
  (:use [clojure.contrib.str-utils :only (str-join)]
	[clojure.set :only (intersection)]
	conexp.base
	conexp.util))

(defn Context-init [objects attributes incidence]
  [ [] {:objects objects :attributes attributes :incidence incidence} ])

(defn objects [ctx]
  ((.state ctx) :objects))

(defn attributes [ctx]
  ((.state ctx) :attributes))

(defn incidence [ctx]
  ((.state ctx) :incidence))

(defn Context-toString [this]
  (let [objects    (vec ((.state this) :objects))
	attributes (vec ((.state this) :attributes))
	incidence  ((.state this) :incidence)]
    (let [max-att (reduce #(max (count (str %2)) %1) attributes)
	  max-obj (reduce #(max (count (str %2)) %1) objects)]
      (with-str-out
	"\n" 
	(ensure-length "" max-obj " ") " |" (for [att attributes] [att " "]) "\n"
	(ensure-length "" max-obj "-") "-+" (for [att attributes]
					      (ensure-length "" (inc (count (str att))) "-")) "\n"
	(for [obj objects]
	  [ (ensure-length (str obj) max-obj) 
	    " |" 
	    (for [att attributes]
	      [ (ensure-length (if (incidence [obj att]) "x" ".") 
			       (count (str att))) 
		" "])
	    "\n" ])))))

(defn Context-equals [this other]
  (and (= (objects this) (objects other))
       (= (attributes this) (attributes other))
       (= (incidence this) (incidence other))))

(defn make-context [objects attributes incidence]
  (conexp.fca.Context. (set objects) (set attributes) (set incidence)))

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

(defn up-down-arrows [ctx]
  (intersection (up-arrows ctx) (down-arrows ctx)))

(defn reduce-context [ctx]
  (let [obj (objects ctx)
	att (attributes ctx)
	inz (incidence ctx)
	uda (up-down-arrows ctx)]
    (let [new-obj (set (map first uda))
	  new-att (set (map second uda))
	  new-inz (filter #(and (new-obj (first %))
				(new-att (second %)))
			  inz)]
    (make-context new-obj new-att new-inz))))

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