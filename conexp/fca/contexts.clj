(ns conexp.fca.contexts
  (:gen-class
   :name "conexp.fca.Context"
   :init init
   :constructors { [ clojure.lang.PersistentHashSet clojure.lang.PersistentHashSet clojure.lang.IFn ] [] }
   :state state)
  (:use [clojure.contrib.str-utils :only (str-join)]
	[clojure.set :only (intersection)]
	conexp.base
	conexp.util))
	

(defn -init [objects attributes incidence]
  [ [] {:objects objects :attributes attributes :incidence incidence} ])

(defn objects [ctx]
  ((.state ctx) :objects))

(defn attributes [ctx]
  ((.state ctx) :attributes))

(defn incidence [ctx]
  ((.state ctx) :incidence))

(defn -toString [this]
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

(defn -equals [this other]
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
			(let [g-prime (object-derivation ctx #{g})]
			  (forall [h obj]
				  (let [h-prime (object-derivation ctx #{h})]
				    (=> (proper-subset? g-prime h-prime)
					(inz [h m]))))))])))

(defn up-arrows [ctx]
  (let [obj (objects ctx)
	att (attributes ctx)
	inz (incidence ctx)]
    (set-of [g m]
	    [g obj
	     m att
	     :when (and (not (inz [g m]))
			(let [m-prime (attribute-derivation ctx #{m})]
			  (forall [n att]
				  (let [n-prime (attribute-derivation ctx #{n})]
				    (=> (proper-subset? m-prime n-prime)
					(inz [g n]))))))])))

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

(defn transpose-context [ctx]
  (make-context (attributes ctx) (objects ctx) (set-of [m g] [[g m] (incidence ctx)])))