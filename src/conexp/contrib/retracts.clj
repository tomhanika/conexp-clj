(ns #^{:doc "Package for computing retracts from formal contexts."}
  conexp.contrib.retracts
  (:use [conexp :only (concepts,
		       objects, object-derivation,
		       attributes, attribute-derivation,
		       compatible-subcontexts,
		       restrict-concept,
		       make-context,
		       set-of)]))

(defn- homomorphism-by-csc
  "Returns the homomorphisms obtained by resticting every concept to the
  given compatible subcontext. (csc == compatible subcontext)"
  [subcontext]
  (fn [concept]
    (restrict-concept concept subcontext)))

(defn- homomorphisms-by-cscs
  "Returns all homomorphisms originating from compatible
  subcontexts. context has to be reduced."
  [context]
  (map homomorphism-by-csc (compatible-subcontexts context)))

(defn- endofunctions-as-hash ; name is misleading
  "Given a homomorphisms which has been obtained by a compatible
  subcontext this function computes a hash representing all
  endofunctions of context originating from this homomorphism. The
  result may be thought of as a \"multifunction\"."
  [context hom]
  (reduce (fn [hash concept]
	    (assoc hash (hom concept)
		   (conj (hash (hom concept)) concept)))
	  {}
	  (concepts context)))

(defn- endofunctions-from-hash ; name is misleading
  "Computes all endofunctions (as a hash) in endo-hash."
  [endo-hash]
  (let [runner (fn runner [func rem]
		 (if (empty? rem)
		   [func]
		   (let [[preimg imgs] (first rem)]
		     (for [img imgs,
			   fun (runner (assoc func preimg img)
				       (rest rem))]
		       fun))))]
    (runner {} (seq endo-hash))))

(defn- endofunctions-by-homomorphism ; name is misleading
  "Returns all endofunctions obtained by homomorphism."
  [context hom]
  (endofunctions-from-hash (endofunctions-as-hash context hom)))

(defn- retract?
  "Tests whether given concept endofunction, given as hash-map, is a
  retract of context or not."
  [context endo]
  (let [zero-concept [(objects context),
		      (object-derivation context (objects context))]
	one-concept  [(attribute-derivation context (attributes context)),
		      (attributes context)]]
    (and (= zero-concept (endo zero-concept))
	 (= one-concept (endo one-concept))
	 (let [imgs (vals endo)
	       img-ctx (make-context (objects context)
				     (attributes context)
				     (set-of [g m] [[objs atts] imgs,
						    g objs,
						    m atts]))]
	   (= (count (concepts context))
	      (count (concepts img-ctx)))))))

(defn retracts
  "Returns all retracts of context as computed by the algorithm of
  Felix Kästner."
  [context]
  (binding [concepts (memoize concepts)]
    (let [concepts (concepts context)]
      (for [hom (homomorphisms-by-cscs context)
	    endo (endofunctions-by-homomorphism context hom)
	    :let [ret (apply hash-map
			     (interleave concepts
					 (map (comp endo hom) concepts)))]
	    :when (retract? context ret)]
	ret))))

nil