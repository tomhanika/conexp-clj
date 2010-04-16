;; Copyright (c) Daniel Borchmann. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.contrib.dl.languages.description-graphs
  (:use conexp.main
	conexp.contrib.dl.framework.syntax
	conexp.contrib.dl.framework.models
	conexp.contrib.dl.framework.boxes)
  (:use clojure.contrib.pprint
	[clojure.contrib.graph :exclude (transitive-closure)])
  (:import [java.util HashMap]))

;;;

(defrecord Description-Graph [language vertices neighbours vertex-labels])

(defn vertices
  "Returns vertices of given description graph."
  [description-graph]
  (:vertices description-graph))

(defn neighbours
  "Returns a function mapping vertices to sets of pairs of roles and
  names."
  [description-graph]
  (:neighbours description-graph))

(defn vertex-labels
  "Returns vertex labeling function of given description graph."
  [description-graph]
  (:vertex-labels description-graph))

(defn graph-language
  "Returns the underlying description language of the given
  description graph."
  [description-graph]
  (:language description-graph))

(defn make-description-graph
  "Creates and returns a description graph for the given arguments."
  [language vertices neighbours vertex-labels]
  (Description-Graph. language vertices neighbours vertex-labels))

(defn edges
  "Returns labeld edges of given description graph."
  [description-graph]
  (set-of [A r B] [A (vertices description-graph),
		   [r B] ((neighbours description-graph) A)]))

(defmethod print-method Description-Graph [dg out]
  (let [#^String output (with-out-str
			  (pprint (list 'Description-Graph
					(vertices dg)
					(neighbours dg)
					(vertex-labels dg))))]
    (.write out (.trim output))))

;;; Normalizing

(defn- conjunctors
  "Returns the elements of the dl-expression connected by
  conjunction. If the dl-expression is not a conjunction the singelton
  set containing the expression is returned."
  [dl-expr]
  (if (and (compound? dl-expr)
	   (= 'and (operator dl-expr)))
    (set (arguments dl-expr))
    (set [dl-expr])))

;; tboxes as hash-maps

(defn- uniquify-tbox-map
  "Renames all defined concepts in the given tbox-map to be globally
  unique symbols. Returns a pair of the result and the transformation
  map used."
  [tbox-map]
  (let [old->new (into {} (for [A (keys tbox-map)]
			    [A (make-dl-expression (expression-language A) (gensym))])),
	old->new* (into {} (for [[A B] old->new]
			     [(expression A) B]))]
    [(into {} (for [[A def-A] tbox-map]
		[(old->new A) (set (map #(substitute % old->new*) def-A))]))
     old->new]))

(defn- tbox->hash-map
  "Transforms given TBox to a hash-map of defined concepts to the sets
  of concepts in the top-level conjunction."
  [tbox]
  (let [language (tbox-language tbox)]
    (into {} (for [def (tbox-definitions tbox)]
	       [(make-dl-expression language (definition-target def))
		(conjunctors (definition-expression def))]))))

(defn- hash-map->tbox
  "Transforms given hash-map to a TBox for the given language."
  [language tbox-map]
  (let [definitions (for [[A def-A] tbox-map]
		      (make-dl-definition language (expression A) (cons 'and def-A)))]
    (make-tbox language (set definitions))))

;; storing names

(defn- new-names
  "Returns a fresh data structure for storing new names."
  []
  (atom {}))

(defn- add-name
  "Adds to names the set-of-concepts under name."
  [names name set-of-concepts]
  (swap! names conj [name set-of-concepts]))

(defn- add-names
  "Adds the tbox-map to names."
  [names tbox-map]
  (swap! names into tbox-map))

(defn- get-names
  "Returns all names and their definitions (i.e. their set of
  concepts) stored in names."
  [names]
  @names)

;; normalizing algorithm - preparing the tbox and introducing new definitions

(defn- normalize-for-goal
  "If term satisfies goal, regards it as being normalized. Otherwise
  handles term as tbox and finally introduces a new symbol defining
  term. New definitions go into the atom new-names."
  [term goal new-names]
  (cond
   (goal term) term
   (tbox-target-pair? term) (let [[tbox target] (expression term),
				  [tbox-map trans] (uniquify-tbox-map (tbox->hash-map tbox))]
			      (add-names new-names tbox-map)
			      (trans (make-dl-expression (expression-language term) target)))
   :else (let [new-sym (make-dl-expression (expression-language term) (gensym))]
	   (add-name new-names new-sym (conjunctors term))
	   new-sym)))

(defn- normalize-term
  "Normalizes conjunctor term. New definitions go into the atom new-names."
  [term new-names]
  (if (and (compound? term)
	   (= 'exists (operator term)))
    (let [[r B] (arguments term),
	  norm (normalize-for-goal B
				   #(and (atomic? %)
					 (not (tbox-target-pair? %))
					 (not (primitive? %)))
				   new-names)]
      (make-dl-expression (expression-language term)
			  (list 'exists r norm)))
    (normalize-for-goal term
			#(and (atomic? %)
			      (not (tbox-target-pair? %)))
			new-names)))

(defn- introduce-auxiliary-definitions
  "Introduces auxiliary definitions into the given tbox-map (as
  returned by tbox->hash-map), such that the top-level conjunctions of
  all defined concepts only consist of defined concepts, primitive
  concepts or existential restrictions of defined concepts."
  [tbox-map]
  (if (empty? tbox-map)
    tbox-map
    (let [new-names (new-names),
	  ;; doall needed to store new names in new-names!
	  normalized-map (doall
			  (into {} (map (fn [[A def-A]]
					  [A (set (map #(normalize-term % new-names) def-A))])
					tbox-map))),
	  new-names-map (introduce-auxiliary-definitions (get-names new-names))]
      (into normalized-map new-names-map))))

;; normalizing algorithm -- squeezing the concept graph

(defn- concept-graph
  "Returns the concept graph of a tbox-map. The graph has the defined
  conecpts of tbox as vertices and connects every two vertices C to D
  if D appears in the top-level conjunction of C."
  [tbox-map]
  (let [defined-concepts (set (keys tbox-map))]
    (struct directed-graph
	    defined-concepts
	    (hashmap-by-function (fn [C]
				   (filter #(contains? defined-concepts %)
					   (tbox-map C)))
				 defined-concepts))))

(defn- squeeze-equivalent-concepts
  "Returns a tbox-map where all equivalent, defined concepts of
  tbox-map are squeezed into one. If A is such a concept, every
  equivalent defined concept used in other definitions is substituted
  by A."
  [tbox-map]
  (let [equivalent-concepts (scc (concept-graph tbox-map)),
	rename-map (into {} (for [concepts equivalent-concepts
				  concept concepts]
			      [concept (first concepts)])),
	used-map (into {} (for [concepts equivalent-concepts]
			    [(first concepts) concepts])),
	new-tbox-map (into {} (map (fn [target]
				     [target
				      (disj (set (replace rename-map
							  (mapcat tbox-map (used-map target))))
					    target)])
				   (vals rename-map)))]
    (into {} (for [target (keys tbox-map)]
	       [target (-> target rename-map new-tbox-map)]))))

(defn- replace-toplevel-concepts
  "Replaces any top-level defined concept in tbox-map by its definition."
  [tbox-map]
  (loop [deps (dependency-list (concept-graph tbox-map)),
	 new-tbox-map {}]
    (if (empty? deps)
      new-tbox-map
      (let [next-concepts (first deps),
	    new-defs (into {} (for [target next-concepts]
				[target (reduce (fn [result next-thing]
						  (if (set? next-thing)
						    (into result next-thing)
						    (conj result next-thing)))
						#{}
						(replace new-tbox-map (tbox-map target)))]))]
	(recur (rest deps)
	       (merge new-tbox-map new-defs))))))


;; normalizing algorithm -- invokation point

(defn normalize-gfp
  "Normalizes given TBox with gfp-semantics."
  [tbox]
  (let [language   (tbox-language tbox)
	result-map (-> tbox
		       tbox->hash-map
		       introduce-auxiliary-definitions
		       squeeze-equivalent-concepts
		       replace-toplevel-concepts)]
    (hash-map->tbox language result-map)))


;;; Conversion to and from description graphs

(defn tbox->description-graph
  "Converts a tbox to a description graph. Normalization is done with gfp semantics."
  [tbox]
  (let [tbox            (normalize-gfp tbox),
	definitions     (tbox-definitions tbox),

	language        (tbox-language tbox),
	vertices        (defined-concepts tbox),
	neighbours      (into {} (for [def definitions]
				   [(definition-target def)
				    (set (map #(vec (map expression (arguments %)))
					      (filter compound?
						      (arguments (definition-expression def)))))])),
	vertex-labels   (into {} (for [def definitions]
				   [(definition-target def),
				    (set (map expression
					      (filter atomic?
						      (arguments (definition-expression def)))))]))]
    (make-description-graph language vertices neighbours vertex-labels)))

(defn description-graph->tbox
  "Converts a description graph to a tbox."
  [description-graph]
  (let [language    (graph-language description-graph),
	labels      (vertex-labels description-graph),
	neighbours  (neighbours description-graph),

	definitions (set-of (make-dl-definition A def-exp)
			    [A (vertices description-graph)
			     :let [def-exp (make-dl-expression language
							       (list* 'and
								      (concat (labels A)
									      (for [[r B] (neighbours A)]
										(list 'exists r B)))))]])]
    (make-tbox language definitions)))

(defn model->description-graph
  "Converts given model to a description graph."
  [model]
  (let [language            (model-language model),
	interpretation      (model-interpretation model),

	vertices            (model-base-set model),
	neighbours          (hashmap-by-function (fn [x]
						   (set-of [r y] [r (role-names language),
								  y (model-base-set model),
								  :when (contains? (interpretation r) [x y])]))
						 (model-base-set model)),
	vertex-labels       (hashmap-by-function (fn [x]
						   (set-of P [P (concept-names language),
							      :when (contains? (interpretation P) x)]))
						 (model-base-set model))]
    (make-description-graph language vertices neighbours vertex-labels)))

;;;

(defn graph-product
  "Returns the product of the two description graphs given."
  [graph-1 graph-2]
  (let [language      (graph-language graph-1),
	vertices      (cross-product (vertices graph-1) (vertices graph-2)),
	neighbours    (hashmap-by-function (fn [[A B]]
					     (set-of [r [C D]] [[r C] ((neighbours graph-1) A),
								[s D] ((neighbours graph-2) B),
								:when (= r s)]))
					   vertices),
	vertex-labels (hashmap-by-function (fn [[A B]]
					     (intersection ((vertex-labels graph-1) A)
							   ((vertex-labels graph-2) B)))
					   vertices)]
 (make-description-graph language vertices neighbours vertex-labels)))

;;; least common subsumers in EL-gfp

(defn EL-gfp-lcs
  "Returns the least common subsumer (in EL-gfp) of A and B in tbox."
  ([tbox A]
     [tbox A])
  ([tbox A B]
     (let [G_T_1 (tbox->description-graph tbox)
	   G-x-G (graph-product G_T_1 G_T_1),
	   T_2   (tbox-union tbox (description-graph->tbox G-x-G))]
       (clarify-tbox [T_2, [A,B]])))
  ([tbox A B & more]
     (let [[new-tbox new-target] (EL-gfp-lcs tbox A B)]
       (apply EL-gfp-lcs (tbox-union tbox new-tbox) new-target more))))

;;; most specific concepts in EL-gfp for objects

(defn EL-gfp-object-msc
  "Returns the model based most specific concept of x in model."
  [model x]
  (clarify-tbox
   [(description-graph->tbox (model->description-graph model)), x]))

(defn EL-gfp-msc
  "Returns the model based most specific concept of args in model."
  [model & args]
  (if-not (empty? args)
    (let [tbox (reduce tbox-union
		       (map (comp first (partial EL-gfp-object-msc model))
			    args))]
      (apply EL-gfp-lcs tbox args))
    (let [language (model-language model),
	  all (make-dl-expression language
				  (list* 'and
					 (concat (concept-names language)
						 (for [r (role-names language)]
						   (list 'exists r 'All)))))]
      [(make-tbox language #{(make-dl-definition 'All all)}), 'All])))

;;; simulations

(defn- HashMap->hash-map
  "Converts a Java HashMap to a Clojure hash-map."
  [#^HashMap map]
  (into {} (for [k (.keySet map)]
	     [k (.get map k)])))

;; schematic

(defn- simulator-sets
  "Returns for all vertices v in the description graph G-1 the sets of
  vertices (sim v) in G-2 such that there exists a simulation from v to
  every vertex in (sim v)."
  [G-1 G-2]
  (let [label-1 (vertex-labels G-1),
	label-2 (vertex-labels G-2),
	neighbours-1 (neighbours G-1),
	neighbours-2 (neighbours G-2),
	edge-2? (fn [v r w]
		  (contains? (neighbours-2 v) [r w])),

	#^HashMap sim-sets (HashMap.)]
    (doseq [v (vertices G-1)]
      (.put sim-sets v (set-of w [w (vertices G-2)
				  :when (subset? (label-1 v) (label-2 w))])))
    (loop []
      (let [[u w] (first (for [u (vertices G-1),
			       [r v] (neighbours-1 u),
			       w (.get sim-sets u)
			       :when (forall [x (.get sim-sets v)]
				       (not (edge-2? w r x)))]
			   [u w]))]
	(when u
	  (.put sim-sets u
		(disj (.get sim-sets u) w))
	  (recur))))
    (HashMap->hash-map sim-sets)))

;; simulation invocation point

(defn simulates?
  "Returns true iff there exists a simulation from G-1 to G-2, where
  vertex v in G-1 simulates vertex w in G-2."
  [G-1 G-2 v w]
  (let [sim-sets (simulator-sets G-1 G-2)]
    (contains? (get sim-sets v) w)))

;;;

nil
