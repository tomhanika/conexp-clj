;; Copyright (c) Daniel Borchmann. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.contrib.dl.languages.description-graphs
  (:use conexp
	conexp.contrib.dl.framework.syntax
	conexp.contrib.dl.framework.models
	conexp.contrib.dl.framework.boxes)
  (:use clojure.contrib.pprint
	[clojure.contrib.graph :exclude (transitive-closure)])
  (:import [java.util HashMap]))

;;;

(deftype Description-Graph [language vertices neighbours vertex-labels])

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
  (Description-Graph language vertices neighbours vertex-labels))

(defn edges
  "Returns labeld edges of given description graph."
  [description-graph]
  (set-of [A r B] [A (vertices description-graph),
		   [r B] ((neighbours description-graph) A)]))

(defmethod print-method ::Description-Graph [dg out]
  (let [#^String output (with-out-str
			  (pprint (list 'Description-Graph
					(vertices dg)
					(neighbours dg)
					(vertex-labels dg))))]
    (.write out (.trim output))))

;;; Normalizing

(defn- uniquify-tbox-map
  "Renames all defined concepts in the given tbox-map to be globally
  unique symbols. Returns a pair of the result and the transformation
  map used."
  [tbox-map]
  (let [old->new (into {} (for [A (keys tbox-map)]
			    [A (gensym)]))]
    [(into {} (for [[A def-A] tbox-map]
		[(old->new A) (set (map #(substitute % old->new) def-A))]))
     old->new]))

(defn- conjunctors
  "Returns the elements of the dl-expression connected by
  conjunction. If the dl-expression is not a conjunction the singelton
  set containing the expression is returned."
  [dl-expr]
  (if (and (compound? dl-expr)
	   (= 'and (operator dl-expr)))
    (set (arguments dl-expr))
    (set [dl-expr])))

(defn- tbox->hash-map
  "Transforms given TBox to a hash-map of defined concepts to the sets
  of concepts in the top-level conjunction."
  [tbox]
  (into {} (for [def (tbox-definitions tbox)]
	     [(definition-target def) (conjunctors (definition-expression def))])))

(defn- hash-map->tbox
  "Transforms given hash-map to a TBox for the given language."
  [language tbox-map]
  (let [definitions (for [[A def-A] tbox-map]
		      (make-dl-definition language A (cons 'and def-A)))]
    (make-tbox language (set definitions))))

(defn- normalize-for-goal [term goal]
  (cond
   (goal term) [term {}]
   (tbox-target-pair? term) (let [[tbox target] (expression term),
				  [tbox-map trans] (uniquify-tbox-map (tbox->hash-map tbox))]
			      [(trans target) tbox-map])
   :else (let [new-sym (gensym)]
	   [new-sym {new-sym (conjunctors term)}])))

(defn- introduce-auxiliary-definitions
  "Introduces auxiliary definitions into the given tbox-map (as
  returned by tbox->hash-map), such that the top-level conjunctions of
  all defined concepts only consist of defined concepts, primitive
  concepts or existential restrictions of defined concepts."
  [tbox-map]
  (if (empty? tbox-map)
    tbox-map
    (let [normalizer (fn [term]
		       (if (and (compound? term)
				(= 'exists (operator term)))
			 (let [[r B] (arguments term),
			       [norm new-defs] (normalize-for-goal B #(and (atomic? %) (not (primitive? %))))]
			   [(make-dl-expression (expression-language term)
						(list 'exists r norm))
			    new-defs])
			 (normalize-for-goal term atomic?))),
	  normalized-terms (map (fn [[A def-A]]
				  [A (set (map normalizer def-A))])
				tbox-map)]
      (reduce (fn [tbox-map [target set-of-pairs]]
		(apply merge tbox-map [target (set (map first set-of-pairs))]
		                      (introduce-auxiliary-definitions (apply merge (map second set-of-pairs)))))
	      {}
	      normalized-terms))))

(defn- normalize-gfp
  "Normalizes given TBox with gfp-semantics."
  [tbox]
  (let [language (tbox-language tbox)
	tbox-map (tbox->hash-map tbox)
	extended-hash-map (introduce-auxiliary-definitions tbox-map)
	result-box (hash-map->tbox language extended-hash-map)]
    result-box))


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
  "Returns the least common subsumer of A and B in tbox (in EL-gfp)."
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

(defn- post
  "Returns all neighbours of v in G, where the edges are labeled with
  r."
  [G r v]
  (set-of x [[s x] ((neighbours G) v)
	     :when (= r s)]))

(defn- pre
  "Returns all vertices in G which have v as its neighbour and whose
  connecting edge is labeled with r."
  [G r v]
  (set-of w [w (vertices G)
	     :when (contains? (post G r w) v)]))

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


;; refined (unfinished)

(defn- refined-simulator-sets
  "Implements RefinedSimliarity as presented by Henzinger, Henzinger
  and Kopke, adapted for computing the maximal simulation between the
  two description graphs G-1 and G-2."
  [G-1 G-2]
  (unsupported-operation "refined-simulator-sets not implemented.")
  (let [neighbours-1 (neighbours G-1),
	neighbours-2 (neighbours G-2),

	label-1 (vertex-labels G-1),
	label-2 (vertex-labels G-2),

	#^HashMap prevsim  (HashMap.),
	#^HashMap sim-sets (HashMap.)]
    (doseq [v (vertices G-1)]
      (.put prevsim v (vertices G-2))
      (.put sim-sets v
	    (if (empty? (neighbours-1 v))
	      (set-of u [u (vertices G-2)
			 :when (subset? (label-1 v) (label-2 u))])
	      (set-of u [u (vertices G-2)
			 :when (and (subset? (label-1 v) (label-2 u))
				    (not (empty? (neighbours-2 u))))]))))
    (loop []
      (when-let [v (first (filter #(not= (.get sim-sets %)
					 (.get prevsim %))
				  (vertices G-1)))]
	))))

;; efficient (wrong)

(defn- efficient-initialize
  "Returns initialization for sim-sets and remove-sets for
  EfficientSimliarity. Returns pair of sim-sets and remove-sets, both
  as transient data structures."
  [G-1 G-2]
  (let [label-1      (vertex-labels G-1),
	label-2      (vertex-labels G-2),

	#^HashMap sim-sets    (HashMap.),
	#^HashMap remove-sets (HashMap.)]
    (doseq [v (vertices G-1)]
      (.put sim-sets v
	    (if (empty? (post G-1 v))
	      (set-of u [u (vertices G-2),
			 :when (subset? (label-1 v) (label-2 u))])
	      (set-of u [u (vertices G-2),
			 :when (and (subset? (label-1 v) (label-2 u))
				    (not (empty? (post G-2 u))))])))
      (.put remove-sets v
	    (difference (set-of u [[u _ _] (edges G-2)])
			(set-of u [[u _ w] (edges G-2)
				   :when (contains? (.get sim-sets v) w)]))))
    [sim-sets remove-sets]))

(defn- efficient-simulator-sets
  "Implements (as far as I can see) the EfficientSimilarity Algorithm
  from \"Computing Simulations on Finite and Infinite Graphs\" by
  Henzinger, Henzinger and Kopke, adapted for computing the maximal
  simulation from G-1 to G-2."
  [G-1 G-2]
  (unsupported-operation "efficient-simulator-sets not implemented.")
  (let [[#^HashMap sim-sets, #^HashMap remove-sets] (efficient-initialize G-1 G-2)]
    (loop []
      (let [v (first (filter (fn [v]
			       (not (empty? (.get remove-sets v))))
			     (vertices G-1)))]
	(if-not v
	  (HashMap->hash-map sim-sets)
	  (do
	    ;; u and v are vertices of G-1
	    ;; w and x are vertices of G-2
	    (doseq [u (pre G-1 v)]
	      (doseq [w (.get remove-sets v)]
		(when (contains? (.get sim-sets u) w)
		  (.put sim-sets u
			(disj (.get sim-sets u) w))
		  (doseq [x (pre G-2 w)]
		    (when (empty? (intersection (post G-2 x) (.get sim-sets u)))
		      (.put remove-sets u
			    (conj (.get remove-sets u) x)))))))
	    (.put remove-sets v #{})
	    (recur)))))))

;;

(defn simulates?
  "Returns true iff there exists a simulation from G-1 to G-2, where
  vertex v in G-1 simulates vertex w in G-2."
  [G-1 G-2 v w]
  (let [sim-sets (simulator-sets G-1 G-2)]
    (contains? (get sim-sets v) w)))

;;;

nil
