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
  (:use clojure.contrib.pprint))

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

(declare normalize)

(defn- get-name-for-expr
  "Returns name for a given expression, if known."
  [names expr]
  (let [syms (get names expr)]
    (and syms (first syms))))

(defn- add-name-for-expr
  "Adds name for expr to names."
  [names expr name]
  (update-in names [expr] conj name))

(defn- normalize-atomic
  "Normalizes an atomic expression as needed by normalize-definition."
  [expr term-names]
  (cond
   (tbox-target-pair? expr) (let [[tbox target] (uniquify-tbox-target-pair (expression expr)),
				  tbox (normalize tbox term-names)]
			      [target (reduce (fn [term-names def]
						(let [def-expr (definition-expression def),
						      def-trgt (definition-target def)]
						  (add-name-for-expr term-names def-expr def-trgt)))
					      term-names
					      (tbox-definitions tbox))]),
   :else [expr term-names]))

(defn- normalize-definition
  "Normalizes given definition using term-names (i.e. a map mapping
  expressions to names), extending term-names if necessary."
  ;; ugly code, can we do this with a monad?
  [definition term-names]
  (let [language (expression-language (definition-expression definition)),
	target   (definition-target definition),
	expr     (let [expr (definition-expression definition)]
		   (if (or (not (compound? expr))
			   (not= 'and (operator expr)))
		     (make-dl-expression language (list 'and expr))
		     expr))]
    (loop [args       (vec (arguments expr)),
	   normalized [],
	   names      term-names]
      (if (empty? args)
	;; make definition
	(let [def-expression (make-dl-expression language (cons 'and normalized))]
	  [(make-dl-definition target def-expression) names])

	;; examine arguments
	(let [next-term (first args)]
	  (if (atomic? next-term)
	    (do
	      ;; this is only a restricted normalization algorithm
	      (when (and (not (primitive? next-term))
			 (not (tbox-target-pair? next-term)))
		(illegal-argument "Sorry, but this normalization algorithm cannot handle your TBox.\n"
				  "The definition " (print-str definition) " contains definied concepts at top-level."))

	      ;; atomic term, possibly a tbox-target-pair
	      (let [[normal-term new-names] (normalize-atomic next-term names)]
		(recur (rest args) (conj normalized normal-term) new-names)))

	    ;; next-term is an existential quantification
	    (let [[r B] (vec (arguments next-term))]
	      (if (and (atomic? B) (not (primitive? B)))
		;; atomic term not being a concept name, but possibly a tbox-target-pair
		(let [[normal-B new-names] (normalize-atomic B names)]
		  (recur (rest args) (conj normalized (list 'exists (expression r) normal-B)) new-names))

		;; B is compound
		(let [name (get-name-for-expr names B)]
		  (if-not (nil? name)
		    (recur (rest args) (conj normalized (list 'exists (expression r) name)) names)
		    (let [new-name (gensym)]
		      (recur (rest args) (conj normalized (list 'exists (expression r) new-name))
			     (add-name-for-expr names B new-name)))))))))))))

(defn- tbox-from-names
  "Creates and returns a tbox from given names and language."
  [language names]
  (make-tbox language (set-of (make-dl-definition A (make-dl-expression language def-A))
			      [[def-A As] names,
			       A As])))

(defn normalize
  "Normalizes given tbox."
  ([tbox]
     (normalize tbox {}))
  ([tbox names]
     (let [language (tbox-language tbox),
	   [normalized-definitions new-names] (reduce (fn [[n-definitions names] definition]
							(let [[n-definition new-names] (normalize-definition definition names)]
							  [(conj n-definitions n-definition) new-names]))
						      [#{} names]
						      (tbox-definitions tbox))]
       (if (= names new-names)
	 (make-tbox language normalized-definitions)
	 (let [names-tbox (normalize (tbox-from-names language new-names) new-names)]
	   (make-tbox language (union normalized-definitions
				      (tbox-definitions names-tbox))))))))


;;; Conversion to and from description graphs

(defn tbox->description-graph
  "Converts a tbox to a description graph."
  [tbox]
  (let [tbox            (normalize tbox),
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

(defn- simulator-sets
  "Returns for all vertices v in the description graph G-1 the sets of
  vertices (sim v) in G-2 such that there exists a simulation from v to
  every vertex in (sim v)."
  [G-1 G-2]
  (let [label-1 (vertex-labels G-1),
	label-2 (vertex-labels G-2),
	neighbours-1 (neighbours G-1),
	neighbours-2 (neighbours G-2),
	edge-1? (fn [v r w]
		  (contains? (neighbours-1 v) [r w])),
	edge-2? (fn [v r w]
		  (contains? (neighbours-2 v) [r w])),

	initial-sim-sets (hashmap-by-function (fn [v]
						(set-of w [w (vertices G-2)
							   :when (subset? (label-1 v) (label-2 w))]))
					      (vertices G-1))]
    (loop [sim-sets initial-sim-sets]
      (let [u-w (first (for [u (vertices G-1),
			     [r v] (neighbours-1 u)
			     w (sim-sets u),
			     :when (forall [x (sim-sets v)]
				     (not (edge-2? w r x)))]
			 [u w]))]
	(if (nil? u-w)
	  sim-sets
	  (recur (update-in sim-sets [(first u-w)] disj (second u-w))))))))

(defn- post
  "Returns all neighbours of v in G."
  [G v]
  ((neighbours G) v))

(defn- pre
  "Returns all vertices in G which have v as its neighbour."
  [G v]
  (set-of w [w (vertices G)
	     :when (contains? (neighbours w) v)]))

(defn- efficient-initialize
  "Returns initialization for sim-sets and remove-sets for
  EfficientSimliarity. Returns pair of sim-sets and remove-sets, both
  as transient data structures."
  [G-1 G-2]
  (let [label-1      (vertex-labels G-1),
	label-2      (vertex-labels G-2),

	sim-sets     (transient {}),
	remove-sets  (transient {})]
    (doseq [v (vertices G-1)]
      (assoc! sim-sets v
	      (if (empty? (post G-1 v))
		(set-of u [u (vertices G-2),
			   :when (subset? (label-1 v) (label-2 u))])
		(set-of u [u (vertices G-2),
			   :when (and (subset? (label-1 v) (label-2 u))
				      (not (empty? (post G-2 u))))])))
      (assoc! remove-sets v
	      (difference (set-of u [[u _ _] (edges G-2)])
			  (set-of u [[u _ w] (edges G-2)
				     :when (contains? (sim-sets v) w)]))))
    [sim-sets remove-sets]))

(defn- efficient-simulator-sets
  "Implements (as far as I can see) the EfficientSimilarity Algorithm
  from \"Computing Simulations on Finite and Infinite Graphs\" by
  Henzinger, Henzinger and Kopke, adapted for computing the maximal
  simulation from G-1 to G-2."
  [G-1 G-2]
  (let [[sim-sets remove-sets] (efficient-initialize G-1 G-2)]
    (loop []
      (let [v (first (filter (fn [v]
			       (not (empty? (remove-sets v))))
			     (vertices G-1)))]
	(if-not v
	  (persistent! sim-sets)
	  (do
	    (doseq [u (pre G-1 v)]
	      (doseq [w (remove-sets v)]
		(when (contains? (sim-sets u) w)
		  (assoc! sim-sets u
			  (disj (sim-sets u) w))
		  (assoc! remove-sets u
			  (into (remove-sets u)
				(filter #(empty? (intersection (post G-2 %)
							       (sim-sets u)))
					(pre G-2 w)))))))
	    (assoc! remove-sets v #{})
	    (recur)))))))

(defn simulates?
  "Returns true iff there exists a simulation from G-1 to G-2, where
  vertex v in G-1 simulates vertex w in G-2."
  [G-1 G-2 v w]
  (let [sim-sets (simulator-sets G-1 G-2)]
    (contains? (get sim-sets v) w)))

;;;

nil
