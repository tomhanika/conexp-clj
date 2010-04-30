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
	conexp.contrib.dl.framework.boxes
	conexp.contrib.dl.framework.semantics
        conexp.contrib.dl.util.graphs)
  (:use clojure.contrib.pprint)
  (:import [java.util HashMap HashSet]))

(update-ns-meta! conexp.contrib.dl.languages.description-graphs
  :doc "Implements description graphs and common operations on them.")

;;;

(defrecord Description-Graph [language vertices neighbours vertex-labels])

(defn vertices
  "Returns vertices of given description graph."
  [#^Description-Graph description-graph]
  (.vertices description-graph))

(defn neighbours
  "Returns a function mapping vertices to sets of pairs of roles and
  names."
  [#^Description-Graph description-graph]
  (.neighbours description-graph))

(defn vertex-labels
  "Returns vertex labeling function of given description graph."
  [#^Description-Graph description-graph]
  (.vertex-labels description-graph))

(defn graph-language
  "Returns the underlying description language of the given
  description graph."
  [#^Description-Graph description-graph]
  (.language description-graph))

(defn make-description-graph
  "Creates and returns a description graph for the given arguments."
  [language vertices neighbours vertex-labels]
  (Description-Graph. language vertices neighbours vertex-labels))

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
			     [(expression-term A) B]))]
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
  (let [definitions (into {} (for [[A def-A] tbox-map]
                               [(expression-term A) (make-dl-definition language (expression-term A) (cons 'and def-A))]))]
    (make-tbox language definitions)))

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
   (tbox-target-pair? term) (let [[tbox target] (expression-term term),
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
    (make-directed-graph defined-concepts
                         (fn [C]
                           (filter #(contains? defined-concepts %)
                                   (tbox-map C))))))

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
				    (set (map #(vec (map expression-term (arguments %)))
					      (filter compound?
						      (arguments (definition-expression def)))))])),
	vertex-labels   (into {} (for [def definitions]
				   [(definition-target def),
				    (set (map expression-term
					      (filter atomic?
						      (arguments (definition-expression def)))))]))]
    (make-description-graph language vertices neighbours vertex-labels)))

(defn description-graph->tbox
  "Converts a description graph to a tbox."
  [description-graph]
  (let [language    (graph-language description-graph),
	labels      (vertex-labels description-graph),
	neighbours  (neighbours description-graph),

	definitions (into {} (for [A (vertices description-graph)
                                   :let [def-exp (make-dl-expression language
                                                                     (list* 'and
                                                                            (concat (labels A)
                                                                                    (for [[r B] (neighbours A)]
                                                                                      (list 'exists r B)))))]]
                               [A (make-dl-definition A def-exp)]))]
    (make-tbox language definitions)))

(defn model->description-graph
  "Converts given model to a description graph."
  [model]
  (let [language            (model-language model),
	interpretation      (model-interpretation model),

	vertices            (model-base-set model),
	neighbours          (fn [x]
                              (set-of [r y] [r (role-names language),
                                             [_ y] (filter #(= (first %) x) (interpretation r))])),                                                  
	vertex-labels       (fn [x]
                              (set-of P [P (concept-names language),
                                         :when (contains? (interpretation P) x)]))]
    (make-description-graph language vertices neighbours vertex-labels)))

(defn model->tbox
  "Converts a given model to its corresponding tbox."
  [model]
  (description-graph->tbox (model->description-graph model)))

;;;

(defn graph-product
  "Returns the product of the two description graphs given."
  [graph-1 graph-2]
  (let [language      (graph-language graph-1),
	vertices      (for [a (vertices graph-1),
                            b (vertices graph-2)]
                        [a b])
	neighbours    (fn [[A B]]
                        (set-of [r [C D]] [[r C] ((neighbours graph-1) A),
                                           [s D] ((neighbours graph-2) B),
                                           :when (= r s)])),
	vertex-labels (fn [[A B]]
                        (intersection ((vertex-labels graph-1) A)
                                      ((vertex-labels graph-2) B)))]
 (make-description-graph language vertices neighbours vertex-labels)))

;;; simulations

(defn- HashMap->hash-map
  "Converts a Java HashMap to a Clojure hash-map."
  [#^HashMap map]
  (into {} (for [k (.keySet map)]
	     [k (.get map k)])))

(defmacro- while-let
  "Runs body with binding in effect as long as x is non-nil.

  binding => [x xs]"
  [binding & body]
  `(loop []
     (when-let ~binding
       ~@body
       (recur))))

;; schematic

(defn schematic-simulator-sets
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

    (while-let [[u w] (first (for [u (vertices G-1),
				   [r v] (neighbours-1 u),
				   w (.get sim-sets u)
				   :when (not (exists [x (.get sim-sets v)]
						(edge-2? w r x)))]
			       [u w]))]
      (.put sim-sets u
	    (disj (.get sim-sets u) w)))

    (HashMap->hash-map sim-sets)))

;; efficient simulator sets (by meng)

(defn- post
  "Computes all vertices w in G such that there exists an edge from w
  to v labeled with r."
  [G v r]
  (set-of w [[s w] ((neighbours G) v)
	     :when (= s r)]))

(defn- pre
  "Computes all vertices w in G such that there exists an edge from v
  to w labeled with r."
  [G v r]
  (set-of w [w (vertices G),
	     :when (contains? ((neighbours G) w) [r v])]))

(defn- efficient-initialize
  "Returns tripel [sim, remove, pre*] as needed by
  efficient-simulator-sets. sim, remove and pre* are Java HashMaps."
  [G-1 G-2]
  (let [#^HashMap sim    (HashMap.),
	#^HashMap remove (HashMap.),
	#^HashMap pre*   (HashMap.),

	label-1 (vertex-labels G-1),
	label-2 (vertex-labels G-2),

	R (role-names (graph-language G-1))]
    (doseq [v (vertices G-1)]
      (.put sim v
	    (set-of u [u (vertices G-2),
		       :when (and (subset? (label-1 v) (label-2 u))
				  (forall [r R]
				    (=> (empty? (post G-2 u r))
					(empty? (post G-1 v r)))))]))
      (doseq [r R]
	(.put remove [v r]
	      (set-of w [w (vertices G-2),
			 :let [post-w (post G-2 w r)]
			 :when (and (not (empty? post-w))
				    (empty? (intersection post-w (.get sim v))))]))))
    (doseq [w (vertices G-2)]
      (.put pre* w
	    (set-of [u r] [r R, u (pre G-2 w r)])))
    [sim remove pre*]))

(defn efficient-simulator-sets
  "Implements ELgfp-EfficientSimilaritiy (for the maximal simulation
  between two graphs) and returns the corresponding simulator sets."
  [G-1 G-2]
  (with-memoized-fns [post pre]
    (let [[sim remove pre*] (efficient-initialize G-1 G-2),

	  neighbours-2 (neighbours G-2),
	  R (role-names (graph-language G-1)),

	  #^HashSet non-empty-removes (HashSet. (for [v (vertices G-1),
						      r R,
						      :when (not (empty? (.get remove [v r])))]
						  [v r]))]
      (while-let [[v r] (first non-empty-removes)]
	(doseq [u (pre G-1 v r),
		w (.get remove [v r])]
	  (when (contains? (.get sim u) w)
	    (.put sim u (disj (.get sim u) w))
	    (doseq [[w* r*] (.get pre* w)]
	      (when (not (exists [x (.get sim u)]
			   (contains? (neighbours-2 w*) [r* x])))
		(.put remove [u r*]
		      (conj (.get remove [u r*]) w*))
		(.add non-empty-removes [u r*])))))
	(.put remove [v r] #{})
	(.remove non-empty-removes [v r]))
      (HashMap->hash-map sim))))

;; simulation invocation point

(defvar *simulator-set-algorithm* schematic-simulator-sets
  "Algorithm to use when computing simulator sets between two description graphs.")

(defn simulates?
  "Returns true iff there exists a simulation from G-1 to G-2, where
  vertex v in G-1 simulates vertex w in G-2."
  [G-1 G-2 v w]
  (let [sim-sets (*simulator-set-algorithm* G-1 G-2)]
    (contains? (get sim-sets v) w)))

;;;

nil
