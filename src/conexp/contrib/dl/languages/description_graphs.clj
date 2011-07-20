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
  (:use clojure.pprint)
  (:import [java.util HashMap HashSet]))

(ns-doc
 "Implements description graphs and common operations on them.")

;;;

(defrecord Description-Graph [language vertices neighbours vertex-labels])

(defn vertices
  "Returns vertices of given description graph."
  [^Description-Graph description-graph]
  (.vertices description-graph))

(defn neighbours
  "Returns a function mapping vertices to sets of pairs of roles and
  names."
  [^Description-Graph description-graph]
  (.neighbours description-graph))

(defn vertex-labels
  "Returns vertex labeling function of given description graph."
  [^Description-Graph description-graph]
  (.vertex-labels description-graph))

(defn graph-language
  "Returns the underlying description language of the given
  description graph."
  [^Description-Graph description-graph]
  (.language description-graph))

(defn make-description-graph
  "Creates and returns a description graph for the given arguments."
  [language vertices neighbours vertex-labels]
  (Description-Graph. language vertices neighbours vertex-labels))

(defmethod print-method Description-Graph [dg out]
  (let [^String output (with-out-str
                         (pprint (list 'Description-Graph
                                       (vertices dg)
                                       (neighbours dg)
                                       (vertex-labels dg))))]
    (.write ^java.io.Writer out (.trim output))))

;;; Normalizing

(defn- conjunctors
  "Returns the elements of the dl-expression connected by
  conjunction. If the dl-expression is not a conjunction the singleton
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
  (let [old->new  (map-by-fn (fn [A]
                               (make-dl-expression (expression-language A) (gensym)))
                             (keys tbox-map)),
        old->new* (reduce! (fn [map [A B]]
                             (assoc! map (expression-term A) B))
                           {}
                           old->new)]
    [(reduce! (fn [map [A def-A]]
                (assoc! map
                        (old->new A)
                        (set-of (substitute term old->new*)
                                [term def-A])))
              {}
              tbox-map)
     old->new]))

(defn- tbox->hash-map
  "Transforms given TBox to a hash-map of defined concepts to the sets
  of concepts in the top-level conjunction."
  [tbox]
  (let [language (tbox-language tbox)]
    (reduce! (fn [map def]
               (assoc! map
                       (make-dl-expression language (definition-target def))
                       (conjunctors (definition-expression def))))
             {}
             (vals (tbox-definition-map tbox)))))

(defn- hash-map->tbox
  "Transforms given hash-map to a TBox for the given language."
  [language tbox-map]
  (make-tbox language
             (reduce! (fn [map [A def-A]]
                        (assoc! map
                                (expression-term A)
                                (make-dl-definition language
                                                    (expression-term A)
                                                    (cons 'and def-A))))
                      {}
                      tbox-map)))

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
   (goal term) term,
   (tbox-target-pair? term)
   (let [[tbox target] (expression-term term),
         [tbox-map trans] (uniquify-tbox-map (tbox->hash-map tbox))]
     (add-names new-names tbox-map)
     (trans (make-dl-expression (expression-language term) target))),
   :else
   (let [new-sym (make-dl-expression (expression-language term) (gensym))]
     (add-name new-names new-sym (conjunctors term))
     new-sym)))

(defn- normalize-term
  "Normalizes conjunctor term. New definitions go into the atom new-names."
  [term new-names]
  (cond
   (and (compound? term)
        (contains? #{'bottom 'top} (operator term)))
   term,
   (and (compound? term)
        (= 'exists (operator term)))
   (let [[r B] (arguments term),
         norm  (normalize-for-goal B
                                   #(and (atomic? %)
                                         (not (tbox-target-pair? %))
                                         (not (primitive? %)))
                                   new-names)]
     (make-dl-expression (expression-language term)
                         (list 'exists r norm))),
   :else
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
  (loop [tbox-map       tbox-map,
         normalized-map {}]
    (if (empty? tbox-map)
      normalized-map
      (let [new-names      (new-names),
            normalized-map (into normalized-map
                                 (reduce! (fn [map [A def-A]]
                                            (assoc! map A (set-of (normalize-term t new-names)
                                                                  [t def-A])))
                                          {}
                                          tbox-map)),
            new-map        (get-names new-names)]
        (recur new-map normalized-map)))))

;; normalizing algorithm -- squeezing the concept graph

(defn- concept-graph
  "Returns the concept graph of a tbox-map. The graph has the defined
  concepts of tbox as vertices and connects every two vertices C to D
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
        rename-map          (reduce! (fn [map concepts]
                                       (let [new-concept (first concepts)]
                                         (reduce (fn [map concept]
                                                   (assoc! map concept new-concept))
                                                 map
                                                 concepts)))
                                     {}
                                     equivalent-concepts),
        used-map            (reduce! (fn [map concepts]
                                       (assoc! map (first concepts) concepts))
                                     {}
                                     equivalent-concepts),
        new-tbox-map        (reduce! (fn [map target]
                                       (assoc! map
                                               target
                                               (disj (set (replace rename-map
                                                                   (mapcat tbox-map (used-map target))))
                                                     target)))
                                     {}
                                     (vals rename-map))]
    (map-by-fn (comp new-tbox-map rename-map)
               (keys tbox-map))))

(defn- replace-toplevel-concepts
  "Replaces any top-level defined concept in tbox-map by its definition."
  [tbox-map]
  (loop [deps (dependency-list (concept-graph tbox-map)),
         new-tbox-map {}]
    (if (empty? deps)
      new-tbox-map
      (let [next-concepts (first deps),
            new-defs      (map-by-fn (fn [target]
                                       (reduce (fn [result next-thing]
                                                 (if (set? next-thing)
                                                   (into result next-thing)
                                                   (conj result next-thing)))
                                                #{}
                                                (replace new-tbox-map (tbox-map target))))
                                     next-concepts)]
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
  (let [tbox          (normalize-gfp tbox),
        language      (tbox-language tbox),
        vertices      (defined-concepts tbox),
        neighbours    (memo-fn _ [target]
                        (set-of (vec (map expression-term (arguments t)))
                                [t (arguments (definition-expression (find-definition tbox target)))
                                 :when (compound? t)])),
        vertex-labels (memo-fn _ [target]
                        (set-of (expression-term t)
                                [t (arguments (definition-expression (find-definition tbox target)))
                                 :when (atomic? t)]))]
    (make-description-graph language vertices neighbours vertex-labels)))

(defn description-graph->tbox
  "Converts a description graph to a tbox."
  [description-graph]
  (let [language    (graph-language description-graph),
        labels      (vertex-labels description-graph),
        neighbours  (neighbours description-graph),

        definitions (map-by-fn (fn [A]
                                 (make-dl-definition
                                  A
                                  (make-dl-expression language
                                                      (list* 'and
                                                             (concat (labels A)
                                                                     (for [[r B] (neighbours A)]
                                                                       (list 'exists r B)))))))
                               (vertices description-graph))]
    (make-tbox language definitions)))

(defn interpretation->description-graph
  "Converts given interpretation to a description graph."
  [interpretation]
  (let [language      (interpretation-language interpretation),
        int-func      (interpretation-function interpretation),
        vertices      (interpretation-base-set interpretation),
        
        neighbours    (memo-fn _ [x]
                        (set-of [r y] [r (role-names language),
                                       [_ y] (filter #(= (first %) x) (int-func r))])),
        vertex-labels (memo-fn _ [x]
                        (set-of P [P (concept-names language),
                                   :when (contains? (int-func P) x)]))]
    (make-description-graph language vertices neighbours vertex-labels)))

(defn interpretation->tbox
  "Converts a given interpretation to its corresponding tbox."
  [interpretation]
  (let [tbox (description-graph->tbox (interpretation->description-graph interpretation))
        tbox (if (not-empty (tbox-definitions tbox))
               (first (tidy-up-ttp [tbox (first (defined-concepts tbox))]))
               tbox)]
    tbox))

;;;

(defn graph-product
  "Returns the product of the two description graphs given. Returns the directed connected component
  of the graph product containing node, if given."
  ([graph-1 graph-2]
     (let [language      (graph-language graph-1),
           vertices      (cross-product (vertices graph-1)
                                        (vertices graph-2)),
           neighbours    (fn [[A B]]
                           (set-of [r [C D]] [[r C] ((neighbours graph-1) A),
                                              [s D] ((neighbours graph-2) B),
                                              :when (= r s)])),
           vertex-labels (fn [[A B]]
                           (intersection ((vertex-labels graph-1) A)
                                         ((vertex-labels graph-2) B)))]
       (make-description-graph language vertices neighbours vertex-labels)))
  ([graph-1 graph-2 node]
     (let [language      (graph-language graph-1),
           vertices      (loop [verts #{node},
                                newvs #{node}]
                           (if (empty? newvs)
                             verts
                             (let [nextvs (set-of [v w] | [x y] newvs
                                                          [r v] ((neighbours graph-1) x)
                                                          [s w] ((neighbours graph-2) y)
                                                          :when (= r s))]
                               (recur (into verts nextvs)
                                      (difference nextvs verts))))),

           neighbours    (fn [[A B]]
                           (set-of [r [C D]] [[r C] ((neighbours graph-1) A),
                                              [s D] ((neighbours graph-2) B),
                                              :when (= r s)])),
           vertex-labels (fn [[A B]]
                           (intersection ((vertex-labels graph-1) A)
                                         ((vertex-labels graph-2) B)))]
       (make-description-graph language vertices neighbours vertex-labels))))

(defn description-graph-component
  "Returns the directed connected component of desgraph containing node."
  [desgraph node]
  (let [new-vertices (loop [verts #{node},
                            newvs #{node}]
                       (if (empty? newvs)
                         verts
                         (let [nextvs (set-of v | w newvs [_ v] ((neighbours desgraph) w))]
                           (recur (into verts nextvs)
                                  (difference nextvs verts)))))]
    (make-description-graph (graph-language desgraph)
                            new-vertices
                            (neighbours desgraph)
                            (vertex-labels desgraph))))

;;; simulations

(defn- HashMap->hash-map
  "Converts a Java HashMap to a Clojure hash-map."
  [^HashMap map]
  (reduce! (fn [map, ^java.util.Map$Entry entry]
             (assoc! map (.getKey entry) (.getValue entry)))
           {}
           (.entrySet map)))

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
  (let [label-1      (vertex-labels G-1),
        label-2      (vertex-labels G-2),
        neighbours-1 (neighbours G-1),
        neighbours-2 (neighbours G-2),
        edge-2?      (fn [v r w]
                       (contains? (neighbours-2 v) [r w])),

        ^HashMap sim-sets (HashMap.)]

    (doseq [v (vertices G-1)]
      (.put sim-sets v (set-of w [w (vertices G-2)
                                  :when (subset? (label-1 v) (label-2 w))])))

    (while-let [[u w] (first (for [u     (vertices G-1),
                                   [r v] (neighbours-1 u),
                                   w     (.get sim-sets u)
                                   :when (not (exists [x (.get sim-sets v)]
                                                (edge-2? w r x)))]
                               [u w]))]
      (.put sim-sets u
            (disj (.get sim-sets u) w)))

    (HashMap->hash-map sim-sets)))


;; efficient simulator sets (by meng)

(defn- single-edge->double-edge-graph
  "Given a single-edged graph G (i.e. a graph with a neighbours
  function on it) returns a structure with a :pre and a :post function
  on it. This function is part of the implementation for
  efficient-simulator-sets."
  [G]
  (loop [pre-map    (transient {}),
         vertex-set (vertices G)]
    (if (not-empty vertex-set)
      (let [v (first vertex-set)]
        (recur (reduce #(assoc! %1 %2 (conj (get %1 %2) v))
                       pre-map
                       ((neighbours G) v))
               (rest vertex-set)))
      {:base-set (vertices G),
       :labels   (vertex-labels G),
       :post     (memo-fn _ [v r]
                   (set-of w [[s w] ((neighbours G) v)
                              :when (= s r)])),
       :pre      (memo-fn _ [v r]
                   (set (get pre-map [r v] nil)))})))

(defn efficient-initialize
  "Returns tripel [sim, remove, pre*] as needed by
  efficient-simulator-sets. sim, remove and pre* are Java HashMaps."
  [language G-1 G-2]
  (let [^HashMap sim    (HashMap.),
        ^HashMap remove (HashMap.),
        ^HashMap pre*   (HashMap.),

        label-1    (:labels G-1),
        label-2    (:labels G-2),
        base-set-1 (:base-set G-1),
        base-set-2 (:base-set G-2),
        post-1     (:post G-1),
        post-2     (:post G-2),
        pre-2      (:pre G-2),

        R (role-names language)]
    (doseq [v base-set-1]
      (.put sim v
            (set-of u [u base-set-2,
                       :when (and (subset? (label-1 v) (label-2 u))
                                  (forall [r R] (=> (empty? (post-2 u r))
                                                    (empty? (post-1 v r)))))]))
      (doseq [r R]
        (.put remove [v r]
              (set-of w [w base-set-2,
                         :let [post-w (post-2 w r)]
                         :when (not-empty post-w)
                         :let [sim-v (.get sim v)]
                         :when (forall [x post-w] (not (contains? sim-v x)))]))))
    (doseq [w base-set-2]
      (.put pre* w
            (set-of [u r] [r R, u (pre-2 w r)])))
    [sim remove pre*]))

(defn efficient-simulator-sets
  "Implements EL-gfp-EfficientSimilaritiy (for the maximal simulation
  between two graphs) and returns the corresponding simulator sets."
  [G-1 G-2]
  (let [L (graph-language G-1),
        R (role-names L),

        G-1 (single-edge->double-edge-graph G-1),
        G-2 (single-edge->double-edge-graph G-2),

        vars            (efficient-initialize L G-1 G-2),
        ^HashMap sim    (nth vars 0),
        ^HashMap remove (nth vars 1),
        ^HashMap pre*   (nth vars 2),

        ^HashSet non-empty-removes (HashSet.),

        base-set-1 (:base-set G-1),
        post-2     (:post G-2),
        pre-1      (:pre G-1)]
    (doseq [v base-set-1,
            r R,
            :when (not-empty (.get remove [v r]))]
      (.add non-empty-removes [v r]))
    (while-let [[v r] (first non-empty-removes)]
      (doseq [w (.get remove [v r]),
              u (pre-1 v r)]
        (when (contains? (.get sim u) w)
          (.put sim u
                (disj (.get sim u) w))
          (doseq [[w* r*] (.get pre* w),
                  :let [sim-u (.get sim u)]]
            (when (forall [x (post-2 w* r*)]
                    (not (contains? sim-u x)))
              (.put remove [u r*]
                    (conj (.get remove [u r*]) w*))
              (.add non-empty-removes [u r*])))))
      (.put remove [v r] #{})
      (.remove non-empty-removes [v r]))
    (HashMap->hash-map sim)))


;; simulation invocation point

(defn simulates?
  "Returns true iff there exists a simulation from G-1 to G-2, where
  vertex v in G-1 simulates vertex w in G-2."
  [G-1 G-2 v w]
  (let [sim-sets (efficient-simulator-sets G-1 G-2)]
    (contains? (get sim-sets v) w)))

;;;

nil
