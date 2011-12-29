;; Copyright (c) Jeffrey Straszheim. All rights reserved. The use and
;; distribution terms for this software are covered by the Eclipse Public
;; License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which can
;; be found in the file epl-v10.html at the root of this distribution. By
;; using this software in any fashion, you are agreeing to be bound by the
;; terms of this license. You must not remove this notice, or any other,
;; from this software.
;;
;; graph
;;
;; Basic Graph Theory Algorithms
;;
;; straszheimjeffrey (gmail)
;; Created 23 June 2009
;;
;; with modifications by D. Borchmann for conexp-clj

(ns
    #^{:author "Jeffrey Straszheim",
       :doc "Basic graph theory algorithms"}
  conexp.util.graph
  (use [conexp.base :exclude (transitive-closure)]))

;;;

(defstruct directed-graph
  :nodes                        ; The nodes of the graph, a collection
  :neighbors)                   ; A function that, given a node returns a collection neighbor nodes.

(defn make-directed-graph
  "Constructs a directed graph."
  [nodes neighbour-fn]
  (struct directed-graph nodes neighbour-fn))

(defn get-neighbors
  "Get the neighbors of a node."
  [g n]
  ((:neighbors g) n))


;; Graph Modification

(defn reverse-graph
  "Given a directed graph, return another directed graph with the
order of the edges reversed."
  [g]
  (let [op (fn [rna idx]
             (let [ns (get-neighbors g idx)
                   am (fn [m val]
                        (assoc m val (conj (get m val #{}) idx)))]
               (reduce am rna ns)))
        rn (reduce op {} (:nodes g))]
    (struct directed-graph (:nodes g) rn)))

(defn add-loops
  "For each node n, add the edge n->n if not already present."
  [g]
  (struct directed-graph
          (:nodes g)
          (into {} (map (fn [n]
                          [n (conj (set (get-neighbors g n)) n)]) (:nodes g)))))

(defn remove-loops
  "For each node n, remove any edges n->n."
  [g]
  (struct directed-graph
          (:nodes g)
          (into {} (map (fn [n]
                          [n (disj (set (get-neighbors g n)) n)]) (:nodes g)))))


;; Graph Walk

(defn lazy-walk
  "Return a lazy sequence of the nodes of a graph starting a node n. Optionally,
provide a set of visited notes (v) and a collection of nodes to
visit (ns)."
  ([g n]
     (lazy-walk g [n] #{}))
  ([g ns v]
     (lazy-seq (let [s (seq (drop-while v ns))
                     n (first s)
                     ns (rest s)]
                 (when s
                   (cons n (lazy-walk g (concat (get-neighbors g n) ns) (conj v n))))))))

(defn transitive-closure
  "Returns the transitive closure of a graph. The neighbors are lazily computed.

Note: some version of this algorithm return all edges a->a
regardless of whether such loops exist in the original graph. This
version does not. Loops will be included only if produced by
cycles in the graph. If you have code that depends on such
behavior, call (-> g transitive-closure add-loops)"
  [g]
  (let [nns (fn [n]
              [n (delay (lazy-walk g (get-neighbors g n) #{}))])
        nbs (into {} (map nns (:nodes g)))]
    (struct directed-graph
            (:nodes g)
            (fn [n] (force (nbs n))))))

;; Strongly Connected Components

(defn- post-ordered-visit
  "Starting at node n, perform a post-ordered walk."
  [g n [visited acc :as state]]
  (if (visited n)
    state
    (let [[v2 acc2] (reduce (fn [st nd] (post-ordered-visit g nd st))
                            [(conj visited n) acc]
                            (get-neighbors g n))]
      [v2 (conj acc2 n)])))

(defn post-ordered-nodes
  "Return a sequence of indexes of a post-ordered walk of the graph."
  [g]
  (fnext (reduce #(post-ordered-visit g %2 %1)
                 [#{} []]
                 (:nodes g))))

(defn scc
  "Returns, as a sequence of sets, the strongly connected components of g."
  [g]
  (let [po (reverse (post-ordered-nodes g))
        rev (reverse-graph g)
        step (fn [stack visited acc]
               (if (empty? stack)
                 acc
                 (let [[nv comp] (post-ordered-visit rev
                                                     (first stack)
                                                     [visited #{}])
                       ns (doall (remove nv stack))] ;doall prevents StackOverflow
                   (recur ns nv (conj acc comp)))))]
    (step po #{} [])))

(defn component-graph
  "Given a graph, perhaps with cycles, return a reduced graph that is acyclic.
Each node in the new graph will be a set of nodes from the old.
These sets are the strongly connected components. Each edge will
be the union of the corresponding edges of the prior graph."
  ([g]
     (component-graph g (scc g)))
  ([g sccs]
     (let [find-node-set (fn [n]
                           (some #(if (% n) % nil) sccs))
           find-neighbors (fn [ns]
                            (let [nbs1 (map (partial get-neighbors g) ns)
                                  nbs2 (map set nbs1)
                                  nbs3 (apply union nbs2)]
                              (set (map find-node-set nbs3))))
           nm (into {} (map (fn [ns] [ns (find-neighbors ns)]) sccs))]
       (struct directed-graph (set sccs) nm))))

(defn recursive-component?
  "Is the component (recieved from scc) self recursive?"
  [g ns]
  (or (> (count ns) 1)
      (let [n (first ns)]
        (some #(= % n) (get-neighbors g n)))))

(defn self-recursive-sets
  "Returns, as a sequence of sets, the components of a graph that are
self-recursive."
  [g]
  (filter (partial recursive-component? g) (scc g)))

;; Dependency Lists

(defn fixed-point
  "Repeatedly apply fun to data until (equal old-data new-data)
returns true. If max iterations occur, it will throw an
exception. Set max to nil for unlimited iterations."
  [data fun max equal]
  (let [step (fn step [data idx]
               (when (and idx (= 0 idx))
                 (throw (Exception. "Fixed point overflow")))
               (let [new-data (fun data)]
                 (if (equal data new-data)
                   new-data
                   (recur new-data (and idx (dec idx))))))]
    (step data max)))
(defn- fold-into-sets
  [priorities]
  (let [max (inc (apply max 0 (vals priorities)))
        step (fn [acc [n dep]]
               (assoc acc dep (conj (acc dep) n)))]
    (reduce step
            (vec (replicate max #{}))
            priorities)))
(defn dependency-list
  "Similar to a topological sort, this returns a vector of sets. The
set of nodes at index 0 are independent. The set at index 1 depend
on index 0; those at 2 depend on 0 and 1, and so on. Those withing
a set have no mutual dependencies. Assume the input graph (which
much be acyclic) has an edge a->b when a depends on b."
  [g]
  (let [step (fn [d]
               (let [update (fn [n]
                              (inc (apply max -1 (map d (get-neighbors g n)))))]
                 (into {} (map (fn [[k v]] [k (update k)]) d))))
        counts (fixed-point (zipmap (:nodes g) (repeat 0))
                            step
                            (inc (count (:nodes g)))
                            =)]
    (fold-into-sets counts)))
(defn stratification-list
  "Similar to dependency-list (see doc), except two graphs are
provided. The first is as dependency-list. The second (which may
have cycles) provides a partial-dependency relation. If node a
depends on node b (meaning an edge a->b exists) in the second
graph, node a must be equal or later in the sequence."
  [g1 g2]
  (assert (= (-> g1 :nodes set) (-> g2 :nodes set)))
  (let [step (fn [d]
               (let [update (fn [n]
                              (max (inc (apply max -1
                                               (map d (get-neighbors g1 n))))
                                   (apply max -1 (map d (get-neighbors g2 n)))))]
                 (into {} (map (fn [[k v]] [k (update k)]) d))))
        counts (fixed-point (zipmap (:nodes g1) (repeat 0))
                            step
                            (inc (count (:nodes g1)))
                            =)]
    (fold-into-sets counts)))


;;; McKay's Algorithm for computing generators of the automorphism group

;; Partitions

(defn- make-ordered-partition
  "Given a collection coll if disjoint collections of all numbers from 0 to n-1, returns the
  corresponding partition."
  [coll]
  (vec (map set coll)))

(defn- discrete-partition?
  ""
  [parti]
  (every? singleton? parti))

(defn- unit-partition?
  ""
  [parti]
  (singleton? parti))

(defn- nr-neighbors-in-set
  ""
  [graph v W]
  (count (intersection W (set (get-neighbors graph v)))))

(defn- partition-by-set
  ""
  [graph A W]
  (let [grouped (group-by #(nr-neighbors-in-set graph % W) A)]
    (make-ordered-partition (map (comp set second)
                                 (sort #(< (first %1) (first %2)) grouped)))))

(defn- first-maximal-set-index
  ""
  [parti]
  (loop [i    0,
         t    0,
         size 0]
    (if (>= i (count parti))
      t
      (if (< size (count (parti i)))
        (recur (inc i) i (count (parti i)))
        (recur (inc i) t size)))))

(defn replace-partition-cell
  ""
  [parti idx new-parts]
  (make-ordered-partition (concat (subvec parti 0 idx)
                                  new-parts
                                  (subvec parti (inc idx)))))

(defn- append-partition
  ""
  [parti new-part]
  (make-ordered-partition (conj parti (set new-part))))

(defn- equitable-partition?
  "Returns true if and only if parti is an equitable partition of the nodes of graph, i.e. for every
  set V_1 in the partition and every two vertices v_1, v_2 in V_1 the number of common neighbors in
  every set V_2 of the partition is equal."
  [graph parti]
  (forall [V_1 parti,                   ;naïve implementation, does everything twice
           V_2 parti,
           v_1 V_1,
           v_2 V_1]
    (= (nr-neighbors-in-set graph v_1 V_2)
       (nr-neighbors-in-set graph v_2 V_2))))

;; Refining equitable partitions

(defn- refine-ordered-partition
  ""
  [graph, pi, alpha]
  (let [pi    (atom pi),
        alpha (atom alpha)]
    (loop [m 0]
      (if (or (discrete-partition? @pi)
              (>= m (count @alpha)))
        @pi
        (let [W (@alpha m),
              m (inc m)]
          (dotimes [k (count @pi)]
            (let [V_k (@pi k),
                  X   (partition-by-set graph V_k W)]
              (when-not (unit-partition? X)
                (let [t (first-maximal-set-index X)]
                  ;; replace V_k in alpha with X_t
                  (when-let [j (loop [j m]
                                 (if (>= j (count @alpha))
                                   nil
                                   (if (= (@alpha j) V_k)
                                     j
                                     (recur (inc j)))))]
                    (reset! alpha
                            (replace-partition-cell @alpha k [(X t)])))
                  ;; append X_1..X_{t-1} to alpha
                  (doseq [i (range 0 t)]
                    (reset! alpha
                            (append-partition @alpha (X i))))
                  ;; append X_{t+1}..X_s to alpha
                  (doseq [i (range (inc t) (count X))]
                    (reset! alpha
                            (append-partition @alpha (X i))))
                  ;; replace V_k in pi with X_1..X_s in that order
                  (reset! pi
                          (replace-partition-cell @pi k X))))))
          (recur m))))))

(defn- split-partition-at
  ""
  [pi u]
  (not-yet-implemented))

;; Search Tree

(defn- terminal-nodes
  ""
  [graph pi]
  (not-yet-implemented))

;; Isomorphy and Automorphisms

(defn- mckay
  ""
  [graph partition]
  (not-yet-implemented))

(defn canonical-isomorph
  ""
  ([graph]
     (canonical-isomorph [(:nodes graph)]))
  ([graph partition]
     (:canonical-isomorph (mckay graph partition))))

(defn automorphism-group-generators
  ""
  ([graph]
     (automorphism-group-generators graph [(:nodes graph)]))
  ([graph partition]
     (:automorphism-generators (mckay graph partition))))

(defn automorphism-group-size
  ""
  ([graph]
     (automorphism-group-size graph [(:nodes graph)]))
  ([graph partition]
     (:automorphism-size (mckay graph partition))))

;;;

nil
