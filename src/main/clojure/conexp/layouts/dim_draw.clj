(ns conexp.layouts.dim-draw
  (:require [loom.graph :as lg]
            [conexp.math.algebra :as alg]
            [conexp.fca.graph :refer :all]
            [conexp.fca.posets :refer :all]
            [conexp.util.graph :refer :all]
            [conexp.layouts.base :as lay]
            [conexp.base :exclude [transitive-closure] :refer :all]
            [rolling-stones.core :as sat :refer :all]
            [clojure.set :refer [difference union subset? intersection]]))

;;; Incremental bipartiteness with a parity union-find.  An edge means the two
;;; endpoints must get different colors; an odd cycle is detected when two
;;; endpoints are already forced into the same color.  `parent`, `parity` and
;;; `size` are immutable maps (so this is thread-safe under pmap).

(defn- uf-find
  "Returns [root parity-of-x-to-root]."
  [parent parity x]
  (loop [x x, acc 0]
    (let [p (parent x x)]
      (if (= p x)
        [x acc]
        (recur p (bit-xor acc (parity x 0)))))))

(defn- uf-union-diff
  "Adds the constraint that `a` and `b` get different colors.  Returns
  `[ok? parent parity size]`, where `ok?` is false iff this closes an odd cycle."
  [parent parity size a b]
  (let [[ra pa] (uf-find parent parity a)
        [rb pb] (uf-find parent parity b)]
    (if (= ra rb)
      [(not= pa pb) parent parity size]
      (let [sa   (size ra 1)
            sb   (size rb 1)
            want (bit-xor 1 pa pb)]
        (if (< sa sb)
          [true (assoc parent ra rb) (assoc parity ra want) (assoc size rb (+ sa sb))]
          [true (assoc parent rb ra) (assoc parity rb want) (assoc size ra (+ sa sb))])))))

(defn fill-graph
  "Returns an inclusion-maximal subset of vertices such that the induced subgraph
  is bipartite and contains `subset`.

  Vertices are considered in random order and greedily kept when they preserve
  bipartiteness.  Bipartiteness is maintained incrementally with a parity
  union-find, so each insertion costs O(deg · α) instead of a fresh traversal."
  [graph subset]
  (let [adj     (:adj graph)
        try-add (fn [present parent parity size v]
                  (reduce (fn [[ok? par pty sz] nb]
                            (if ok?
                              (uf-union-diff par pty sz v nb)
                              (reduced [false par pty sz])))
                          [true parent parity size]
                          (filter present (adj v))))
        ;; seed the union-find with the (already bipartite) `subset`
        [present parent parity size]
        (reduce (fn [[pres par pty sz] v]
                  (let [[_ par' pty' sz'] (try-add pres par pty sz v)]
                    [(conj pres v) par' pty' sz']))
                [#{} {} {} {}]
                subset)]
    (loop [remaining (seq (shuffle (difference (set (lg/nodes graph)) subset)))
           present   present
           parent    parent
           parity    parity
           size      size]
      (if (empty? remaining)
        present
        (let [v (first remaining)
              [ok? par' pty' sz'] (try-add present parent parity size v)]
          (if ok?
            (recur (rest remaining) (conj present v) par' pty' sz')
            (recur (rest remaining) present parent parity size)))))))

(defn create-individuums
  "Create the start individuums. The number of individuums created is the
  argument"
  [graph number-of-individuums]
  (pmap fill-graph (repeat number-of-individuums graph) (repeat #{})))

(defn reproduce
  "Takes two individuums and computes their intersection. Then makes it
  inclusion maximal and returns them"
  [graph ind1 ind2]
  (fill-graph graph (intersection ind1 ind2)))

(defn discrete-distribution
  "Gives a random sample out of list. The (inverse) probability is given in
  the probability vector"
  [list prob]
  (let [sum (reduce + prob)
        rand (inc (rand-int sum))
        sumprob (reduce
                 (fn [l e]
                   (conj l (+ (peek l) e)))
                 [(first prob)]
                 (rest prob))
        retpos (loop [pos 0]
                 (if (<= rand (nth sumprob pos))
                   pos
                   (recur (inc pos))))]
    (nth list retpos)))

(defn new-generation
  "Takes the generation, breeds a new one and returns it"
  [graph generation max-individuums]
  (let [sizes (map count generation)
        min (- (apply min sizes) 1)
        prob (map #(- % min) sizes)
        breed   (vec (pmap (fn [_] ( reproduce graph
                                    (discrete-distribution generation prob)
                                    (discrete-distribution generation prob)))
                           (repeat (* max-individuums 4) 0)))
        candidates (concat generation breed)
        survivors (take max-individuums  (sort-by
                                          (fn [x] (- 0 (count x)))
                                          candidates))]
    survivors))

(defn genetic-bipartite-subgraph
  "A genetic algorithm to compute an inclusion minimal set to remove, such that
  the graph becomes bipartiteRuns on [graph] with
  [individuums] individuums and
  [generations] generations"
  [graph max-individuums max-generations]
  (let [start-individuums (create-individuums graph max-individuums)
        end-individuums  (loop [current start-individuums
                                counter 0]
                           (if (= counter max-generations)
                             current
                             (recur  (new-generation graph current max-individuums)
                                    (inc counter))))
        best (first (sort-by (fn [x] (- 0 (count x))) end-individuums))
        to-remove (difference (set (lg/nodes graph)) best)]
    to-remove))

(defn lt-seq
  "Constructs a constraint stating that at most `k` of the given variables `xs` are true.

  Input: A list `xs` of variables and a number `k`
  Output: A CNF stating that at most k of the variables are true,
  usable for rolling-stones sat solver.

  This is from \"Towards an optimal CNF Encoding of Boolean Cardinality
  Constraints\", Carsten Sinz (2005).
  Could be replaced by `rolling-stones.core/at-most`."
  ([xs k]
   (lt-seq xs k (Math/random)))
  ([xs k prefix]
   (assert (>= k 0))
   (if (and (> k 0) (not (empty? xs)))
     (let [n (count xs)
           s prefix]
       (concat
         [[(! (xs 0)) [:tmp s 0 0]]]
         (map #(vec [(! [:tmp s 0 %])])
              (vec (range 1 k)))
         (mapcat (fn [i] (concat [
                                  [(! (xs i)) [:tmp s i 0]]
                                  [(! [:tmp s (- i 1) 0]) [:tmp s i 0]]]
                                 (mapcat (fn [j] [
                                                  [(! (xs i)) (! [:tmp s (- i 1) (- j 1)]) [:tmp s i j]]
                                                  [(! [:tmp s (- i 1) j]) [:tmp s i j]]])
                                         (vec (range 1 k)))
                                 [[(! (xs i)) (! [:tmp s (- i 1) (- k 1)])]]))
                 (vec (range 1 (- n 1))))
         [[(! (xs (- n 1))) (! [:tmp s (- n 2) (- k 1)])]]))
     (vec (map #(vec [(! %)]) xs)))))


(defn is-compatible
  "Checks if two edges are compatible.

  For an ordering given as graph and two edges e1 and e2 this means that the
  transitive closure of the graph with both edges added does not contain a
  (non-trivial) cycle."
  [digraph e1 e2]
   (not(and
        (or (= (lg/dest e2) (lg/src e1)) (lg/has-edge? digraph (lg/dest e2) (lg/src e1)))
        (or (= (lg/dest e1) (lg/src e2)) (lg/has-edge? digraph (lg/dest e1) (lg/src e2))))))


(defn tig
  "Returns the \"transitive incompatibility graph\" (tig) for a given ordering
  (given as graph).

  See Section 3, Dürrschnabel, Hanika, Stumme (2019) https://arxiv.org/abs/1903.00686"
  [graph]
  (let [nodes    (vec (lg/nodes graph))
        ;; oriented incomparable pairs
        incs     (vec (for [x nodes, y nodes
                            :when (and (not= x y)
                                       (not (lg/has-edge? graph x y))
                                       (not (lg/has-edge? graph y x)))]
                        [x y]))
        n        (count incs)
        ;; edges of the tig: incompatible pairs of incomparable pairs
        incedges (for [i (range n)
                       j (range (inc i) n)
                       :let [a (incs i), b (incs j)]
                       :when (not (is-compatible graph a b))]
                   [a b])
        ;; adjacency in a single pass over the edges
        adj      (reduce (fn [m [a b]]
                           (-> m (update a conj b) (update b conj a)))
                         (zipmap incs (repeat []))
                         incedges)]
    (if (empty? incs)
      (lg/graph)
      (lg/graph adj))))

(defn- sat-reduction-static
  "The k-independent part of the vertex-bipartization CNF for `g`:
  returns `[node-clauses edge-clauses node-vars]`."
  [g]
  [(mapcat #(vec [[[% 1] [% 2] [% 3]]])                    ; at least one of V_{i,1..3}
           (nodes g))
   (mapcat #(let [vi (lg/src %) vj (lg/dest %)]
              [[(! [vi 1]) (! [vj 1])]
               [(! [vi 2]) (! [vj 2])]])
           (lg/edges g))
   (vec (map #(vec [% 3]) (nodes g)))])

(defn- sat-reduction-solve
  "Solves the vertex-bipartization instance for a fixed `k`, reusing the static
  clauses.  Returns the set `C` to remove (possibly empty), or nil if there is no
  such set of cardinality at most `k`."
  [node-clauses edge-clauses node-vars k]
  (let [solution (sat/solve-symbolic-cnf
                   (concat node-clauses edge-clauses (lt-seq node-vars k "s")))]
    (when solution
      (map first (filter #(and (sat/positive? %) (= (% 1) 3)) solution)))))

(defn sat-reduction
  "Reduces the problem of finding a maximum bipartite subgraph to satisfiability
  and solves it.

  The vertices of the graph get partitioned in 3 sets: `P_1`, `P_2` and `C`, s.t.
  `P_1` and `P_2` constitute the bipartite graph and `C` has cardinality at most k.

  Input: An undirected graph and optionally a maximum cardinality for `C`.
  Output: The set to be removed from the graph s.t. the graph becomes bipartite.
  nil, if there is no such set of cardinality at most `k`.

  See Section 5.2, Dürrschnabel, Hanika, Stumme (2019) https://arxiv.org/abs/1903.00686"
  ([g]
   ;; smallest k for which the instance is satisfiable; the static clauses are
   ;; built once and reused for every k.
   (let [[nc ec nv] (sat-reduction-static g)]
     (loop [k 0]
       (or (sat-reduction-solve nc ec nv k)
           (recur (inc k))))))
  ([g k]
   (let [[nc ec nv] (sat-reduction-static g)]
     (sat-reduction-solve nc ec nv k))))

(defn compute-conjugate-order
  "For a given ordered set, computes the conjugate order.
  This is a transitive orientation on the not-yet oriented pairs.
  If that is not possible, returns nil.

  See Section 5.2, Dürrschnabel, Hanika, Stumme (2019) https://arxiv.org/abs/1903.00686"
  ([graph]
    (compute-conjugate-order (lg/nodes graph) #(lg/has-edge? graph %1 %2)))
  ([P <=]
   (let [C (co-comparability P <=)]
     (if (comparability-graph? C)                           ; has transitive orientation?
       (lg/edges (transitive-orientation C))
       nil))))

(defn compute-coordinates-helper
  ([P relation args]
    (let [conjugate (atom (compute-conjugate-order P relation))
          base      (atom ())]
      (while
        (nil? @conjugate)
        (swap! base
               (fn [nodes] 
                 (let [graph (tig (transitive-edge-union P relation nodes))]
                   (union nodes 
                          (map reverse
                            (cond 
                              (= (first args) "greedy")
                                (difference (set (lg/nodes graph))
                                            (fill-graph graph #{}))
                              (= (first args) "genetic")
                                (genetic-bipartite-subgraph graph 
                                                            (nth args 1)
                                                            (nth args 2))
                              :else
                                (sat-reduction graph)))))))
        (reset! conjugate 
                (compute-conjugate-order 
                  (transitive-edge-union P relation @base))))
      @conjugate)))

(defn compute-coordinates
  "Given a set `P` and a binary relation `<=`, computes coordinates for each
  element of the set s.t. placing the elements to the coordinates gives a clear
  image of the relation.

  The coordinates are returned in the form `[[e1 [x1 y1]] [e2 [x2 y2]] ...]`
  where e is the element, and x and y are the corresponding coordinates.

  See Section 5.2, Dürrschnabel, Hanika, Stumme (2019) https://arxiv.org/abs/1903.00686"
  ([graph args]
    (let [P        (lg/nodes graph)
          relation #(lg/has-edge? graph %1 %2)
          C        (compute-coordinates-helper P relation args)
          x-graph  (transitive-edge-union P relation C)
          y-graph  (transitive-edge-union P relation (map reverse C))
          ;; rank in a linear extension = number of elements below = in-degree in
          ;; the transitive DAG (minus the reflexive loop).
          get-pos  (fn [g elem] (dec (count (lg/predecessors* g elem))))
          coords   (map
                     #(vector % [(get-pos x-graph %) (get-pos y-graph %)])
                     P)]
      coords)))

(defn dim-draw-layout
  "Returns a layout for a given ordered set.

  The positions in the layout are computed using DimDraw, see
  Dürrschnabel, Hanika, Stumme (2019) https://arxiv.org/abs/1903.00686"
  [poset & args]
  (let [g (poset->graph poset)
        coordinates (map #(vector (first %)
                                  (let [x1x2 (second %)
                                        x (- (first x1x2) (second x1x2))
                                        y (+ (first x1x2) (second x1x2))]
                                    [x y]))
                         (compute-coordinates g args))
        positions (reduce conj {} coordinates)]
    (lay/make-layout-nc poset
                        positions
                        (mapcat (fn [n] (map #(vector n %)
                                             (poset-upper-neighbours poset n)))
                                (alg/base-set poset)))))

(defn- replicate-str
  [s i]
  (if (> i 1) (replicate-str (str s s) (/ i 2)) s))

(defn draw-ascii
  "Given coordinates in the form `[[e1 [x1 y1]] [e2 [x2 y2]] ...]`, prints
  the elements in the console."
  [coords]
  (let [x-step 2
        size (count coords)
        spaces (replicate-str "  " (* size x-step))
        line-contents (map (fn [i] (filter
                                     #(= i (+ (first (last %)) (last (last %))))
                                     coords))
                           (reverse (range 0 (- (* 2 size) 1))))
        lines (map (fn [line] (map #(vec [(first %)
                                          (- (first (last %)) (last (last %)))])
                                   line))
                   line-contents)
        min-x (if (empty? lines)
                0
                (apply min (map #(apply min (cons 0 (map last %))) lines)))
        positioned-lines (map (fn [line] (map #(vec [(first %)
                                                     (* (- (last %) min-x) x-step)])
                                              line))
                              lines)]
    (doseq [pos-line positioned-lines]
      (println (reduce
                 (fn [l p]
                   (str
                     (.substring
                       (str l spaces)
                       0
                       (last p))
                     (first p)))
                 "" (sort-by last pos-line))))))

nil
