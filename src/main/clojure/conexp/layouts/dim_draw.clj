(ns conexp.layouts.dim-draw
  (:require [ubergraph.core :as uber]
            [loom.graph :as lg]
            [conexp.fca.lattices :as lat]
            [conexp.fca.graph :refer :all]
            [conexp.util.graph :refer :all]
            [conexp.layouts.base :as lay]
            [conexp.base :exclude [transitive-closure] :refer :all]
            [rolling-stones.core :as sat :refer :all])
  (:import [org.dimdraw Bipartite]))

(defn in-odd-cycle?
  "Returns true, if vertex is in an odd cycle with subset in g"
  [graph subset vertex]
   (loop [to-check [vertex]
         coloring  {vertex 1}]
    (let [first (peek to-check)
          color-first  (coloring first)
          color (mod (+ 1 color-first) 2)
          succ (set (filter (fn [x] (contains? subset x))
                            (lg/successors graph first)))
          wrong (filter (fn [x] (= color-first (coloring x))) succ)
          uncolored (filter (fn [x] (not (contains? coloring x))) succ)
          to-check (vec (concat uncolored to-check))
          coloring (conj coloring (zipmap uncolored (repeat color)))
          incycle (not (empty? wrong))
          popped (pop to-check)]
      (if (or incycle (empty? popped))
        incycle
        (recur popped coloring)))))

(defn fill-graph
  "Returns an inclusion-maximal subset of vertices, such that it is biparite
  and contains subset"
  [graph subset]
  (let [order (shuffle (difference (set (lg/nodes graph)) subset))
        order (vec (distinct order))]
    (if (empty? order)
      subset
      (loop [remaining order
             final subset]
        (let [first (peek remaining)
              popped (pop remaining)
              newsub (conj final first)
              ;;              can-insert  (not (in-odd-cycle? graph newsub first))
              can-insert  (not (. Bipartite isInOddCycle (:adj graph) newsub first))
              next-subset (if can-insert
                            newsub
                            final)]
          (if (not (empty? popped))
            (recur popped next-subset)
            next-subset))))))

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
  (given ad graph).

  See Section 3, Dürrschnabel, Hanika, Stumme (2019) https://arxiv.org/abs/1903.00686"
  [graph]
  (let [pairs     (filter #(not (= (first %) (peek %)))
                          (reduce concat (for [x (lg/nodes graph)]
                                           (for [y (lg/nodes graph)] [x y]))))
        inc       (filter #(not (or (lg/has-edge? graph (first %) (peek  %))
                                    (lg/has-edge? graph (peek %)  (first %))))
                          pairs)
        alledges  (reduce concat (for [x (range (count inc))]
                                   (for [y (range (+ x 1) (count inc))]
                                     [(nth inc x) (nth inc y)])))
        incedges (filter #(not (is-compatible graph (first %) (peek %))) alledges)
        ;;       graphmap (zipmap inc (map #(for [edge incedges]) inc))
        graph    (zipmap inc
                         (for [node inc]
                           (vec (filter #(not (= nil %))
                                        (for [edge incedges]
                                          (cond
                                            (= (first edge) node) (peek edge)
                                            (= (peek edge ) node) (first edge)
                                            :else nil))))))]
    (if (empty? inc)
      (lg/graph)
      (lg/graph graph))))

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
   (first (drop-while #(= % nil) (map #(sat-reduction g %) (range)))))
  ([g k]
   (let [node-clauses (mapcat
                        #(vec [[[% 1] [% 2] [% 3]]])        ; alt least one of V_{i,1}, V_{i,2}, or V_{i,3} is true
                        (nodes g))
         edge-clauses (mapcat
                        #(let [vi (lg/src %) vj (lg/dest %)]
                           [[(! [vi 1]) (! [vj 1])]
                            [(! [vi 2]) (! [vj 2])]])
                        (lg/edges g))
         no-more-than-k-bad-edges-clauses (lt-seq
                                            (vec (map #(vec [% 3]) (nodes g)))
                                            k "s")
         clauses (concat node-clauses edge-clauses no-more-than-k-bad-edges-clauses)
         raw-solution (sat/solve-symbolic-cnf clauses)
         C (if (= raw-solution nil)
             nil
             (map first (filter #(and (sat/positive? %) (= (% 1) 3)) raw-solution)))]
     C)))

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
       (map edge->vec (lg/edges (transitive-orientation C)))
       nil))))


(defn compute-coordinates
  "Given a set `P` and a binary relation `<=`, computes coordinates for each
  element of the set s.t. placing the elements to the coordinates gives a clear
  image of the relation.

  The coordinates are returned in the form `[[e1 [x1 y1]] [e2 [x2 y2]] ...]`
  where e is the element, and x and y are the corresponding coordinates.

  See Section 5.2, Dürrschnabel, Hanika, Stumme (2019) https://arxiv.org/abs/1903.00686"
  ([graph args]
   (let [P (lg/nodes graph)
         <= #(lg/has-edge? graph %1 %2)]
     (let [<=C (let [<=CAtom (atom (compute-conjugate-order P <=))
                     CAtom (atom ())]
                                        ;(println "<=CAtom: " @<=CAtom)
                 (while (= @<=CAtom nil)
                   (swap! CAtom         ; update C in each iteration
                          (fn [C] (let [I (tig (transitive-edge-union P <= C))]
                                        ;(println "(transitive-edge-union P <= C):")
                                        ;(uber/pprint (transitive-edge-union P <= C))
                                        ;(println "I:")
                                        ;(uber/pprint I)
                                        ;(println C " <-- C ; new -->" (sat-reduction I))
                                    ;;(union C (map reverse (sat-reduction I)))
                                    (union C (map reverse
                                                  (cond
                                                    (= (first args) "greedy")
                                                    (difference (set (lg/nodes I)) (fill-graph I #{}))
                                                    (= (first args) "genetic")
                                                    (genetic-bipartite-subgraph I
                                                                                (nth args 1)
                                                                                (nth args 2))
                                                    :else
                                                    (sat-reduction I))))
                                    )))
                   (let [C @CAtom
                         <=CNew (compute-conjugate-order (transitive-edge-union P <= C))] ; compute new value for <=C
                                        ;(println "C: " C)
                                        ;(println "C size: " (count C))
                                        ;(println "<=CNew: " <=CNew)
                     (reset! <=CAtom    ; set new value
                             <=CNew)))
                 @<=CAtom)
           <=1 (map edge->vec (lg/edges (transitive-edge-union P <= <=C)))
           <=2 (map edge->vec (lg/edges (transitive-edge-union P <= (map reverse <=C))))
           elements-less (fn [le elem] (- (count (filter #(some #{[% elem]} le) P)) 1)) ; util function used to compute |{x' | x' <= x}| - 1 for given <= and x
           coords (map #(let
                            [x1 (elements-less <=1 %) x2 (elements-less <=2 %)]
                            [% [x1 x2]]) P)]
       coords))))                                              ; return coordinates in x1-x2-coordinate-system

(defn dim-draw-layout
  "Returns a layout for a given lattice.

  The positions in the layout are computed using DimDraw, see
  Dürrschnabel, Hanika, Stumme (2019) https://arxiv.org/abs/1903.00686"
  [lattice & args]
  (let [g (lattice->graph lattice)
        coordinates (map #(vector (first %)
                                  (let [x1x2 (second %)
                                        x (- (first x1x2) (second x1x2))
                                        y (+ (first x1x2) (second x1x2))]
                                    [x y]))
                         (compute-coordinates g args))
        positions (reduce conj {} coordinates)]
    (lay/make-layout-nc lattice
                    positions
                    (mapcat (fn [n] (map #(vector n %)
                                         (lat/lattice-upper-neighbours lattice n)))
                            (lat/base-set lattice)))))

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
