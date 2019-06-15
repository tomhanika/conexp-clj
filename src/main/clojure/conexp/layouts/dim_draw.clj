(ns conexp.layouts.dim-draw
  (:require [ubergraph.core :as uber]
            [loom.graph :as lg]
            [conexp.fca.lattices :as lat]
            [conexp.fca.graph :refer :all]
            [conexp.util.graph :refer :all]
            [conexp.layouts.base :as lay]
            [conexp.base :exclude [transitive-closure] :refer :all]
            [rolling-stones.core :as sat :refer :all]))

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
       (lg/has-edge? digraph (lg/dest e2) (lg/src e1))
       (lg/has-edge? digraph (lg/dest e1) (lg/src e2)))))

(defn tig
  "Returns the \"transitive incompatibility graph\" (tig) for a given ordering
  (given ad graph).

  See Section 3, Dürrschnabel, Hanika, Stumme (2019) https://arxiv.org/abs/1903.00686"
  [graph]
  (let [ccg (co-comparability (lg/nodes graph) #(lg/has-edge? graph %1 %2))
        tig-nodes (map edge->vec (lg/edges ccg))]
    (reduce
      (fn [g-outer e1]
        (reduce
          (fn [g-inner e2]
            (if (is-compatible graph e1 e2)
              g-inner
              (uber/add-undirected-edges g-inner [e1 e2])))
          g-outer
          tig-nodes))
      (lg/add-nodes* (uber/graph) tig-nodes)               ; empty graph
      tig-nodes)))


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
                        #(let [vi (:src %) vj (:dest %)]
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
  ([graph]
   (compute-coordinates (lg/nodes graph) #(lg/has-edge? graph %1 %2)))
  ([P <=]
   (let [<=C (let [<=CAtom (atom (compute-conjugate-order P <=))
                   CAtom (atom ())]
               ;(println "<=CAtom: " @<=CAtom)
               (while (= @<=CAtom nil)
                 (swap! CAtom                               ; update C in each iteration
                        (fn [C] (let [I (tig (transitive-edge-union P <= C))]
                                  ;(println "(transitive-edge-union P <= C):")
                                  ;(uber/pprint (transitive-edge-union P <= C))
                                  ;(println "I:")
                                  ;(uber/pprint I)
                                  ;(println C " <-- C ; new -->" (sat-reduction I))
                                  (union C (map reverse (sat-reduction I))))))
                 (let [C @CAtom
                       <=CNew (compute-conjugate-order (transitive-edge-union P <= C))] ; compute new value for <=C
                   ;(println "C: " C)
                   ;(println "C size: " (count C))
                   ;(println "<=CNew: " <=CNew)
                   (reset! <=CAtom                          ; set new value
                           <=CNew)))
               @<=CAtom)
         <=1 (map edge->vec (lg/edges (transitive-edge-union P <= <=C)))
         <=2 (map edge->vec (lg/edges (transitive-edge-union P <= (map reverse <=C))))
         elements-less (fn [le elem] (- (count (filter #(some #{[% elem]} le) P)) 1)) ; util function used to compute |{x' | x' <= x}| - 1 for given <= and x
         coords (map #(let
                        [x1 (elements-less <=1 %) x2 (elements-less <=2 %)]
                        [% [x1 x2]]) P)]
     coords)))                                              ; return coordinates in x1-x2-coordinate-system

(defn dim-draw-layout
  "Returns a layout for a given lattice.

  The positions in the layout are computed using DimDraw, see
  Dürrschnabel, Hanika, Stumme (2019) https://arxiv.org/abs/1903.00686"
  [lattice]
  (let [g (lattice->graph lattice)
        coordinates (map #(vector (first %)
                                  (let [x1x2 (second %)
                                        x (- (first x1x2) (second x1x2))
                                        y (+ (first x1x2) (second x1x2))]
                                    [x y]))
                         (compute-coordinates g))
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
