(ns conexp.layouts.dim-draw
  (:require [ubergraph.core :as uber]
            [loom.graph :as lg]
            [conexp.fca.lattices :as lat]
            [conexp.fca.graph :refer :all]
            [conexp.util.graph :refer :all]
            [conexp.base :exclude [transitive-closure] :refer :all]
            [rolling-stones.core :as sat :refer :all]))

(defn lt-seq
  "Constructs a constraint stating that at most `k` of the given variables `xs` are true.

  This is from ??.

  Could be replaced by `rolling-stones.core/at-most`."      ; todo paper name
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


(defn sat-reduction
  "Reduces the problem of finding a maximum bipartite subgraph to satisfiability.


  As described in Section 5.2 of Dominik's paper."          ; todo paper name
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
     ;(println node-clauses)
     ;(println edge-clauses)
     ;(println no-more-than-k-bad-edges-clauses)
     ;(println "solved:")
     ;(println raw-solution)
     ;(println C)
     C)))

;(sat-reduction (uber/graph [:a :b] [:a :c] [:b :e] [:c :e] [:a :e]) 1)

(defn compute-conjugate-order
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

  See paper of Dominik (2019)"                              ;todo paper name
  ([graph]
   (compute-coordinates (lg/nodes graph) #(lg/has-edge? graph %1 %2)))
  ([P <=]
   (let [<=as-set (map edge->vec (lg/edges (make-digraph-from-condition P <=)))
         <=C (let [<=CAtom (atom (compute-conjugate-order P <=))
                   CAtom (atom ())]
               (println "<=CAtom: " @<=CAtom)
               (while (= @<=CAtom nil)
                 (swap! CAtom
                        (fn [C] (let [I (incompatibility-graph (transitive-edge-union P <= C))]
                                  (println "(transitive-edge-union P <= C):")
                                  (uber/pprint (transitive-edge-union P <= C))
                                  (println "I:")
                                  (uber/pprint I)
                                  (println C (sat-reduction I))
                                  (union C (sat-reduction I)))))
                 (let [C @CAtom
                       C-bar-graph (transitive-closure (apply uber/digraph C))
                       ;<=CNew (compute-conjugate-order P #(or (<= %1 %2) (lg/has-edge? C-bar-graph %2 %1)))] ;why \bar C? why not union?
                       ;<=CNew (compute-conjugate-order (transitive-edge-union P <= #(lg/has-edge? C-bar-graph %1 %2)))]
                       <=CNew (compute-conjugate-order (transitive-edge-union P <= #(lg/has-edge? C-bar-graph %2 %1)))]
                   (println "C: " C)
                   (println "C-bar-graph:")
                   (uber/pprint C-bar-graph)
                   (println "<=CNew: " <=CNew)
                   (reset! <=CAtom
                           <=CNew)))
               @<=CAtom)
         <=1 (map edge->vec (lg/edges (transitive-edge-union P <= <=C)))
         <=2 (map edge->vec (lg/edges (transitive-edge-union P <= (map reverse <=C))))
         elements-less (fn [le elem] (- (count (filter #(some #{[% elem]} le) P)) 1))
         coords (map #(let
                        [x1 (elements-less <=1 %) x2 (elements-less <=2 %)]
                        [% [x1 x2]]) P)]
     coords)))

;(println (compute-conjugate-order
;           #{1 2 3 4 5} (non-strict #(or (= 1 %1) (= 5 %2)))))
;
;(println (compute-coordinates
;           #{1 2 3 4 5} (non-strict #(or (= 1 %1) (= 5 %2)))))

(defn- replicate-str
  [s i]
  (if (> i 1) (replicate-str (str s s) (/ i 2)) s))

(defn draw-ascii
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

;(draw-ascii (compute-coordinates
;              #{1 2 3 4 5} (non-strict #(or (= 1 %1) (= 5 %2)))))
;
;(draw-ascii (compute-coordinates
;              #{1 2 3 4 5} (non-strict #(or (= 1 %1) (= 5 %2) (and (= %1 2) (= %2 3))))))


nil
