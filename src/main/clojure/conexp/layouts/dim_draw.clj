(ns conexp.layouts.dim-draw
  (:require [ubergraph.core :as uber]
            [loom.graph :as lg]
            [conexp.fca.lattices :as lat]
            [conexp.fca.graph :refer :all]
            [conexp.util.graph :refer :all]
            [conexp.base :exclude [transitive-closure] :refer :all]))

(defn compute-conjugate-order
  [P <=]
  (let [C (co-comparability P <=)]
    (if (comparability-graph? C)
      (map edge->vec (lg/edges (transitive-orientation C)))
      nil)))

(defn maximum-bipartite-subgraph
  [I]
  42)

(def ?? nil)
(def ??remove-nodes nil)

(defn compute-coordinates
  [P <=]
  (let [<=as-set (map edge->vec (lg/edges (make-digraph-from-condition P <=)))
        <=C
        (let [<=C' (compute-conjugate-order P <=)]
          (if (= <=C' nil)
            (let [I (incompatibility-graph P <=)
                  B (maximum-bipartite-subgraph I)
                  C (?? (??remove-nodes I B))]
              (compute-conjugate-order P (union <= (transitive-closure C))))
            <=C'))
        <=1 (union <=as-set <=C)
        <=2 (union <=as-set (map reverse <=C))
        elements-less (fn [le elem] (- (count (filter #(some #{[% elem]} le) P)) 1))
        coords (map #(let
                       [x1 (elements-less <=1 %) x2 (elements-less <=2 %)]
                       [% [x1 x2]]) P)
        ]
    coords))

(println (compute-conjugate-order
           #{1 2 3 4 5} (non-strict #(or (= 1 %1) (= 5 %2)))))

(println (compute-coordinates
           #{1 2 3 4 5} (non-strict #(or (= 1 %1) (= 5 %2)))))

(defn- replicate-str
  [s i]
  (if (> i 1) (replicate-str (str s s) (/ i 2)) s))

(defn draw-ascii
  [coords]
  (let [
        x-step 2
        size (count coords)
        spaces (.substring (replicate-str "   " (* size x-step)) (* size x-step))
        line-contents (map (fn [i] (filter
                                     #(= i (+ (first (last %)) (last (last %))))
                                     coords))
                           (reverse (range 0 (- (* 2 size) 1))))
        lines (map (fn [line] (map #(vec [(first %)
                                          (- (first (last %)) (last (last %)))])
                                   line))
                   line-contents)
        min-x (apply min (map #(apply min (cons 0 (map last %))) lines))
        positioned-lines (map (fn [line] (map #(vec [(first %)
                                                     (* (- (last %) min-x) x-step)])
                                              line))
                              lines)
        ]
    (doseq [pos-line positioned-lines]
      (println (reduce
                 (fn [l p]
                   (str
                     (.substring
                       (str l spaces)
                       0
                       (last p))
                     (first p)))
                 "" pos-line))
      )
    ))

(draw-ascii (compute-coordinates
              #{1 2 3 4 5} (non-strict #(or (= 1 %1) (= 5 %2)))))


nil