(ns conexp.layouts.dim-draw
  (:require [ubergraph.core :as uber]
            [loom.graph :as lg]
            [conexp.fca.lattices :as lat]
            [conexp.fca.graph :refer :all]
            [conexp.util.graph :refer :all]
            [conexp.base :exclude [transitive-closure] :refer :all]
            [rolling-stones.core :as sat :refer :all]))

(defn- lt-seq
  ([xs k]
   (lt-seq xs k (Math/random)))
  ([xs k prefix]
   (if (> k 0)
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

(def cnf (lt-seq [:x1 :x2 :x3] 0 "s"))
(println cnf)
(println)
(doseq [line (sat/solutions-symbolic-cnf cnf)]
  (println line))

(defn sat-reduction
  ([g]
   (first (drop-while #(= % nil) (map #(sat-reduction g %) (range)))))
  ([g k]
   (let [node-clauses (mapcat
                        #(vec [[[% 1] [% 2] [% 3]]          ;either V_{i,1}, V_{i,2}, or V_{i,3} is true
                               [(! [% 1]) (! [% 2])]        ;but not two of them
                               [(! [% 2]) (! [% 3])]
                               [(! [% 3]) (! [% 1])]])
                        (nodes g))
         ;edge-clauses (mapcat
         ;               #(let [vi (:src %) vj (:dest %)]
         ;                  [[[vi 1] [vj 1]] [[vi 2] [vj 2]]])
         ;               (lg/edges g))
         edge-clauses (mapcat
                        #(let [vi (:src %) vj (:dest %)]
                           [[[vi 1] [vj 1] [vi 2] [vj 2]]
                            [[vi 2] [vj 2] [vi 3] [vj 3]]
                            [[vi 3] [vj 3] [vi 1] [vj 1]]])
                        (lg/edges g))
         no-more-than-k-bad-edges-clauses (lt-seq
                                            (vec (map #(vec [% 3]) (nodes g)))
                                            k "s")
         clauses (concat node-clauses edge-clauses no-more-than-k-bad-edges-clauses)
         raw-solution (sat/solve-symbolic-cnf clauses)
         C (map first (filter #(and (sat/positive? %) (= (% 1) 3)) raw-solution))]
     (println node-clauses)
     (println edge-clauses)
     (println no-more-than-k-bad-edges-clauses)
     (println "solved:")
     (println raw-solution)
     (println C)
     C)))

(sat-reduction (uber/graph [:a :b] [:a :c] [:b :e] [:c :e] [:a :e]) 1)

(defn compute-conjugate-order
  [P <=]
  (let [C (co-comparability P <=)]
    (if (comparability-graph? C)
      (map edge->vec (lg/edges (transitive-orientation C)))
      nil)))


(defn compute-coordinates
  [P <=]
  (let [<=as-set (map edge->vec (lg/edges (make-digraph-from-condition P <=)))
        <=C
        (let [<=C' (compute-conjugate-order P <=)]
          (if (= <=C' nil)
            (let [I (incompatibility-graph P <=)
                  C (sat-reduction I)]
              (compute-conjugate-order P (union <= (transitive-closure C))))
            <=C'))
        <=1 (union <=as-set <=C)
        <=2 (union <=as-set (map reverse <=C))
        elements-less (fn [le elem] (- (count (filter #(some #{[% elem]} le) P)) 1))
        coords (map #(let
                       [x1 (elements-less <=1 %) x2 (elements-less <=2 %)]
                       [% [x1 x2]]) P)]
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

(draw-ascii (compute-coordinates
              #{1 2 3 4 5} (non-strict #(or (= 1 %1) (= 5 %2) (and (= %1 2) (= %2 3))))))



nil