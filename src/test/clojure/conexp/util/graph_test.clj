;; Copyright ⓒ the conexp-clj developers; all rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.util.graph-test
  (:require [conexp.base :exclude [transitive-closure] :refer :all]
            [loom.graph :as lg]
            [clojure.test :refer :all]
            [conexp.util.graph :refer :all]))

;;;

(deftest test-mk-graph
  (let [g1 (make-directed-graph #{1 2 3} (fn [x] (if (< x 3) #{3} #{1 2})))
        g2 (make-directed-graph [1 2 3] (fn [x] (if (< x 3) [3] [1 2])))]
    (is (= (set (get-neighbors g1 1)) #{3}))
    (is (= (set (get-neighbors g1 2)) #{3}))
    (is (= (set (get-neighbors g1 3)) #{1 2}))
    (is (= (set (nodes g1)) (set (nodes g2))))
    (doseq [n (nodes g1)]
      (is (= (set (get-neighbors g1 n)) (set (get-neighbors g2 n)))))
    ))

(def some-graphs
  {
   :g1     (make-directed-graph #{1 2 3} (fn [x] (if (< x 3) #{3} #{1 2})))
   :g2     (make-directed-graph #{1 2 3} (fn [_] #{3}))
   :g3     (make-directed-graph #{1 2 3} (fn [x] (if (= x 3) #{1 2 3} #{})))
   :g4     (make-directed-graph #{1 2 3} (fn [x] (if (= x 3) #{3} #{1 2})))
   :g5     (make-directed-graph #{1 2 3 4} (fn [x] (if (< x 3) #{(+ x 1) 4} #{})))
   :gempty (make-directed-graph #{} (fn [_] (assert false "this fn should not have been called")))
   })

(deftest test-reverse-graphs
  (is (= (nodes (:g1 some-graphs)) (nodes (reverse-graph (:g1 some-graphs)))))
  (is (= (nodes (:g2 some-graphs)) (nodes (reverse-graph (:g2 some-graphs)))))
  (is (= (nodes (:g3 some-graphs)) (nodes (reverse-graph (:g3 some-graphs)))))
  (doseq [n1 #{1 2 3}
          n2 #{1 2 3}]
    (is (<=> (contains? (set (get-neighbors (:g1 some-graphs) n1)) n2)
             (contains? (set (get-neighbors (reverse-graph (:g1 some-graphs)) n2)) n1)))
    (is (<=> (contains? (set (get-neighbors (:g2 some-graphs) n1)) n2)
             (contains? (set (get-neighbors (reverse-graph (:g2 some-graphs)) n2)) n1)))
    (is (<=> (contains? (set (get-neighbors (:g3 some-graphs) n1)) n2)
             (contains? (set (get-neighbors (reverse-graph (:g3 some-graphs)) n2)) n1)))))

(deftest test-loops
  (let [g (:g2 some-graphs)
        gWith (add-loops g)
        gWithout1 (remove-loops g)
        gWithout2 (remove-loops gWith)]
    (doseq [n #{1 2 3}
            n2 #{1 2 3}]
      (if (= n n2)
        (do (is (contains? (set (get-neighbors gWith n)) n))
            (is (not (contains? (set (get-neighbors gWithout1 n)) n)))
            (is (not (contains? (set (get-neighbors gWithout2 n)) n))))
        (do (is (<=> (contains? (set (get-neighbors gWith n)) n2) (contains? (set (get-neighbors g n)) n2)))
            (is (<=> (contains? (set (get-neighbors gWithout1 n)) n2) (contains? (set (get-neighbors g n)) n2)))
            (is (<=> (contains? (set (get-neighbors gWithout2 n)) n2) (contains? (set (get-neighbors g n)) n2))))))))

(deftest test-transitive-closure
  (let [tc1 (transitive-closure (:g1 some-graphs))
        tc2 (transitive-closure (:g2 some-graphs))
        tc3 (transitive-closure (:g3 some-graphs))
        ]
    (doseq [n #{1 2 3}]
      (is (= (set (get-neighbors tc2 n)) #{3}))
      (is (= (set (get-neighbors tc1 n)) #{1 2 3})))
    (is (= (set (get-neighbors tc3 3)) #{1 2 3}))
    (is (= (set (get-neighbors tc3 1)) #{}))
    (is (= (transitive-closure (make-directed-graph [1 2 3] (fn [x] (if (< x 3) [(+ x 1)] []))))
           (lg/digraph [1 2] [1 3] [2 3])))
    (is (= (transitive-closure (make-directed-graph [0 1 2] (fn [x] [(mod (+ x 1) 3)])))
           (make-digraph-from-condition [0 1 2] (fn [_, _] true))))
    ))

(deftest test-scc
  (is (= (scc (:g1 some-graphs)) [#{1 2 3}]))
  (is (= (set (scc (:g2 some-graphs))) #{#{1} #{2} #{3}}))
  (is (= (set (scc (:g3 some-graphs))) #{#{1} #{2} #{3}}))
  (is (= (set (scc (:g4 some-graphs))) #{#{1 2} #{3}})))

(deftest test-component-graph
  (is (= (set (nodes (component-graph (:g1 some-graphs)))) #{#{1 2 3}}))
  (is (= (set (nodes (component-graph (:g2 some-graphs)))) #{#{1} #{2} #{3}}))
  (is (= (set (nodes (component-graph (:g3 some-graphs)))) #{#{1} #{2} #{3}}))
  (is (= (set (nodes (component-graph (:g4 some-graphs)))) #{#{1 2} #{3}}))
  (is (= (set (get-neighbors (component-graph (:g1 some-graphs)) #{1 2 3})) #{#{1 2 3}}))
  (is (= (set (get-neighbors (component-graph (:g2 some-graphs)) #{1})) #{#{3}}))
  (is (= (set (get-neighbors (component-graph (:g3 some-graphs)) #{1})) #{}))
  (is (= (set (get-neighbors (component-graph (:g4 some-graphs)) #{1 2})) #{#{1 2}})))

(deftest test-self-recursive-sets
  (is (= (set (self-recursive-sets (:g1 some-graphs))) #{#{1 2 3}}))
  (is (= (set (self-recursive-sets (:g2 some-graphs))) #{#{3}}))
  (is (= (set (self-recursive-sets (:g3 some-graphs))) #{#{3}}))
  (is (= (set (self-recursive-sets (:g4 some-graphs))) #{#{1 2} #{3}})))

(deftest test-fixed-point
  (is (thrown-with-msg? Exception #"Fixed point overflow" (dependency-list (:g1 some-graphs))))
  (is (thrown-with-msg? Exception #"Fixed point overflow" (dependency-list (:g2 some-graphs))))
  (is (thrown-with-msg? Exception #"Fixed point overflow" (dependency-list (:g3 some-graphs))))
  (is (thrown-with-msg? Exception #"Fixed point overflow" (dependency-list (:g4 some-graphs))))
  (is (= (dependency-list (remove-loops (:g3 some-graphs))) [#{1 2} #{3}]))
  (is (= (dependency-list (remove-loops (:g5 some-graphs))) [#{3 4} #{2} #{1}])))

(deftest test-stratification-list
  (is (= (stratification-list (remove-loops (:g3 some-graphs)) (:g3 some-graphs)) [#{1 2} #{3}]))
  (is (thrown? AssertionError (stratification-list (remove-loops (:g3 some-graphs)) (:g5 some-graphs))))
  (is (= (stratification-list (:g5 some-graphs) (:g5 some-graphs)) [#{3 4} #{2} #{1}]))
  (is (= (stratification-list (reverse-graph (:g5 some-graphs)) (reverse-graph (:g5 some-graphs))) [#{1} #{2} #{3 4}]))
  (is (= (stratification-list (make-directed-graph (nodes (:g5 some-graphs)) (fn [_] #{})) (:g5 some-graphs)) [#{1 2 3 4}])))

;;;

nil
