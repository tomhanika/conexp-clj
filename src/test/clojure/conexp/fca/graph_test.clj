;; Copyright ⓒ the conexp-clj developers; all rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.fca.graph-test
  (:require [clojure.test :refer :all]
            [conexp.base :exclude [transitive-closure] :refer :all]
            [conexp.fca.graph :refer :all]
            [conexp.util.graph :refer :all]
            [conexp.fca.lattices :as lat]
            [loom.graph :as lg]
            [loom.io :as li]))

;; helper function

(defn assert-equal-graphs
  ([graphs]
   (doseq [g (drop 1 graphs)]
     (assert-equal-graphs (first graphs) g)))
  ([g1 g2]
   (if (= g1 g2) (is true)                                  ; if equal: count as valid test
                 (do (println "graphs that do not match:")  ; else: print graphs and ...
                     (print (li/dot-str g1))
                     (print (li/dot-str g2))
                     (is (= g1 g2))))))                     ; ...do the test to report the fail

;;;

(def lat1
  (lat/make-lattice [1 2 3] [[1 1] [2 2] [3 3]
                             [1 2] [2 3] [1 3]]))

(def lat2
  (lat/make-lattice [1 2 3 4] [[1 1] [2 2] [3 3] [4 4]
                               [1 2] [1 3] [1 4] [2 4] [3 4]]))


(deftest test-lattice<->graph
  (assert-equal-graphs [
                        (lattice->graph lat1)
                        (add-loops (lg/digraph [1 2] [2 3] [1 3]))
                        (lg/digraph [1 1] [2 2] [3 3] [1 2] [2 3] [1 3])
                        (lattice->graph (graph->lattice (lg/digraph [1 2] [2 3] [1 3])))
                        ])
  (assert-equal-graphs [
                        (lattice->graph lat2)
                        (add-loops (lg/digraph [1 2] [1 3] [1 4] [2 4] [3 4]))
                        (lg/digraph [1 1] [2 2] [3 3] [4 4] [1 2] [1 3] [1 4] [2 4] [3 4])
                        (lattice->graph (graph->lattice (lg/digraph [1 2] [1 3] [1 4] [2 4] [3 4])))
                        ])
  (is (thrown? IllegalArgumentException (graph->lattice (lg/digraph [1 2] [2 3]))))
  (is (graph->lattice-nc (lg/digraph [1 2] [2 3]))))      ;note that no Exception is thrown

(deftest test-fail-illegal-graph->lattice
  "tests a graph that is transitive, but does not represent a lattice:
       6
      / \\
     4   5
     |\\ /|
     | X |
     |/ \\|
     2   3
      \\ /
       1
  Note that \\ is only a single edge. X is just the cross of two edges, *not* a
  node.
  This is not a representation of a lattice, because sup(2,3) is not defined
  properly."
  (let [g (transitive-closure
            (add-loops
              (lg/digraph [1 2] [1 3] [2 4] [2 5] [3 4] [3 5] [4 6] [5 6])))]
    (is (thrown? IllegalArgumentException (graph->lattice g)))))

(deftest test-comparability
  (assert-equal-graphs (comparability [1 2 3] (fn [a b] (contains? #{[1 1] [1 2] [2 3]} [a b])))
                       (lg/graph [1 1] [1 2] [2 3]))
  (assert-equal-graphs (co-comparability [1 2 3] (fn [a b] (contains? #{[1 1] [1 2] [2 3]} [a b])))
                       (lg/graph [1 3] [2 2] [3 3]))
  (assert-equal-graphs (comparability #{} (fn [_ _] (assert false "should not be called")))
                       (lg/graph))
  (assert-equal-graphs (co-comparability #{} (fn [_ _] (assert false "should not be called")))
                       (lg/graph))
  (assert-equal-graphs (comparability #{1} (fn [_ _] true))
                       (lg/graph [1 1]))
  (assert-equal-graphs (comparability #{1} (fn [_ _] false))
                       (lg/graph 1))
  (assert-equal-graphs (co-comparability #{1} (fn [_ _] true))
                       (lg/graph 1))
  (assert-equal-graphs (co-comparability #{1} (fn [_ _] false))
                       (lg/graph [1 1])))

(deftest test-consistency-digraph
  (let [cube-like-graph
        (transitive-closure
          (add-loops
            (lg/digraph [1 2] [1 3] [2 4] [3 4] [1 5] [2 6] [3 7] [4 8] [5 6] [5 7] [6 8] [7 8])))
        consistency (lg/digraph [[7 6] [7 6]] [[7 6] [3 6]] [[2 3] [2 3]] [[2 3] [2 7]]
                                  [[2 5] [2 5]] [[2 5] [2 7]] [[7 2] [7 2]] [[7 2] [7 4]]
                                  [[7 2] [7 6]] [[7 2] [3 2]] [[7 2] [3 6]] [[7 2] [5 2]]
                                  [[7 2] [5 4]] [[6 7] [6 7]] [[6 7] [2 7]] [[7 4] [7 4]]
                                  [[7 4] [5 4]] [[5 4] [5 4]] [[6 3] [6 3]] [[6 3] [6 7]]
                                  [[6 3] [6 4]] [[6 3] [2 3]] [[6 3] [2 7]] [[6 3] [5 3]]
                                  [[6 3] [5 4]] [[5 3] [5 3]] [[5 3] [5 4]] [[4 7] [4 7]]
                                  [[4 7] [2 7]] [[5 2] [5 2]] [[5 2] [5 4]] [[4 6] [4 6]]
                                  [[4 6] [3 6]] [[6 4] [6 4]] [[6 4] [5 4]] [[2 7] [2 7]]
                                  [[3 6] [3 6]] [[4 5] [4 5]] [[4 5] [4 7]] [[4 5] [4 6]]
                                  [[4 5] [2 5]] [[4 5] [2 7]] [[4 5] [3 5]] [[4 5] [3 6]]
                                  [[3 5] [3 5]] [[3 5] [3 6]] [[3 2] [3 2]] [[3 2] [3 6]])]
    (is (= (consistency-digraph cube-like-graph)
               consistency)))
  (is (thrown? AssertionError (consistency-digraph (lg/digraph [1 2] [2 3]))))
  (is (= (consistency-digraph (transitive-closure (lg/graph [1 2] [3 4]))) ; consistency-digraph also works on undirected graphs! (may be useful when having equivalent nodes (a <= b && b <= a))
         (lg/digraph [[2 3] [2 3]] [[2 3] [2 4]] [[2 3] [1 3]] [[2 3] [1 4]]
                       [[4 2] [4 2]] [[4 2] [4 1]] [[4 2] [3 2]] [[4 2] [3 1]]
                       [[4 1] [4 1]] [[4 1] [4 2]] [[4 1] [3 1]] [[4 1] [3 2]]
                       [[1 4] [1 4]] [[1 4] [1 3]] [[1 4] [2 4]] [[1 4] [2 3]]
                       [[1 3] [1 3]] [[1 3] [1 4]] [[1 3] [2 3]] [[1 3] [2 4]]
                       [[2 4] [2 4]] [[2 4] [2 3]] [[2 4] [1 4]] [[2 4] [1 3]]
                       [[3 1] [3 1]] [[3 1] [3 2]] [[3 1] [4 1]] [[3 1] [4 2]]
                       [[3 2] [3 2]] [[3 2] [3 1]] [[3 2] [4 2]] [[3 2] [4 1]]))))

(deftest test-incompatibility-graph
  (let [g2 (transitive-closure (add-loops (lg/digraph [1 3] [1 4] [2 3] [2 4] [3 4] [5 4] 6)))]
    (is (= (incompatibility-graph g2)
           (lg/graph [[4 6] [6 5]] [[3 5] [5 2]] [[3 5] [5 1]]
                       [[6 1] [4 6]] [[6 2] [4 6]] [[1 2] [2 1]])))))

(deftest test-strictness
  (let [less (strict <=)
        le (non-strict <)]
    (is (less 1 2))
    (is (less [1 2]))
    (is (not (less 1 1)))
    (is (not (less 1 0)))
    (is (le 1 2))
    (is (le [1 2]))
    (is (le 1 1))
    (is (not (le 1 0)))))

;;;

nil
