(ns conexp.layouts.dim-draw-test
  (:require [clojure.test :refer :all]
            [ubergraph.core :as uber]
            [conexp.fca.lattices :as lat]
            [conexp.fca.graph :refer :all]
            [conexp.util.graph :refer :all]
            [conexp.base :exclude [transitive-closure] :refer :all]
            [loom.graph :as lg]
            [rolling-stones.core :as sat :refer :all]
            [conexp.layouts.base :as lay])
  (:use conexp.layouts.dim-draw))



(def g1 (transitive-closure (add-loops (uber/digraph))))

(def g2 (transitive-closure (add-loops (uber/digraph 0))))

(def g5 (transitive-closure (add-loops (uber/digraph
                                         [0 1] [0 2]
                                         [1 3]
                                         [2 3]))))

(def g686 (transitive-closure (add-loops (uber/digraph
                                           [0 1]
                                           [1 2] [1 4] [1 7]
                                           [2 3]
                                           [3 5]
                                           [4 5]
                                           [5 6]
                                           [6 8]
                                           [7 8]))))

(def g774 (transitive-closure (add-loops (uber/digraph
                                           [0 1] [0 2] [0 5]
                                           [1 4] [1 6]
                                           [2 3] [2 7]
                                           [3 4]
                                           [4 8]
                                           [5 6] [5 7]
                                           [6 8]
                                           [7 8]))))

(def g44986 (transitive-closure (add-loops (uber/digraph
                                             [0 1]
                                             [1 2] [1 5]
                                             [2 3]
                                             [3 4] [3 6]
                                             [4 8]
                                             [5 10]
                                             [6 7]
                                             [7 8] [7 9]
                                             [8 10]
                                             [9 10]))))

(def g44995 (transitive-closure (add-loops (uber/digraph
                                             [0 1]
                                             [1 8] [1 2] [1 5]
                                             [2 3]
                                             [3 9] [3 4]
                                             [4 10]
                                             [5 6]
                                             [6 7]
                                             [7 9]
                                             [8 9]
                                             [9 10]))))

(def gDiamond (transitive-closure (add-loops (uber/digraph
                                               [0 1]
                                               [1 2]
                                               [0 3]
                                               [3 2]
                                               ))))

(def gShortChains (transitive-closure (add-loops (uber/digraph
                                                   [0 1]
                                                   [1 2]
                                                   [2 3]
                                                   [0 4]
                                                   [4 5]
                                                   [5 3]
                                                   ))))

(def gLongChains (transitive-closure (add-loops (uber/digraph
                                                  [0 1]
                                                  [1 2]
                                                  [2 3]
                                                  [3 4]
                                                  [0 5]
                                                  [5 6]
                                                  [6 7]
                                                  [7 4]))))

(def gThreeLongChains (transitive-closure (add-loops (uber/digraph
                                                       [0 1]
                                                       [1 2]
                                                       [2 3]
                                                       [3 4]
                                                       [0 5]
                                                       [5 6]
                                                       [6 7]
                                                       [7 4]
                                                       [0 8]
                                                       [8 9]
                                                       [9 10]
                                                       [10 4]))))

(def gParallel (transitive-closure (add-loops (uber/digraph
                                                [0 1]
                                                [1 5]
                                                [0 2]
                                                [2 5]
                                                [0 3]
                                                [3 5]
                                                [0 4]
                                                [4 5]
                                                ))))

(def cube-like-graph
  (transitive-closure
    (add-loops (uber/digraph
                 [1 2] [1 3] [2 4] [3 4] [1 5] [2 6]
                 [3 7] [4 8] [5 6] [5 7] [6 8] [7 8]))))

(def two-cubes-like-graph
  (transitive-closure
    (add-loops (uber/digraph
                 [1 2] [1 3] [2 4] [3 4] [1 5] [2 6]
                 [3 7] [4 8] [5 6] [5 7] [6 8] [7 8]
                 [8 9]
                 [9 10] [9 11] [10 12] [11 12] [9 13] [10 14]
                 [11 15] [12 16] [13 14] [13 15] [14 16] [15 16]))))

(def hyper-cube-like-graph
  (transitive-closure
    (add-loops (uber/digraph
                 ;[1 2] [1 3] [2 4] [3 4] [1 5] [2 6]
                 ;[3 7] [4 8] [5 6] [5 7] [6 8] [7 8]
                 ;[1 9] [2 10] [3 11] [4 12] [5 13] [6 14] [7 15] [8 16]
                 ;[9 10] [9 11] [10 12] [11 12] [9 13] [10 14]
                 ;[11 15] [12 16] [13 14] [13 15] [14 16] [15 16]))))
                 [0 1] [0 2] [1 3] [2 3] [0 4] [1 5]
                 [2 6] [3 7] [4 5] [4 6] [5 7] [6 7]
                 [0 8] [1 9] [2 10] [3 11] [4 12] [5 13] [6 14] [7 15]
                 [8 9] [8 10] [9 11] [10 11] [8 12] [9 13]
                 [10 14] [11 15] [12 13] [12 14] [13 15] [14 15]))))

(def g-dominik's-favorite
  (transitive-closure
    (add-loops
      (lg/transpose
        (uber/digraph [0 1] [0 2] [0 3] [0 4]
                      [1 5] [1 9]
                      [2 7] [2 10] [2 11]
                      [3 8] [3 9] [3 10]
                      [4 6] [4 11]
                      [5 7] [5 12]
                      [6 8] [6 13]
                      [7 14] [7 15]
                      [8 17]
                      [9 12]
                      [10 15] [10 17]
                      [11 13] [11 16]
                      [12 15]
                      [13 17]
                      [14 18]
                      [15 18]
                      [16 18]
                      [17 18])))))

(deftest test-compute-conjugate-order
  (is (=
        (set (compute-conjugate-order (nodes g1) #(lg/has-edge? g1 %1 %2)))
        #{}))
  (is (=
        (set (compute-conjugate-order (nodes g2) #(lg/has-edge? g2 %1 %2)))
        #{}))
  (is (=
        (set (compute-conjugate-order (nodes g5) #(lg/has-edge? g5 %1 %2)))
        #{[1 2]}))
  (is (=
        (set (compute-conjugate-order (nodes g686) #(lg/has-edge? g686 %1 %2)))
        #{[2 4] [2 7] [4 7] [3 4] [3 7] [5 7] [6 7]}))
  (is (=
        (set (compute-conjugate-order (nodes g774) #(lg/has-edge? g774 %1 %2)))
        #{}))
  (is (=
        (set (compute-conjugate-order (nodes g44986) #(lg/has-edge? g44986 %1 %2)))
        #{[6 4] [6 5] [4 5] [9 8] [9 4] [9 5] [8 5] [7 4] [7 5] [2 5] [3 5]}))
  (is (=
        (set (compute-conjugate-order (nodes g44995) #(lg/has-edge? g44995 %1 %2)))
        #{[6 2] [6 3] [6 4] [6 8] [5 3] [5 4] [5 2] [5 8]
          [7 3] [7 2] [7 4] [7 8] [8 2] [8 3] [8 4] [9 4]})))


(deftest test-lt-seq
  (is (not= nil
            (sat/solutions-symbolic-cnf (lt-seq [] 0 "s"))))
  (is (thrown? AssertionError
               (sat/solutions-symbolic-cnf (lt-seq [] -1 "s"))))
  (is (not= nil
            (sat/solutions-symbolic-cnf (concat (lt-seq [] 0 "s")
                                                [[:a]]))))
  (is (not= nil
            (sat/solutions-symbolic-cnf (lt-seq [:a :b :c] 0 "s"))))
  (is (= nil
         (sat/solutions-symbolic-cnf (concat (lt-seq [:a :b :c] 0 "s")
                                             [[:a]]))))
  (is (= nil
         (sat/solutions-symbolic-cnf (concat (lt-seq [:a :b :c] 0)
                                             [[:b]]))))
  (is (= nil
         (sat/solutions-symbolic-cnf (concat (lt-seq [:a :b :c] 0)
                                             [[:c]]))))
  (is (not= nil
            (sat/solutions-symbolic-cnf (lt-seq [:a :b :c] 1 "s"))))
  (is (not= nil
            (sat/solutions-symbolic-cnf (lt-seq [:a :b :c] 5 "s"))))
  (is (= nil
         (sat/solutions-symbolic-cnf (concat (lt-seq [:a :b :c] 0 "s")
                                             [[:a] [:b]]))))
  (is (= nil
         (sat/solutions-symbolic-cnf (concat (lt-seq [:a :b :c] 1 "s")
                                             [[:a] [:b]]))))
  (is (not= nil
            (sat/solutions-symbolic-cnf (concat (lt-seq [:a :b :c] 2 "s")
                                                [[:a] [:b]])))))


(deftest test-sat-reduction
  (is (let [red (sat-reduction (uber/graph [:a :b] [:a :c] [:b :e] [:c :e] [:a :e]) 1)]
        (or (= red '(:a))
            (= red '(:e)))))
  (is (= (sat-reduction (tig g1))
         '()))
  (is (= (sat-reduction (tig g2))
         '()))
  (is (= (sat-reduction (tig g5))
         '()))
  (is (= (sat-reduction (tig g686))
         '()))
  (is (= (sat-reduction (tig g774))
         '([7 1])))
  (is (= (sat-reduction (tig g44986))
         '()))
  (is (= (sat-reduction (tig g-dominik's-favorite))
         '([2 8] [2 3] [2 9] [2 12] [2 6])))
  (is (= (sat-reduction (tig hyper-cube-like-graph))
         '([10 5] [11 4] [11 6] [6 1] [10 1] [14 1] [14 5] [10 4] [11 5] [9 4] [14 9])))
  (is (= (sat-reduction (uber/graph [:a :b] [:a :c] [:b :e] [:c :e] [:a :e]) 0)
         nil))
  (is (= (sat-reduction (uber/graph [:a :b] [:a :c] [:b :e] [:c :e] [:a :e]) 1)
         '(:e)))
  (is (= (sat-reduction (uber/graph [:a :b] [:a :c] [:b :e] [:c :e] [:a :e]) 2)
         '(:e)))
  (is (= (sat-reduction (uber/graph [:a :b] [:a :c] [:b :e] [:c :e] [:a :e]))
         '(:e))))


(deftest test-compute-coordinates
  (is (= (compute-coordinates g1)
         '()))
  (is (= (compute-coordinates g2)
         '([0 [0 0]])))
  (is (= (compute-coordinates g5)
         '([0 [0 0]] [1 [1 2]] [2 [2 1]] [3 [3 3]])))
  (is (= (compute-coordinates g686)
         '([0 [0 0]] [1 [1 1]] [2 [2 4]] [4 [4 3]] [7 [7 2]] [3 [3 5]] [5 [5 6]] [6 [6 7]] [8 [8 8]])))
  (is (= (compute-coordinates g774)
         '([0 [0 0]] [1 [2 3]] [2 [4 1]] [5 [1 5]] [4 [7 4]] [6 [3 7]] [3 [6 2]] [7 [5 6]] [8 [8 8]])))
  (is (= (compute-coordinates g44995)
         '([0 [0 0]] [7 [4 8]] [1 [1 1]] [4 [9 4]] [6 [3 7]] [3 [7 3]] [2 [6 2]] [9 [8 9]] [5 [2 6]] [10 [10 10]] [8 [5 5]])))
  (is (= (compute-coordinates g-dominik's-favorite)
         '([0 [18 18]] [7 [3 14]] [1 [7 17]] [4 [17 7]] [15 [2 8]] [13 [12 3]]
            [6 [13 6]] [17 [8 2]] [3 [11 12]] [12 [4 10]] [2 [16 15]] [11 [15 4]]
            [9 [6 11]] [5 [5 16]] [14 [1 13]] [16 [14 1]] [10 [9 9]] [18 [0 0]] [8 [10 5]])))
  (is (= (compute-coordinates hyper-cube-like-graph)
         '([0 [0 0]] [7 [7 14]] [1 [2 4]] [4 [4 2]] [15 [15 15]] [13 [14 7]]
            [6 [5 10]] [3 [3 12]] [12 [12 3]] [2 [1 8]] [11 [11 13]] [9 [10 5]]
            [5 [6 6]] [14 [13 11]] [10 [9 9]] [8 [8 1]])))
  (is (= (compute-coordinates gLongChains)
         '([0 [0 0]] [1 [1 4]] [2 [2 5]] [3 [3 6]] [4 [7 7]] [5 [4 1]] [6 [5 2]] [7 [6 3]])))
  (is (= (compute-coordinates gThreeLongChains)
         '([0 [0 0]] [7 [3 9]] [1 [4 4]] [4 [10 10]] [6 [2 8]] [3 [6 6]] [2 [5 5]] [9 [8 2]] [5 [1 7]] [10 [9 3]] [8 [7 1]])))
  (is (= (compute-coordinates gParallel)
         '([0 [0 0]] [1 [1 4]] [5 [5 5]] [2 [2 3]] [3 [3 2]] [4 [4 1]]))))


;; test drawing layout
;(conexp.gui.draw/draw-layout (dim-draw-layout (graph->lattice g-dominik's-favorite)))
