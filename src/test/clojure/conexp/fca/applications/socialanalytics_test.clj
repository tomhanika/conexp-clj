;; Copyright ⓒ the conexp-clj developers; all rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.fca.applications.socialanalytics-test
  (:require [conexp.fca.contexts :refer [random-context
                                         random-contexts
                                         make-context-from-matrix
                                         objects
                                         attributes]]
            [conexp.fca.applications.socialanalytics :refer :all]
            [clojure.test :refer [deftest is are]]
            [conexp.base :refer [with-testing-data close?]]))

;Average-shortest-path

(deftest test-context-graph-average-shortest-path
  (let [ctx (make-context-from-matrix 4 3
                          [1 0 1
                           0 1 0
                           1 0 0
                           1 1 0])
        ctx1 (make-context-from-matrix 3 3
                                [0 0 0
                                 0 1 0
                                 1 0 1])]
    (is (= (context-graph-average-shortest-path ctx) (/ 50 21)))
    (is (= (context-graph-average-shortest-path ctx1) (/ 5 4)))
    (is (= (context-graph-average-shortest-path (random-context 50 0)) nil)))

  (with-testing-data [ctx (random-contexts 5 50)]
    (let [value (context-graph-average-shortest-path ctx)]
      (or (nil? value)
          (<= 1
              value
              (+ (count (objects ctx))
                 (count (attributes ctx))))))))

(deftest test-object-projection-average-shortest-paths
  (let [ctx (make-context-from-matrix 3 3
                               [1 0 0
                                1 1 1
                                0 0 0])
        ctx1 (make-context-from-matrix ['a 'b 'c 'd 'e] ['a 'b 'c]
                                       [1 0 1
                                        0 1 0
                                        1 1 0
                                        0 1 1
                                        0 0 1])]
    (is (= (object-projection-average-shortest-path ctx) 1))
    (is (= (object-projection-average-shortest-path ctx1) (/ 13 10)))
    (is (nil? (object-projection-average-shortest-path (random-context 50 0)))))

  (with-testing-data [ctx (random-contexts 5 60)]
    (let [value (object-projection-average-shortest-path ctx)]
      (or (nil? value)
          (<= 1
              value
              (count (objects ctx)))))))

(deftest test-attribute-projection-average-shortest-path
  (let [ctx (make-context-from-matrix 4 5
                                      [1 0 1 1 0
                                       0 1 1 0 0
                                       1 0 0 1 0
                                       0 1 1 0 1])
        ctx1 (make-context-from-matrix 4 4
                                      [1 0 0 0
                                       1 1 0 0
                                       0 1 1 0
                                       0 0 1 1])]
    (is (= (attribute-projection-average-shortest-path ctx) (/ 7 5)))
    (is (= (attribute-projection-average-shortest-path ctx1) (/ 5 3)))
    (is (nil? (attribute-projection-average-shortest-path (random-context 60 0)))))

  (with-testing-data [ctx (random-contexts 7 50)]
    (let [value (attribute-projection-average-shortest-path ctx)]
          (or (nil? value)
              (<= 1
                  value
                  (count (attributes ctx)))))))

(deftest test-context-graph-vertex-degrees
  (let [ctx (make-context-from-matrix 5 3
                                      [1 1 0
                                       0 1 0
                                       0 0 1
                                       0 1 1
                                       1 0 1])
        ctx1 (make-context-from-matrix ['a, 'b, 'c, 'd] ['a, 'b, 'c]
                                       [1 1 0
                                        0 1 1
                                        1 0 0
                                        0 1 0])]
    (is (= (sort (context-graph-vertex-degrees ctx))
           '(1 1 2 2 2 2 3 3)))
    (is (= (sort (context-graph-vertex-degrees ctx1))
           '(1 1 1 2 2 2 3))))

  (with-testing-data [ctx (random-contexts 7 150)]
                     (let [degrees (context-graph-vertex-degrees ctx)
                           m (count (objects ctx))
                           n (count (attributes ctx))]
                       (and (= (count degrees) (+ m n))
                            (every? #(<= 0 % (max m n)) degrees)))))

(deftest test-object-projection-vertex-degrees
  (let [ctx (make-context-from-matrix 5 3
                                      [1 1 0
                                       0 1 0
                                       0 0 1
                                       0 1 1
                                       1 0 1])
        ctx1 (make-context-from-matrix ['a, 'b, 'c, 'd] ['a, 'b, 'c]
                                       [1 1 0
                                        0 1 1
                                        1 0 0
                                        0 1 0])]
    (is (= (sort (object-projection-vertex-degrees ctx))
           '(3 3 4 4 5)))
    (is (= (sort (object-projection-vertex-degrees ctx1))
           '(2 3 3 4))))

  (with-testing-data [ctx (random-contexts 7 150)]
                     (let [degrees (object-projection-vertex-degrees ctx)
                           n (count (objects ctx))]
                       (and (= (count degrees) n)
                            (every? #(<= 0 % n) degrees)))))

(deftest test-attribute-projection-vertex-degrees
  (let [ctx (make-context-from-matrix 5 3
                                      [1 1 0
                                       0 1 0
                                       0 0 1
                                       0 1 1
                                       1 0 1])
        ctx1 (make-context-from-matrix ['a, 'b, 'c] ['a, 'b, 'c, 'd, 'e]
                                       [1 0 0 1 0
                                        1 1 1 0 0
                                        0 0 0 1 0])]
    (is (= (sort (attribute-projection-vertex-degrees ctx))
           '(3 3 3)))
    (is (= (sort (attribute-projection-vertex-degrees ctx1))
           '(0 2 3 3 4))))

  (with-testing-data [ctx (random-contexts 7 150)]
                     (let [degrees (attribute-projection-vertex-degrees ctx)
                           n (count (attributes ctx))]
                       (and (= (count degrees) n)
                            (every? #(<= 0 % n) degrees)))))


;;; K-cores

(deftest test-k-cores
  (let [graph1 {0 #{1} 1 #{0 2 3} 2 #{1 3}
                3 #{1 2 4 5} 4#{3} 5 #{3}}
        graph2 {'a #{'b 'c 'd} 'b #{ 'a 'c 'd}
                'c #{'a 'b 'd} 'd #{'a 'b 'c 'e}
                'e #{'d 'f} 'f #{'e 'g 'h 'i}
                'g #{'f 'h 'i} 'h #{'f 'g 'i}
                'i #{'f 'g 'h}}]
   (is (= (k-cores graph1 2)
      (list 
        #{1 2 3})))
   (is (= (k-cores graph2 1)
          (list (set (keys graph2)))))
   (is (= (set (k-cores graph2 3))
          #{#{'a 'b 'c 'd }
            #{'f 'g 'h 'i}}))))

(deftest test-context-graph-k-cores
  (let [ctx (make-context-from-matrix 5 4
                                      [1 0 0 0
                                       1 1 0 0
                                       1 1 0 0
                                       0 0 1 0
                                       0 0 0 1])
        ctx1 (make-context-from-matrix [0 1 2]
                                       ['a 'b 'c 'd]
                                       [1 0 0 1
                                        1 1 0 1
                                        0 0 1 0])]

    (let [one-cores (context-graph-k-cores ctx 1)
          two-cores (context-graph-k-cores ctx 2)]
      (is (= (set one-cores) #{#{"obj-4", "atr-3"} #{"obj-3" "atr-2"}
                               #{"obj-0" "obj-1" "obj-2" "atr-1" "atr-0"}}))
      (is (= two-cores (list #{"obj-1" "obj-2" "atr-0" "atr-1"})))

    (let [one-cores (context-graph-k-cores ctx1 1)
          two-cores (context-graph-k-cores ctx1 2)]
      (is (= (set one-cores)
             #{#{"obj-2" "atr-c"} #{"obj-0" "obj-1" "atr-a" "atr-b" "atr-d"}}))
      (is (= two-cores
             (list #{"obj-0" "obj-1" "atr-a" "atr-d"}))))))
    (with-testing-data [ctx (random-contexts 10 100)]
      (empty? (context-graph-k-cores ctx 101))))

(deftest test-object-projection-k-cores
  (let [ctx (make-context-from-matrix 5 4
                                    [1 0 0 0
                                     1 1 0 0
                                     1 1 0 0
                                     0 0 1 0
                                     0 0 0 1])
      ctx1 (make-context-from-matrix [0 1 2]
                                     ['a 'b 'c 'd]
                                     [1 0 0 1
                                      1 1 0 1
                                      0 0 1 0])]

    (let [one-cores (object-projection-k-cores ctx 1)
          two-cores (object-projection-k-cores ctx 2)]
      (is (= (set one-cores) #{#{4} #{3} #{0 1 2}}))
      (is (= two-cores '(#{0 1 2})))
      (is (= two-cores (object-projection-k-cores ctx 3))))

    (let [one-cores (object-projection-k-cores ctx1 1)
          two-cores (object-projection-k-cores ctx1 2)]
      (is (= (set one-cores) #{#{2} #{0 1}}))
      (is (= two-cores '(#{0 1})))
      (is (empty? (object-projection-k-cores ctx1 3)))))

  (with-testing-data [ctx (random-contexts 10 75)]
    (is (empty? (object-projection-k-cores ctx 76)))))

(deftest test-attribute-projection-k-cores
  (let [ctx (make-context-from-matrix 5 4
                                    [1 0 0 0
                                     1 1 0 0
                                     1 1 0 0
                                     0 0 1 0
                                     0 0 0 1])
      ctx1 (make-context-from-matrix [0 1 2]
                                     ['a 'b 'c 'd]
                                     [1 0 0 1
                                      1 1 0 1
                                      0 0 1 0])]

    (let [one-cores (attribute-projection-k-cores ctx 1)
          two-cores (attribute-projection-k-cores ctx 2)]
      (is (= (set one-cores) #{#{0 1} #{2} #{3}}))
      (is (= two-cores '(#{0 1})))
      (is (empty? (attribute-projection-k-cores ctx 3))))

    (let [one-cores (attribute-projection-k-cores ctx1 1)
          two-cores (attribute-projection-k-cores ctx1 2)]
      (is (= (set one-cores) #{#{'c} #{'a 'b 'd}}))
      (is (= two-cores (list #{'a 'b 'd})))
      (is (= two-cores (attribute-projection-k-cores ctx1 3)))))

  (with-testing-data [ctx (random-contexts 20 42)]
    (is (empty? (attribute-projection-k-cores ctx 43)))))

;;;Average-shortest-path-via-bfs

(deftest test-context-graph-average-shortest-path-via-bfs
  (let [ctx (make-context-from-matrix 4 3
                          [1 0 1
                           0 1 0
                           1 0 0
                           1 1 0])
        ctx1 (make-context-from-matrix 3 3
                                [0 0 0
                                 0 1 0
                                 1 0 1])]
    (is (= (context-graph-average-shortest-path-via-bfs ctx) (/ 50 21)))
    (is (= (context-graph-average-shortest-path-via-bfs ctx1) (/ 5 4)))
    (is (= (context-graph-average-shortest-path-via-bfs (random-context 50 0)) nil)))
  
  (with-testing-data [ctx (random-contexts 5 50)]
    (let [value (context-graph-average-shortest-path-via-bfs ctx)]
      (or (nil? value)
          (<= 1
              value
              (+ (count (objects ctx))
                 (count (attributes ctx))))))))

(deftest test-object-projection-average-shortest-path-via-bfss
  (let [ctx (make-context-from-matrix 3 3
                               [1 0 0
                                1 1 1
                                0 0 0])
        ctx1 (make-context-from-matrix ['a 'b 'c 'd 'e] ['a 'b 'c]
                                       [1 0 1
                                        0 1 0
                                        1 1 0
                                        0 1 1
                                        0 0 1])]
    (is (= (object-projection-average-shortest-path-via-bfs ctx) 1))
    (is (= (object-projection-average-shortest-path-via-bfs ctx1) (/ 13 10)))
    (is (nil? (object-projection-average-shortest-path-via-bfs (random-context 50 0)))))
  
  (with-testing-data [ctx (random-contexts 5 60)]
    (let [value (object-projection-average-shortest-path-via-bfs ctx)]
      (or (nil? value)
          (<= 1
              value
              (count (objects ctx)))))))

(deftest test-attribute-projection-average-shortest-path-via-bfs
  (let [ctx (make-context-from-matrix 4 5
                                      [1 0 1 1 0
                                       0 1 1 0 0
                                       1 0 0 1 0
                                       0 1 1 0 1])
        ctx1 (make-context-from-matrix 4 4
                                      [1 0 0 0
                                       1 1 0 0
                                       0 1 1 0
                                       0 0 1 1])]
    (is (= (attribute-projection-average-shortest-path-via-bfs ctx) (/ 7 5)))
    (is (= (attribute-projection-average-shortest-path-via-bfs ctx1) (/ 5 3)))
    (is (nil? (attribute-projection-average-shortest-path-via-bfs (random-context 60 0)))))
  
  (with-testing-data [ctx (random-contexts 7 50)]
    (let [value (attribute-projection-average-shortest-path-via-bfs ctx)]
          (or (nil? value)
              (<= 1
                  value
                  (count (attributes ctx)))))))


;;; Clustering-coefficients

(deftest test-local-clustering-coefficient
  (let [g {0 #{1 2} 1 #{0 3} 2 #{0 3 4} 3 #{1 2 4} 4 #{2 3}}
        h {'a #{'a 'b 'c} 'b #{'a 'c} 'c #{'a 'b 0} 0 #{'c 'd 0}
           'd #{0 'e 'f} 'e #{'d 'f} 'f #{'d 'e}}]
    (is (= (set (map #(local-clustering-coefficient g %)
                     (keys g)))
           #{0 (/ 1 3) 1}))
    (is (= (set (map #(local-clustering-coefficient h %)
                     (keys h)))
           #{0 1 (/ 1 3)})))

  (with-testing-data [ctx (random-contexts 10 100)]
    (let [g (context-graph ctx)]
      (or (empty? g)
          (= (set (map #(local-clustering-coefficient g %)
                       (keys g)))
             #{0}))))

  (with-testing-data [ctx (take 10 (repeat (random-context 50 1.0)))]
    (let [g (attribute-projection ctx)]
      (or (empty? g)
          (= (set (map #(local-clustering-coefficient g %)
                       (keys g)))
             #{1})))))

(deftest test-clustering-coefficient
  (let [g {0 #{1 2} 1 #{0 3} 2 #{0 3 4} 3 #{1 2 4} 4 #{2 3}}
        h {'a #{'a 'b 'c} 'b #{'a 'c} 'c #{'a 'b 0} 0 #{'c 'd 0}
           'd #{0 'e 'f} 'e #{'d 'f} 'f #{'d 'e}}]
    (is (= (clustering-coefficient g)
           (/ 1 3))
        (= (clustering-coefficient h)
           (/ 2 3))))

  (with-testing-data [ctx (random-contexts 7 100)]
    (or (empty? (context-graph ctx))
        (= (clustering-coefficient (context-graph ctx))
           0)))

  (with-testing-data [ctx (take 10 (repeat (random-context 50 1.0)))]
    (= (clustering-coefficient (object-projection ctx))
       1)))

(deftest test-object-projection-clustering-coefficient
  (let [ctx (make-context-from-matrix 5 3 [1 1 0
                                           0 1 0
                                           0 0 1
                                           0 1 1
                                           1 0 1])
        ctx1 (make-context-from-matrix 4 3 [1 1 0
                                            0 1 1
                                            1 0 0
                                            0 1 0])]

    (is (= (object-projection-clustering-coefficient ctx)
           (/ 23 30)))
    (is (= (object-projection-clustering-coefficient ctx1)
           (/ 7 12))))

  (with-testing-data [ctx (random-contexts 7 100)]
    (or (empty? (object-projection ctx))
        (<= 0 (object-projection-clustering-coefficient ctx) 1))))

(deftest test-attribute-projection-average-locac-clustering-coefficient
  (let [ctx (make-context-from-matrix 3 5
                                      [1 0 0 1 0
                                       1 1 1 0 0
                                       0 0 0 1 0])
        ctx1 (make-context-from-matrix ['a 'b 'c 'd] 5
                                       [1 1 0 0 0
                                        0 1 1 0 0
                                        0 0 1 1 0
                                        1 0 0 1 1])]

    (is (= (attribute-projection-clustering-coefficient ctx)
           (/ 7 15)))
    (is (= (attribute-projection-clustering-coefficient ctx1)
           (/ 1 3))))

  (with-testing-data [ctx (random-contexts 7 100)]
    (or (empty? (attribute-projection ctx))
        (<= 0
            (attribute-projection-clustering-coefficient ctx)
            1))))


(deftest test-two-mode-local-clustering-coefficient
  (let [g { 'u #{ 1 2 3 4 5} 'v #{3 4 5 6 7 8} 1 #{'u}
           2 #{'u} 3 #{'u 'v} 4 #{ 'u 'v} 5 #{'u 'v}
           6 #{'v} 7 #{'v} 8 #{'v}}]
    (is (= (set (map #(two-mode-local-clustering-coefficient g %)
                     (keys g)))
           #{(/ 1 2) (/ 3 5) 1 (/ 9 14)})))

  (with-testing-data [ctx (random-contexts 5 50)]
    (let [g (context-graph ctx)]
      (every? #(<= 0 (two-mode-local-clustering-coefficient g %) 1)
              (keys g)))))

(deftest test-two-mode-clustering-coefficient
  (let [g { 'u #{ 1 2 3 4 5} 'v #{3 4 5 6 7 8} 1 #{'u}
           2 #{'u} 3 #{'u 'v} 4 #{ 'u 'v} 5 #{'u 'v}
           6 #{'v} 7 #{'v} 8 #{'v}}
        h { 1 #{'a 'b} 2 #{'c} 3 #{'b 'e} 4 #{'d 'e}
           'a #{1} 'b #{1 3} 'c #{ 2} 'd #{4} 'e #{3 4}}]

    (is (= (two-mode-clustering-coefficient g)
           (/ 281 350)))
    (is (= (two-mode-clustering-coefficient h)
           (/ 1 2))))

  (with-testing-data [ctx (random-contexts 5 50)]
    (let [g (context-graph ctx)]
      (<= 0 (two-mode-clustering-coefficient g) 1))))

(deftest test-context-graph-clustering-coefficient
  (let [ctx (make-context-from-matrix 4 4
                                      [1 0 1 0
                                       0 1 0 1
                                       0 1 0 0
                                       0 0 1 1])
        ctx1 (make-context-from-matrix ['a 'b 'c]
                                       [7 8 9 10]
                                       [1 0 0 1
                                        1 1 0 1
                                        0 0 1 0])]
    (is (= (context-graph-clustering-coefficient ctx)
           (/ 5 8)))
    (is (= (context-graph-clustering-coefficient ctx1)
           (/ 25 42))))

  (with-testing-data [ctx (random-contexts 7 100)]
    (<= 0 (context-graph-clustering-coefficient ctx) 1)))


;Betweenes-centrality

(deftest test-betweenes-centrality
  (let [g {1 #{2 3 4 5} 2 #{1 3} 3 #{1 2 5}
           4 #{1} 5#{1 3}}
        h {1 #{5} 2 #{4} 3 #{4} 4 #{2 3 5} 5#{1 4 6}
           6 #{5 7 8} 7 #{6 8 9} 8 #{6 7 9} 9 #{7 8}}
        i {'a #{'b 'c 'd} 'b #{'a 'e} 'c #{'a 'e}
           'd #{'a 'e} 'e #{'b 'c 'd}}
        j (zipmap (range 1 100) (repeat #{}))
        centrality-i (betweenes-centrality i)]
    (is (= (betweenes-centrality g) {1 7.0 2 0 3 1.0 4 0 5 0}))
    (is (= (betweenes-centrality h) {1 0 2 0 3 0 4 26.0 5 38.0 6 30.0
                                     7 6.0 8 6.0 9 0}))
    (are [x y] (close? x y 0.1)
         (centrality-i 'a) 3
         (centrality-i 'e) 3
         (centrality-i 'b) 0.66
         (centrality-i 'c) 0.66
         (centrality-i 'd) 0.66)
    (is (= (set (vals (betweenes-centrality j))) #{0}))))

(deftest test-betweenes-centrality-normalized
  (let [centrality-g (betweenes-centrality-normalized
                       {1 #{2 3 4 5} 2 #{1 3} 3 #{1 2 5}
                        4 #{1} 5#{1 3}})
        centrality-h (betweenes-centrality-normalized
                       {1 #{5} 2 #{4} 3 #{4} 4 #{2 3 5} 5#{1 4 6}
                        6 #{5 7 8} 7 #{6 8 9} 8 #{6 7 9} 9 #{7 8}})
        centrality-i (betweenes-centrality-normalized
                       {'a #{'b 'c 'd} 'b #{'a 'e} 'c #{'a 'e}
                        'd #{'a 'e} 'e #{'b 'c 'd}})
        j (zipmap (range 1 100) (repeat #{}))]
    (are [x y] (close? x y 0.1)
         (centrality-g 1) 1
         (centrality-g 2) 0
         (centrality-g 3) (/ 1 7)
         (centrality-g 4) 0
         (centrality-g 5) 0
         ;;;
         (centrality-h 1) 0
         (centrality-h 2) 0
         (centrality-h 3) 0
         (centrality-h 4) (/ 26 38)
         (centrality-h 5) 1
         (centrality-h 6) (/ 30 38)
         (centrality-h 7) (/ 6 30)
         (centrality-h 8) (/ 6 30)
         (centrality-h 9) 0
         ;;;
         (centrality-i 'a) 1
         (centrality-i 'b) 0
         (centrality-i 'c) 0
         (centrality-i 'd) 0
         (centrality-i 'e) 1)
    (is (= (set (vals (betweenes-centrality-normalized j)))) #{0})))


(deftest test-context-graph-betweenes-centrality
  (let [centrality-ctx (context-graph-betweenes-centrality
                         (make-context-from-matrix 5 4
                                                  [1 0 0 0
                                                   1 1 0 0
                                                   1 1 0 0
                                                   0 0 1 0
                                                   0 0 0 1 ]))
        solution-ctx {"obj-0" 0 "obj-1" 2.0 "obj-2" 2.0
                      "obj-3" 0 "obj-4" 0 "atr-0" 7.0
                      "atr-1" 1.0 "atr-2" 0 "atr-3" 0}
        centrality-ctx1 (context-graph-betweenes-centrality
                          (make-context-from-matrix ['a 'b 'c]
                                                    [2 4 6]
                                                    [1 0 0
                                                     1 1 0
                                                     0 1 1]))
        solution-ctx1 {"obj-a" 0 "obj-b" 12.0 "obj-c" 8.0
                       "atr-2" 8.0 "atr-4" 12.0 "atr-6" 0}]
    (is (every? #(= (centrality-ctx %) (solution-ctx %))
                (keys centrality-ctx)))
    (is (every? #(= (centrality-ctx1 %) (solution-ctx1 %))
                (keys centrality-ctx1))))
  (with-testing-data [ctx (random-contexts 20 20)]
    (let [ctx-graph-centrality (context-graph-betweenes-centrality ctx)
          centrality (betweenes-centrality (context-graph ctx))]
      (every? #(= (ctx-graph-centrality %) (centrality %))
              (keys ctx-graph-centrality)))))

(deftest test-object-projection-betweenes-centrality
  (let [ctx (make-context-from-matrix 5 4 [1 0 0 0
                                           1 1 0 0
                                           1 1 0 0
                                           0 0 1 0
                                           0 0 0 1])
        centrality-ctx1 (object-projection-betweenes-centrality
                          (make-context-from-matrix ['a 'b 'c] [2 4 6]
                                                    [1 0 0
                                                     1 1 0
                                                     0 1 1]))]
    (is (= (set (vals (object-projection-betweenes-centrality ctx)))
           #{0}))
    (are [x y] (= x y)
         (centrality-ctx1 'a) 0
         (centrality-ctx1 'b) 2.0
         (centrality-ctx1 'c) 0))
  (with-testing-data [ctx (random-contexts 20 20)]
    (let [obj-centrality (object-projection-betweenes-centrality ctx)
          centrality (betweenes-centrality (object-projection ctx))]
      (every? #(= (obj-centrality %) (centrality %))
              (keys (object-projection ctx))))))

(deftest test-attribute-projection-betweenes-centrality
  (let [ctx (make-context-from-matrix 5 4 [1 0 0 0
                                           1 1 0 0
                                           1 1 0 0
                                           0 0 1 0
                                           0 0 0 1])
        centrality-ctx1 (attribute-projection-betweenes-centrality
                          (make-context-from-matrix ['a 'b 'c] [2 4 6]
                                                    [1 0 0
                                                     1 1 0
                                                     0 1 1]))]
    (is (= (set (vals (attribute-projection-betweenes-centrality ctx)))
           #{0}))
    (are [x y] (= x y)
         (centrality-ctx1 2) 0
         (centrality-ctx1 4) 2.0
         (centrality-ctx1 6) 0))
  (with-testing-data [ctx (random-contexts 20 20)]
    (let [atr-centrality (attribute-projection-betweenes-centrality ctx)
          centrality (betweenes-centrality (attribute-projection ctx))]
      (every? #(= (atr-centrality %) (centrality %))
              (keys (attribute-projection ctx))))))

(deftest test-context-graph-betweenes-centrality-normalized
  (let [centrality-ctx (context-graph-betweenes-centrality-normalized
                         (make-context-from-matrix 5 4
                                                  [1 0 0 0
                                                   1 1 0 0
                                                   1 1 0 0
                                                   0 0 1 0
                                                   0 0 0 1 ]))
        solution-ctx {"obj-0" 0 "obj-1" (/ 2 7) "obj-2" (/ 2 7)
                      "obj-3" 0 "obj-4" 0 "atr-0" 1
                      "atr-1" (/ 1 7) "atr-2" 0 "atr-3" 0}
        centrality-ctx1 (context-graph-betweenes-centrality-normalized
                          (make-context-from-matrix ['a 'b 'c]
                                                    [2 4 6]
                                                    [1 0 0
                                                     1 1 0
                                                     0 1 1]))
        solution-ctx1 {"obj-a" 0 "obj-b" 1 "obj-c" (/ 2 3)
                       "atr-2" (/ 2 3) "atr-4" 1 "atr-6" 0}]
    (is (every? #(close? (centrality-ctx %) (solution-ctx %) 0.01)
                (keys centrality-ctx)))
    (is (every? #(close? (centrality-ctx1 %) (solution-ctx1 %) 0.01)
               (keys centrality-ctx1))))
  (with-testing-data [ctx (random-contexts 20 20)]
                     (let [hmap (context-graph-betweenes-centrality-normalized ctx)]
                       (or (empty? hmap)
                           (= #{0} (set (vals hmap)))
                           (= #{1} (set (vals hmap)))
                           (and (close? 1.0 (apply max (vals hmap)) 0.001)
                                (close? 0.0 (apply min (vals hmap)) 0.001))))))

(deftest test-object-projection-betweenes-centrality-normalized
  (let [ctx (make-context-from-matrix 5 4 [1 0 0 0
                                           1 1 0 0
                                           1 1 0 0
                                           0 0 1 0
                                           0 0 0 1])
        centrality-ctx1 (object-projection-betweenes-centrality-normalized
                          (make-context-from-matrix ['a 'b 'c] [2 4 6]
                                                    [1 0 0
                                                     1 1 0
                                                     0 1 1]))]
    (is (= #{0} (set (vals (object-projection-betweenes-centrality-normalized ctx)))))
    (is (= centrality-ctx1 {'a 0.0 'b 1.0 'c 0.0})))
  (with-testing-data [ctx (random-contexts 20 20)]
                     (let [hmap (object-projection-betweenes-centrality-normalized ctx)]
                       (or (empty? hmap)
                           (= 0 (apply max (vals hmap)))
                           (= 1 (apply min (vals hmap)))
                           (and (close? 1.0 (apply max (vals hmap)) 0.001)
                                (close? 0.0 (apply min (vals hmap)) 0.001))))))

(deftest test-attribute-projection-betweenes-centrality-normalized
  (let [ctx (make-context-from-matrix 5 4 [1 0 0 0
                                           1 1 0 0
                                           1 1 0 0
                                           0 0 1 0
                                           0 0 0 1])
        centrality-ctx1 (attribute-projection-betweenes-centrality-normalized
                          (make-context-from-matrix ['a 'b 'c] [2 4 6]
                                                    [1 0 0
                                                     1 1 0
                                                     0 1 1]))]
    (is (= #{0} (set (vals (attribute-projection-betweenes-centrality-normalized ctx)))))
    (is (= centrality-ctx1 {2 0.0 4 1.0 6 0.0})))
  (with-testing-data [ctx (random-contexts 20 20)]
                     (let [hmap (attribute-projection-betweenes-centrality-normalized ctx)]
                       (or (empty? hmap)
                           (= 0 (apply max (vals hmap)))
                           (= 1 (apply min (vals hmap)))
                           (and (close? 1.0 (apply max (vals hmap)) 0.001)
                                (close? 0.0 (apply min (vals hmap)) 0.001))))))


;;;

nil
