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
            [clojure.test :refer [deftest is]]
            [conexp.base :refer [with-testing-data]]))

;; Average-shortest-path

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

;; K-cores

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

;;

true