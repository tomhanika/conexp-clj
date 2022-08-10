;; Copyright ⓒ the conexp-clj developers; all rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.fca.posets-test
  (:use clojure.test)
  (:require [conexp.base :refer :all]
            [conexp.fca.contexts :refer [make-context-from-matrix]]
            [conexp.fca.posets :refer :all]
            [conexp.math.algebra :refer [base-set order]]))

(def adj0 #{})

(def adj1 #{['a 'a]['b 'b]['c 'c]['d 'd]
            ['a 'c]['a 'd]['b 'd]})

(def adj2 #{['a 'c]['a 'd]['b 'd]})

(def adj3 #{[1 2][2 3][3 4][4 1]
            [1 1][2 2][3 3][4 4]})

(def adj4 #{[1 2][2 3][3 4]
            [1 'a][2 'b][3 'c][4 'd]
            [1 1][2 2][3 3][4 4]
            [1 3][1 4][2 4]
            [1 'b][1 'c][1 'd]
            [2 'c][2 'd][3 'd]
            ['a 'a]['b 'b]['c 'c]['d 'd]})

(def fast-poset (fn [a] (make-poset (set (apply concat a))
                                    (fn [x y] (some #{[x y]} a)))))

(def testing-posets
  (map fast-poset [adj0 adj1 adj4]))

(deftest test-make-poset
  (is (fast-poset adj0))
  (is (fast-poset adj1))
  (is (thrown? IllegalArgumentException 
               (fast-poset adj2)))
  (is (thrown? IllegalArgumentException 
               (fast-poset adj3)))
  (is (fast-poset adj4)))

(deftest test-poset-to-matrix
  (is (= []
         (poset-to-matrix (fast-poset adj0))))
  (is (= [1 0 1 1  ;a 
          0 1 0 1  ;b
          0 0 1 0  ;c
          0 0 0 1] ;d
         (poset-to-matrix (fast-poset adj1)
                          ['a 'b 'c 'd])))
  (is (= [1 1 1 1  1 1 1 1  ;1
          0 1 1 1  0 1 1 1  ;2
          0 0 1 1  0 0 1 1  ;3
          0 0 0 1  0 0 0 1  ;4
          0 0 0 0  1 0 0 0  ;a
          0 0 0 0  0 1 0 0  ;b
          0 0 0 0  0 0 1 0  ;c
          0 0 0 0  0 0 0 1] ;d
         (poset-to-matrix (fast-poset adj4)
                          [1 2 3 4 'a 'b 'c 'd]))))

(deftest test-poset-context
  (are [poset context] (= (poset-context poset) context)
    (fast-poset adj0) (make-context-from-matrix [] [] #{})
    (fast-poset adj1) (make-context-from-matrix ['a 'b 'c 'd] ['a 'b 'c 'd]
                                                [1 0 1 1
                                                 0 1 0 1
                                                 0 0 1 0
                                                 0 0 0 1])
    (fast-poset adj4) (make-context-from-matrix [1 2 3 4 'a 'b 'c 'd]
                                                [1 2 3 4 'a 'b 'c 'd]
                                                [1 1 1 1  1 1 1 1
                                                 0 1 1 1  0 1 1 1
                                                 0 0 1 1  0 0 1 1
                                                 0 0 0 1  0 0 0 1
                                                 0 0 0 0  1 0 0 0
                                                 0 0 0 0  0 1 0 0
                                                 0 0 0 0  0 0 1 0
                                                 0 0 0 0  0 0 0 1])))

(deftest test-directly-neighboured?
  (with-testing-data [poset testing-posets]
    (forall [x (base-set poset),
             y (base-set poset)]
            (<=> (directly-neighboured? poset x y)
                 (and (not= x y)
                      ((order poset) x y)
                      (forall [z (base-set poset)]
                              (=> (and ((order poset) x z)
                                       ((order poset) z y))
                                  (or (= x z) (= y z)))))))))

(deftest test-poset-upper-neighbours
  (with-testing-data [poset testing-posets]
    (forall [x (base-set poset)]
            (let [xs (set (poset-upper-neighbours poset x))]
              (forall [y (base-set poset)]
                      (<=> (directly-neighboured? poset x y)
                           (contains? xs y)))))))

(deftest test-poset-lower-neighbours
  (with-testing-data [poset testing-posets]
    (forall [x (base-set poset)]
            (let [xs (set (poset-lower-neighbours poset x))]
              (forall [y (base-set poset)]
                      (<=> (directly-neighboured? poset y x)
                           (contains? xs y)))))))
