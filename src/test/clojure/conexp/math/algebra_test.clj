;; Copyright ⓒ the conexp-clj developers; all rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.math.algebra-test
  (:use conexp.math.algebra)
  (:use clojure.test))

;;;

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

