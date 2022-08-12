;; Copyright ⓒ the conexp-clj developers; all rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.layouts.common-test
  (:use conexp.base
        conexp.math.algebra
        conexp.fca.lattices
        conexp.fca.posets
        conexp.layouts.base
        conexp.layouts.common)
  (:use clojure.test))

;;;

(def- test-lattice (make-lattice (subsets #{1 2 3})
                                 subset?))

(def- test-poset (make-poset [1 2 3 4 5]
                             (fn [A B]
                               (contains? #{[1 1] [1 3] [1 4] [1 5]
                                            [2 2] [2 3] [2 4] [2 5]
                                            [3 3] [3 5]
                                            [4 4] [4 5]
                                            [5 5]}
                                          [A B]))))

(deftest test-placement-by-initials
  (let [lattice-placement (placement-by-initials test-lattice
                                                 [1 1]
                                                 {#{1 2} [-2 0], 
                                                  #{1 3} [0 0], 
                                                  #{2 3} [2 0]})
        poset-placement (placement-by-initials test-poset
                                               [1 1]
                                               {3 [0 0],
                                                4 [2 0]})]
    (is (= lattice-placement
           {#{} [-2 -2],
            #{1} [-3 -1],
            #{2} [-1 -1],
            #{3} [1 -1],
            #{1 2} [-2 0],
            #{1 3} [0 0],
            #{2 3} [2 0],
            #{1 2 3} [1 1]}))
    (is (= poset-placement
           {5 [1 1],
            4 [2 0],
            3 [0 0],
            2 [1 -1]
            1 [1 -1]}))))

(def- test-layout-1 (make-layout {#{} [-2 -6],
                                #{1} [-5 -1],
                                #{2} [-1 -1],
                                #{3} [0 -1],
                                #{1 2} [-2 0],
                                #{1 3} [0 0],
                                #{2 3} [2 0],
                                #{1 2 3} [1 1]}
                               [[#{} #{1}], [#{} #{2}], [#{} #{3}],
                                [#{1} #{1 2}], [#{1} #{1 3}],
                                [#{2} #{1 2}], [#{2} #{2 3}],
                                [#{3} #{1 3}], [#{3} #{2 3}],
                                [#{1 2} #{1 2 3}],
                                [#{1 3} #{1 2 3}],
                                [#{2 3} #{1 2 3}]]))
(def test-layout-2 (make-layout {1 [1 1],
                                  2 [-2 3],
                                  3 [0 3],
                                  4 [4 -1],
                                  5 [4 4],
                                  6 [-1 3]}
                                 [[1 2] [1 3] [1 5]
                                  [2 6] [3 6] [4 5]]))

(deftest test-to-inf-additive-layout
  (is (= (positions (to-inf-additive-layout test-layout-1))
         {#{} [-2 -2],
          #{1} [-3 -1],
          #{2} [-1 -1],
          #{3} [1 -1],
          #{1 2} [-2 0],
          #{1 3} [0 0],
          #{2 3} [2 0],
          #{1 2 3} [1 1]}))
  (is (= (count (positions (to-inf-additive-layout test-layout-2)))
         6)))

(deftest test-layout-by-placement
  (is (= (positions (layout-by-placement test-lattice
                                         [1 1]
                                         {#{1 2} [-2 0], #{1 3} [0 0], #{2 3} [2 0]}))
         {#{} [-2 -2],
          #{1} [-3 -1],
          #{2} [-1 -1],
          #{3} [1 -1],
          #{1 2} [-2 0],
          #{1 3} [0 0],
          #{2 3} [2 0],
          #{1 2 3} [1 1]}))
  (is (= (positions (layout-by-placement test-poset
                                         [1 1]
                                         {3 [0 0], 4 [2 0]}))
         {5 [1 1],
          4 [2 0],
          3 [0 0],
          2 [1 -1],
          1 [1 -1]})))

;;;

nil
