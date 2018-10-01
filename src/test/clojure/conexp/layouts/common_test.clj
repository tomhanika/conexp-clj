;; Copyright ⓒ the conexp-clj developers; all rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.layouts.common-test
  (:use conexp.base
        conexp.fca.lattices
        conexp.layouts.base
        conexp.layouts.common)
  (:use clojure.test))

;;;

(deftest test-placement-by-initials
  (let [placement (placement-by-initials (make-lattice (subsets #{1 2 3})
                                                       subset?)
                                         [1 1]
                                         {#{1 2} [-2 0], #{1 3} [0 0], #{2 3} [2 0]})]
    (is (= placement
           {#{} [-2 -2],
            #{1} [-3 -1],
            #{2} [-1 -1],
            #{3} [1 -1],
            #{1 2} [-2 0],
            #{1 3} [0 0],
            #{2 3} [2 0],
            #{1 2 3} [1 1]}))))

(def- test-lattice (make-lattice (subsets #{1 2 3})
                                     subset?))

(def- test-layout (make-layout {#{} [-2 -6],
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

(deftest test-to-inf-additive-layout
  (is (= (positions (to-inf-additive-layout test-layout))
         {#{} [-2 -2],
          #{1} [-3 -1],
          #{2} [-1 -1],
          #{3} [1 -1],
          #{1 2} [-2 0],
          #{1 3} [0 0],
          #{2 3} [2 0],
          #{1 2 3} [1 1]})))

(deftest test-layout-by-placement
  (is (= (positions (layout-by-placement (make-lattice (subsets #{1 2 3})
                                                       subset?)
                                         [1 1]
                                         {#{1 2} [-2 0], #{1 3} [0 0], #{2 3} [2 0]}))
         {#{} [-2 -2],
          #{1} [-3 -1],
          #{2} [-1 -1],
          #{3} [1 -1],
          #{1 2} [-2 0],
          #{1 3} [0 0],
          #{2 3} [2 0],
          #{1 2 3} [1 1]})))


;;;

nil
