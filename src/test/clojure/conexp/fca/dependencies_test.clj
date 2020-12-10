;; Copyright ⓒ the conexp-clj developers; all rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.fca.dependencies-test
  (:use clojure.test)
  (:use conexp.base
        conexp.fca.contexts
        conexp.fca.many-valued-contexts
        conexp.fca.implications
        conexp.fca.dependencies))

;;;

(def mv1 (make-mv-context #{1 2 3} #{5 6} 
                          #{[1 5 'a] [1 6 'b]
                            [2 5 'A] [2 6 'B]
                            [3 5 'a] [3 6 'b]}))

(def mv2 (make-mv-context #{1 2 3} #{5 6} 
                          #{[1 5 'a] [1 6 'b]
                            [2 5 'A] [2 6 'B]
                            [3 5 'c] [3 6 'b]}))

(def mv3 (make-mv-context #{1 2 3 4 5} #{6 7 8 9 10} 
                          #{[1 6 1][1 7 'B][1 8 'c][1 9 'C][1 10 'e]
                            [2 6 2][2 7 'A][2 8 'c][2 9 'C][2 10 'e]
                            [3 6 1][3 7 'B][3 8 'c][3 9 'D][3 10 'e]
                            [4 6 2][4 7 'A][4 8 'd][4 9 'D][4 10 'e]
                            [5 6 1][5 7 'B][5 8 'd][5 9 'D][5 10 'e]}))

(deftest test-dependency
  (is (= (dependencies mv1 {})
         (dependencies mv1 {} :weak)
         #{(make-implication #{5} #{6}) 
           (make-implication #{6} #{5})}))
  (is (= (dependencies mv1 {5 (nominal-scale '[a A])
                            6 (nominal-scale '[b B])})
         (dependencies mv1 {5 (nominal-scale '[a A])
                            6 (nominal-scale '[b B])}
                           :weak)
         #{(make-implication #{5} #{6}) 
           (make-implication #{6} #{5})}))
  (is (= (dependencies mv2 {5 (nominal-scale '[a A])
                            6 (nominal-scale '[b B])})
         (dependencies mv2 {5 (nominal-scale '[a A])
                            6 (nominal-scale '[b B])}
                           :weak)
         #{}))
  (is (let [scales {6 (ordinal-scale [1 2])
                    7 (nominal-scale '[A B])
                    8 (nominal-scale '[c d])
                    9 (nominal-scale '[C D])
                    10 (nominal-scale '[e])}]
        (= (dependencies mv3 scales)
           (dependencies mv3 scales :weak)
         #{(make-implication #{10} #{7 6 9 8}) 
           (make-implication #{6} #{10 7 9 8})}))))

(deftest test-functional-dependency
  (is (= (functional-dependencies mv1)
         #{(make-implication #{5} #{6})
           (make-implication #{6} #{5})}))
  (is (= (functional-dependencies mv2)
         #{(make-implication #{5} #{6})}))
  (is (= (functional-dependencies mv3)
         #{(make-implication #{6} #{7 10})
           (make-implication #{7} #{6 10})
           (make-implication #{8} #{10})
           (make-implication #{9} #{10})})))

;;; end

true
