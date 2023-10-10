;; Copyright ⓒ the conexp-clj developers; all rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.fca.simplicial-complexes-test
  (:use clojure.test)
  (:require [conexp.fca.simplicial-complexes :refer :all]
            [conexp.fca.contexts :refer [make-context-from-matrix]])
  (:import conexp.fca.simplicial_complexes.FullSimplicialComplex))

(deftest test-FullSimplicialComplex-equals
  (is (= (FullSimplicialComplex. #{} #{})
         (FullSimplicialComplex. #{} #{})))
  (is (= (FullSimplicialComplex. #{1 2 3} #{#{} #{1} #{2} #{3}})
         (FullSimplicialComplex. #{1 2 3} #{#{} #{1} #{2} #{3}})))
  (is (= (FullSimplicialComplex. #{1 2 3} #{#{} #{1} #{2} #{3}})
         (FullSimplicialComplex. #{1 2 3} #{#{} #{3} #{2} #{1}})))
  (is (= (FullSimplicialComplex. #{1 2 3} #{#{} #{1} #{2} #{3}})
         (FullSimplicialComplex. #{1 2 3} '(#{} #{3} #{2} #{1}))))
  (is (= (FullSimplicialComplex. #{1 2 3} #{#{} #{1} #{2} #{3}})
         (FullSimplicialComplex. #{1 2 3} [#{} #{3} #{2} #{1}])))
  (is (not= (FullSimplicialComplex. #{1 2 3} #{#{} #{1} #{2} #{3}})
            (FullSimplicialComplex. #{1 2 3} #{#{} #{1} #{2} #{3} #{1 2}})))
  (is (not= (FullSimplicialComplex. #{1 2 3} #{#{} #{1} #{2} #{3}})
            (FullSimplicialComplex. #{1 2 3 4} #{#{} #{1} #{2} #{3}})))
  (is (not= (FullSimplicialComplex. #{} #{})
            (Object.))))

(deftest test-FullSimplicialComplex-hashCode
  (let [simplicial-complex-1 (FullSimplicialComplex. #{1 2} #{#{} #{1} #{2} #{1 2}})
        simplicial-complex-2 (FullSimplicialComplex. #{1 2} #{#{} #{1} #{2} #{1 2}})
        simplicial-complex-3 (FullSimplicialComplex. #{1 2} #{#{} #{1} #{2}})]
    (is (= (hash simplicial-complex-1) (hash simplicial-complex-1)))
    (is (= (hash simplicial-complex-1) (hash simplicial-complex-2)))
    (is (not= (hash simplicial-complex-1) (hash simplicial-complex-3)))))

(deftest test-FullSimplicialComplex-toString
  (is (= (str (FullSimplicialComplex. #{1} #{#{1}}))
         "#{#{1}}"))
  (is (= (str (FullSimplicialComplex. #{1} [#{1}]))
         "#{#{1}}")))

(deftest test-make-full-simplicial-complex-nc
  (is (= (make-full-simplicial-complex-nc #{1 2 3} [#{} #{1} #{2} #{3}])
         (FullSimplicialComplex. #{1 2 3} #{#{} #{1} #{2} #{3}})))
  (is (= (make-full-simplicial-complex-nc [#{} #{1} #{2} #{3}])
         (FullSimplicialComplex. #{1 2 3} #{#{} #{1} #{2} #{3}})))
  (is (= (make-full-simplicial-complex-nc  [#{} #{1} #{2} #{3} #{1 2} #{1 3}])
         (FullSimplicialComplex. #{1 2 3} #{#{} #{1} #{2} #{3} #{1 2} #{1 3}})))
  (is (thrown? IllegalArgumentException (make-full-simplicial-complex-nc 0))))

(deftest test-is-simplicial-complex
  (is (is-simplicial-complex? 
       (make-full-simplicial-complex-nc #{#{} #{1} #{2} #{3} #{1 2} #{1 3}})))
  (is (is-simplicial-complex? 
       (make-full-simplicial-complex-nc #{1 2 3} [#{} #{1} #{2} #{3}])))
  (is (not (is-simplicial-complex? 
            (make-full-simplicial-complex-nc #{#{} #{2} #{3} #{1 2} #{1 3}}))))
  (is (not (is-simplicial-complex? 
            (make-full-simplicial-complex-nc #{#{1} #{2} #{3} #{1 2} #{1 3}}))))
  (is (is-simplicial-complex? 
       (make-full-simplicial-complex-nc #{1 2 3} #{#{} #{1} #{2} #{3} #{1 2} #{1 3}})))
  (is (is-simplicial-complex? 
       (make-full-simplicial-complex-nc #{1 2 3 4} #{#{} #{1} #{2} #{3} #{1 2} #{1 3}})))
  (is (not (is-simplicial-complex? 
            (make-full-simplicial-complex-nc #{1 2} #{#{} #{1} #{2} #{3} #{1 2} #{1 3}})))))

(deftest test-make-full-simplicial-complex
  (is (= (make-full-simplicial-complex #{1 2 3} [#{} #{1} #{2} #{3}])
         (FullSimplicialComplex. #{1 2 3} #{#{} #{1} #{2} #{3}})))
  (is (= (make-full-simplicial-complex [#{} #{1} #{2} #{3}])
         (FullSimplicialComplex. #{1 2 3} #{#{} #{1} #{2} #{3}})))
  (is (= (make-full-simplicial-complex [#{} #{1} #{2} #{3} #{1 2} #{1 3}])
         (FullSimplicialComplex. #{1 2 3} #{#{} #{1} #{2} #{3} #{1 2} #{1 3}})))
  (is (thrown? IllegalArgumentException (make-full-simplicial-complex [#{} #{1} #{2} #{1 2} #{1 3}])))
  (is (thrown? IllegalArgumentException (make-full-simplicial-complex #{1 2} [#{} #{1} #{2} #{3} #{1 2} #{1 3}])))
  (is (thrown? IllegalArgumentException (make-full-simplicial-complex-nc 0))))

(deftest test-t-simplex
  (let [ctx (make-context-from-matrix [0 1 2 3] ['a 'b 'c 'd] 
                                      [1 1 1 1 1 0 0 0 0 1 0 1 1 0 1 1])]
    (is (= (t-simplex ctx [#{0 3} #{'a 'c 'd}])
           (FullSimplicialComplex. #{[#{0} #{'a 'b 'c 'd}] [#{0 2} #{'b 'd}]}
                                   #{#{} #{[#{0} #{'a 'b 'c 'd}]} #{[#{0 2} #{'b 'd}]}
                                     #{[#{0} #{'a 'b 'c 'd}] [#{0 2} #{'b 'd}]}})))))
