;; Copyright ⓒ the conexp-clj developers; all rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.fca.simplicial-complexes-test
  (:use clojure.test)
  (:require [conexp.fca.simplicial-complexes :refer :all])
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

