;; Copyright (c) Daniel Borchmann. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.tests.fca.context-automorphisms
  (:use conexp.base
        conexp.fca.contexts
        conexp.fca.context-automorphisms)
  (:use clojure.test))


;;;

(def testing-data (random-contexts 20 6))

;;;

(deftest context-to-graph
  (not-yet-implemented))

(deftest number-of-automorphisms
  (is (= 6 (count (context-automorphisms (adiag-context 3)))))
  (is (= 24 (count (context-automorphisms (adiag-context 4)))))
  (is (= 720 (count (context-automorphisms (adiag-context 6)))))
  (is (= 12 (count (context-automorphisms (context-sum (diag-context 3)
                                                       (diag-context 2))))))
  (is (= 1 (count (context-automorphisms (make-context-from-matrix 3 3
                                                                   [1 1 1
                                                                    0 1 1
                                                                    0 0 1]))))))

(deftest context-automorphisms-returns-context-automorphisms
  (with-testing-data [ctx testing-data]
    (forall [tau (context-automorphisms ctx)]
      (context-automorphism? ctx tau))))

(deftest test-context-automorphisms
  (not-yet-implemented))

(deftest test-isomorphic-contexts?
  (not-yet-implemented))

(deftest test-rigid?
  (with-testing-data [ctx testing-data]
    (<=> (rigid? ctx)
         (singleton? (context-automorphisms ctx)))))

;;;

(deftest test-induced-object-automorphisms
  (not-yet-implemented))

(deftest test-induced-attribute-automorphisms
  (not-yet-implemented))

;;;

nil


