;; Copyright (c) Daniel Borchmann. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.tests.fca.many-valued-contexts
  (:use conexp.base
        conexp.fca.contexts
        conexp.fca.many-valued-contexts)
  (:use clojure.test))

;;;

(deftest test-make-mv-context
  (is (make-mv-context #{1 2 3 4} #{1 2 3 4} +))
  (is (make-mv-context #{} #{} (constantly true)))
  (is (make-mv-context [1 2 3] [4 5 6] /))
  (is (thrown? IllegalArgumentException (make-mv-context 1 2 3)))
  (is (make-mv-context [1] '#{a} (constantly false)))
  (is (= {[1 1] 2, [1 2] 3, [2 1] 3, [2 2] 4}
         (incidence (make-mv-context [1 2] [1 2]
                                     {[1 1] 2, [1 2] 3, [2 1] 3, [2 2] 4}))))
  (is (thrown? IllegalArgumentException
               (make-mv-context [] [] {[1 1] 2}))))

(deftest test-make-mv-context-from-matrix
  (is (let [mv-ctx (make-mv-context-from-matrix 2 2 [1 2 nil 3])]
        (is (= #{0 1} (objects mv-ctx)))
        (is (= #{0 1} (attributes mv-ctx)))
        (is (= {[0 0] 1, [0 1] 2, [1 0] nil, [1 1] 3}
               (incidence mv-ctx)))))
  (is (= #{'a 'b} (objects (make-mv-context-from-matrix '[a b] [] []))))
  (is (= #{nil =} (attributes (make-mv-context-from-matrix [] [nil =] [])))))

(deftest test-Many-Valued-Context-equals
  (is (= (make-mv-context [] [] (constantly true))
         (make-mv-context [] [] (constantly true))))
  (is (= (make-mv-context [1 2 3 4] [1 2 3 4] +)
         (make-mv-context [4 3 2 1] [3 2 4 1] +)))
  (is (not= (make-mv-context [1 2 3 4] [1 2 3 4] -)
            (make-mv-context [1 2 3 4] [1 2 3 4] +)))
  (is (= (make-mv-context [1] [1] +)
         (make-mv-context [1] [1] [[1 1 2] [1 2 3] [2 3 4]]))))

(defvar- testing-data
  [(make-mv-context #{} #{} +),
   (make-mv-context (range 100) (range 100) *),
   (make-mv-context (range 100) (range 100) (fn [& _] (rand)))])

(deftest test-Many-Valued-Context-hashCode
  (with-testing-data [mv-ctx-1 testing-data,
                      mv-ctx-2 testing-data]
    (=> (= mv-ctx-1 mv-ctx-2)
        (= (hash mv-ctx-1) (hash mv-ctx-2)))))

;;;

(deftest test-scale-context
  )

;;;

nil
