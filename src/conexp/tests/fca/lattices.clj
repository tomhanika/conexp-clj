;; Copyright (c) Daniel Borchmann. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.tests.fca.lattices
  (:use conexp.base
        conexp.fca.contexts
        conexp.fca.lattices)
  (:use clojure.test))

;;; Testing basic datastructure

(deftest test-make-lattice
  (is (make-lattice #{1 2 3 4} <=))
  (is (make-lattice (subsets #{1 2 3 4}) subset?))
  (is (make-lattice [1 2 3 4] [[1 2] [3 4] [5 6]]))
  (is (thrown? IllegalArgumentException (make-lattice 1 2 3))))

(deftest test-Lattice-equals
  (is (= (make-lattice #{} #{}) (make-lattice #{} #{})))
  (is (= (make-lattice #{} min max) (make-lattice #{} min max)))
  (is (= (make-lattice [1 2 3 4] min max) (make-lattice [4 3 2 1] min max)))
  (is (not= (make-lattice [1 2 3 4] min max) (make-lattice [1 2 3 4] max min)))
  (is (= (make-lattice [1 2 3 4] <=) (make-lattice [1 2 3 4] min max))))

(deftest test-base-set
  (are [my-set] (= (set my-set) (base-set (make-lattice my-set #{})))
       #{1 2 3 4}
       (subsets #{})
       [1 2 3 4]
       []))

(deftest test-order
  (are [my-set my-order] (let [lattice (make-lattice my-set my-order),
                               our-order (order lattice)]
                           (forall [x (base-set lattice),
                                    y (base-set lattice)]
                             (and (<=> (my-order x y)
                                       (our-order [x y]))
                                  (<=> (my-order x y)
                                       (our-order x y)))))
       #{1 2 3 4} <=,
       #{1 2 3 4} >=,
       #{1 2 3 4} =,
       (subsets #{1 2 3 4}) subset?))

;;; Testing common operations for lattices

(defvar- *testing-data*
  [(make-lattice #{} #{}),
   (make-lattice [1 2 3 4 5 6] <=),
   (make-lattice (list 6 5 4 3 2 1) min max),
   (make-lattice #{} max min),
   (make-lattice #{0 1 2 3 4} #{[0 1] [0 2] [0 3] [0 4]
                                [2 1] [3 1] [4 1] [0 0]
                                [1 1] [2 2] [3 3] [4 4]}),
   (concept-lattice (rand-context (set-of-range 7) 0.4)),
   (concept-lattice (rand-context (set-of-range 7) 0.1)),
   (concept-lattice (rand-context (set-of-range 5) 0.9))])

(deftest test-has-lattice-order?
  (with-testing-data [lattice *testing-data*]
    (has-lattice-order? lattice))
  (are [base-set order] (not (has-lattice-order? (make-lattice base-set order)))
    #{1} #{},
    [1 2 3 4] [[1 2] [2 3] [3 4]],
    [1 2 3 4] [[1 1] [2 2] [3 3] [4 4]
               [1 2] [3 4] [1 4] [2 3]]))

(deftest test-Lattice-hashCode
  (with-testing-data [lattice-1 *testing-data*,
                      lattice-2 *testing-data*]
    (=> (= lattice-1 lattice-2)
        (= (hash lattice-1) (hash lattice-2)))))

(deftest test-dual-lattice
  (with-testing-data [lattice *testing-data*]
    (= lattice (dual-lattice (dual-lattice lattice))))
  (with-testing-data [lattice *testing-data*]
    (= (base-set lattice) (base-set (dual-lattice lattice)))))

(deftest test-distributive?
  (with-testing-data [lattice *testing-data*]
    (<=> (distributive? lattice)
         (forall [x (base-set lattice),
                  y (base-set lattice),
                  z (base-set lattice)]
           (and (= ((sup lattice) ((inf lattice) x y) ((inf lattice) x z))
                   ((inf lattice) x ((sup lattice) y z)))
                (= ((inf lattice) ((sup lattice) x y) ((sup lattice) x z))
                   ((sup lattice) x ((inf lattice) y z))))))))

(deftest test-modular?
  (with-testing-data [lattice *testing-data*]
    (<=> (modular? lattice)
         (forall [a (base-set lattice),
                  b (base-set lattice),
                  x (base-set lattice)]
           (=> ((order lattice) x b)
               (= ((sup lattice) x ((inf lattice) a b))
                  ((inf lattice) ((sup lattice) x a) b)))))))

(deftest test-lattice-one
  (with-testing-data [lattice *testing-data*]
    (let [one (lattice-one lattice)]
      (forall [x (base-set lattice)]
        ((order lattice) x one)))))

(deftest test-lattice-zero
  (with-testing-data [lattice *testing-data*]
    (let [zero (lattice-zero lattice)]
      (forall [x (base-set lattice)]
        ((order lattice) zero x)))))

(deftest test-directly-neighboured?
  (with-testing-data [lattice *testing-data*]
    (forall [x (base-set lattice),
             y (base-set lattice)]
      (<=> (directly-neighboured? lattice x y)
           (and (not= x y)
                ((order lattice) x y)
                (forall [z (base-set lattice)]
                  (=> (and ((order lattice) x z)
                           ((order lattice) z y))
                      (or (= x z) (= y z)))))))))

;;; Testing FCA for lattices

;;;

nil
