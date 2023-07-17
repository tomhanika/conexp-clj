;; Copyright ⓒ the conexp-clj developers; all rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.fca.lattices-test
  (:use conexp.base
        conexp.fca.contexts
        conexp.fca.implications
        conexp.fca.posets
        conexp.math.algebra
        conexp.fca.lattices)
  (:use clojure.test))

;;; Testing basic datastructure

(deftest test-make-lattice
  (is (make-lattice #{1 2 3 4} <=))
  (is (make-lattice (subsets #{1 2 3 4}) subset?))
  (is (make-lattice-nc [1 2 3 4] [[1 2] [3 4] [5 6]]))
  (is (thrown? IllegalArgumentException (make-lattice 1 2 3)))
  (is (thrown? IllegalArgumentException (make-lattice [1 2 3] <))))

(deftest test-Lattice-equals
  (is (= (make-lattice #{} #{}) (make-lattice #{} #{})))
  (is (= (make-lattice #{} min max) (make-lattice #{} min max)))
  (is (= (make-lattice [1 2 3 4] min max) (make-lattice [4 3 2 1] min max)))
  (is (not= (make-lattice [1 2 3 4] min max) (make-lattice [1 2 3 4] max min)))
  (is (= (make-lattice [1 2 3 4] <=) (make-lattice [1 2 3 4] min max))))

(deftest test-base-set
  (are [my-set] (= (set my-set) (base-set (make-lattice-nc my-set #{})))
       #{1 2 3 4}
       (subsets #{})
       [1 2 3 4]
       []))

(deftest test-order
  (are [my-set my-order] (let [lattice   (make-lattice-nc my-set my-order),
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

(deftest test-inf-and-sup
  (let [lat (make-lattice [1 2 3 4 5] min max)]
    (is (= 2 ((sup lat) 1 2)))
    (is (= 1 ((inf lat) 1 2)))
    (is ((order lat) 1 2)))
  (let [lat (make-lattice (subsets [1 2 3]) subset?)]
    (is (= #{1} ((inf lat) #{1 2} #{1 3})))
    (is (= #{1 2 3} ((sup lat) #{1 2} #{1 3})))
    (is ((order lat) #{1} #{1 2}))
    (is (not ((order lat) #{1 2 3} #{1 3 4})))))

;;; Testing common operations for lattices

(def- testing-data
  [(make-lattice #{} #{}),
   (make-lattice [1 2 3 4 5 6] <=),
   (make-lattice (list 6 5 4 3 2 1) min max),
   (make-lattice #{} max min),
   (make-lattice #{0 1 2 3 4} #{[0 1] [0 2] [0 3] [0 4]
                                [2 1] [3 1] [4 1] [0 0]
                                [1 1] [2 2] [3 3] [4 4]}),
   (concept-lattice (rand-context (set-of-range 7) 0.4)),
   (concept-lattice (rand-context (set-of-range 7) 0.1)),
   (concept-lattice (rand-context (set-of-range 5) 0.9)),
   (make-lattice (subsets #{1 2 3}) subset?)])

(deftest test-lattice-construction
  (let [lat-1 (make-lattice-nc [1 2 3 4 5 6] min max),
        lat-2 (make-lattice-nc [1 2 3 4 5 6] <=)]
    (is (= lat-1 lat-2))
    (is (= 2 ((sup lat-1) 1 2)))
    (is (= 1 ((inf lat-2) 1 2)))))

(deftest test-has-lattice-order?
  (with-testing-data [lattice testing-data]
    (has-lattice-order? lattice))
  (are [base-set order] (not (has-lattice-order? (make-lattice-nc base-set order)))
    #{1} #{},
    [1 2 3 4] [[1 2] [2 3] [3 4]],
    [1 2 3 4] [[1 1] [2 2] [3 3] [4 4]
               [1 2] [3 4] [1 4] [2 3]]))

(deftest test-Lattice-hashCode
  (with-testing-data [lattice-1 testing-data,
                      lattice-2 testing-data]
    (=> (= lattice-1 lattice-2)
        (= (hash lattice-1) (hash lattice-2)))))

(deftest test-dual-lattice
  (with-testing-data [lattice testing-data]
    (= lattice (dual-lattice (dual-lattice lattice))))
  (with-testing-data [lattice testing-data]
    (= (base-set lattice) (base-set (dual-lattice lattice)))))

(deftest test-distributive?
  (with-testing-data [lattice testing-data]
    (<=> (distributive? lattice)
         (forall [x (base-set lattice),
                  y (base-set lattice),
                  z (base-set lattice)]
           (and (= ((sup lattice) ((inf lattice) x y) ((inf lattice) x z))
                   ((inf lattice) x ((sup lattice) y z)))
                (= ((inf lattice) ((sup lattice) x y) ((sup lattice) x z))
                   ((sup lattice) x ((inf lattice) y z))))))))

(deftest test-modular?
  (with-testing-data [lattice testing-data]
    (<=> (modular? lattice)
         (forall [a (base-set lattice),
                  b (base-set lattice),
                  x (base-set lattice)]
           (=> ((order lattice) x b)
               (= ((sup lattice) x ((inf lattice) a b))
                  ((inf lattice) ((sup lattice) x a) b)))))))

(deftest test-lattice-one
  (with-testing-data [lattice testing-data]
    (=> (not-empty (base-set lattice))
        (let [one (lattice-one lattice)]
          (forall [x (base-set lattice)]
                  ((order lattice) x one))))))

(deftest test-lattice-zero
  (with-testing-data [lattice testing-data]
    (=> (not-empty (base-set lattice))
        (let [zero (lattice-zero lattice)]
          (forall [x (base-set lattice)]
                  ((order lattice) zero x))))))

(deftest test-lattice-upper-neighbours
  (with-testing-data [lat testing-data]
    (forall [x (base-set lat)]
      (let [xs (set (lattice-upper-neighbours lat x))]
        (forall [y (base-set lat)]
          (<=> (directly-neighboured? lat x y)
               (contains? xs y)))))))

(deftest test-lattice-lower-neighbours
  (with-testing-data [lat testing-data]
    (forall [x (base-set lat)]
      (let [xs (set (lattice-lower-neighbours lat x))]
        (forall [y (base-set lat)]
          (<=> (directly-neighboured? lat y x)
               (contains? xs y)))))))

(deftest test-lattice-atoms
  (with-testing-data [lat testing-data]
    (=> (not-empty (base-set lat))
        (= (lattice-atoms lat)
           (lattice-upper-neighbours lat (lattice-zero lat))))))

(deftest test-lattice-coatoms
  (with-testing-data [lat testing-data]
    (=> (not-empty (base-set lat))
        (= (lattice-coatoms lat)
           (lattice-lower-neighbours lat (lattice-one lat))))))

(deftest test-lattice-inf-irreducibles
  (with-testing-data [lat testing-data]
    (let [inf-irr (lattice-inf-irreducibles lat)]
      (forall [x (base-set lat)]
        (<=> (contains? inf-irr x)
             (and (not= x (lattice-one lat))
                  (forall [a (disj (base-set lat) x),
                           b (disj (base-set lat) x)]
                    (not= x ((inf lat) a b)))))))))

(deftest test-lattice-sup-irreducibles
  (with-testing-data [lat testing-data]
    (let [sup-irr (lattice-sup-irreducibles lat)]
      (forall [x (base-set lat)]
        (<=> (contains? sup-irr x)
             (and (not= x (lattice-zero lat))
                  (forall [a (disj (base-set lat) x),
                           b (disj (base-set lat) x)]
                    (not= x ((sup lat) a b)))))))))

(deftest test-lattice-doubly-irreducibles
  (with-testing-data [lat testing-data]
    (= (lattice-doubly-irreducibles lat)
       (intersection (lattice-inf-irreducibles lat)
                     (lattice-sup-irreducibles lat)))))

;;; Testing FCA for lattices

(deftest test-concept-lattice
  (let [ctx (make-context-from-matrix 7 7 [0 1 1 0 0 1 1
                                           0 0 1 0 1 0 0
                                           1 0 0 0 1 1 0
                                           1 1 1 0 0 0 1
                                           1 1 0 0 1 1 1
                                           1 1 1 0 1 0 0
                                           0 0 1 0 0 1 1])]
    (is (= (concept-lattice ctx)
           (make-lattice (concepts ctx)
                         (fn [[A _] [C _]]
                           (subset? A C)))))))

(deftest test-standard-context
  (with-testing-data [lat testing-data]
    (=> (not-empty (base-set lat))
        (let [ctx (standard-context lat)]
          (and (= (count (base-set lat))
                  (count (concepts ctx)))
               (= (objects ctx)
                  (lattice-sup-irreducibles lat))
               (= (attributes ctx)
                  (lattice-inf-irreducibles lat)))))))

;;;

(def- titanic-testing-data
  [(diag-context (set-of-range 10)),
   (one-context (set-of-range 10)),
   (make-context #{1 2 3 4 5 6 7 8 9 10}
                 '#{e p c l i}
                 '#{[1 e] [1 c]
                    [2 e] [2 c] [2 i]
                    [3 e] [3 l] [3 i]
                    [4 e] [4 l] [4 i]
                    [5 e] [5 c] [5 i]
                    [6 e] [6 c]
                    [7 p] [7 l] [7 i]
                    [8 p] [8 l] [8 i]
                    [9 p] [9 l] [9 i]
                    [10 p] [10 l]}),
   ])

(deftest test-titanic-intents
  (with-testing-data [ctx titanic-testing-data]
    (= (set (intents ctx)) (set (titanic-intents ctx)))))

(deftest test-titanic-iceberg-intent-seq
  (forall [minsupp [0.0 0.2 0.5 0.7 0.9 1.0]]
    (with-testing-data [ctx titanic-testing-data]
      (= (set (frequent-closed-itemsets ctx minsupp)) 
         (set (titanic-iceberg-intent-seq ctx minsupp)))))
  (with-testing-data [ctx titanic-testing-data]
    (= (set (titanic-intents ctx))
       (set (titanic-iceberg-intent-seq ctx 0.0)))))

(deftest test-titanic-keys
  (with-testing-data [ctx titanic-testing-data]
    (= (set-of (context-attribute-closure ctx key)
               [key (titanic-keys (attributes ctx)
                                  (supports ctx 0)
                                  1.0
                                  <=)])
       (set (titanic-intents ctx)))))

(deftest test-supports
  (with-testing-data [ctx titanic-testing-data,
                      min [0 0.2 0.4 0.7 1.0]]
    (let [minnum (* (count (objects ctx)) min),
          sp     (supports ctx min),
          supps  (sp (subsets (attributes ctx)))]
      (forall [[atts supp] supps]
        (let [card (count (attribute-derivation ctx atts))]
          (if (<= minnum card)
            (= supp card)
            (= supp -1)))))))

;;;

nil
