;; Copyright (c) Daniel Borchmann. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.tests.fca.contexts
  (:use conexp.base
        conexp.fca.contexts)
  (:use clojure.test))

;;;

(deftest test-make-context
  (is (make-context #{1 2 3} #{1 2 3} #{[1 1] [2 2] [3 3]}))
  (is (make-context #{1 2 3} #{1 2 3} =))
  (is (make-context #{1 2 3} #{1 2 3} <))
  (is (make-context #{1 2 3} #{1 2 3} not=))
  (is (make-context [1 2 3] '(a d g) not=))
  (is (= (make-context-from-matrix 4 3
                                   [0 0 0,
                                    0 1 0
                                    1 1 0,
                                    0 0 0])
         (make-context [0 1 2 3]
                       [0 1 2]
                       #{[1 1], [2 0], [2 1]})))
  (is (thrown? IllegalArgumentException (make-context 1 2 3))))

;;;

(def empty-context (make-context #{} #{} #{}))

(def test-ctx-01 (make-context #{1 2 3 4 5} #{1 2 3 4 5}
                               #{[1 2] [1 1] [1 3]
                                 [2 2] [2 3] [3 5]
                                 [5 2] [5 3] [5 4]}))
(def test-ctx-02 (make-context (set-of-range 0 30) (set-of-range 0 30) <))
(def test-ctx-03 (make-context (set-of-range 0 50)
                               (set-of-range 0 50)
                               #(= 1 (gcd %1 %2))))
(def test-ctx-04 (make-context #{} #{} #{}))
(def test-ctx-05 (make-context (set-of-range 0 50)
                               (set-of-range 0 50)
                               (fn [_ _] (< 0.5 (rand)))))
(def test-ctx-06 (make-context (set-of-range 0 10)
                               (cross-product (set-of-range 0 10)
                                              (set-of-range 0 10))
                               (fn [x [y z]]
                                 (= x (mod (+ y z) 10)))))
(def test-ctx-07 (make-context #{1 2 3 4}
                               #{1 2 3 4 5}
                               #{[1 1] [1 3] [1 4] [2 1] [2 2] [2 4] [3 4] [4 1] [4 2] [4 5]}))
(def test-ctx-08 (make-context #{1 2 3 4} '#{a b c d e}
                               '#{[1 a] [1 c]
                                  [2 a] [2 b] [2 c] [2 e]
                                  [3 b] [3 e]
                                  [4 c] [4 d] [4 e]}))

(defvar testing-data [empty-context,
                      test-ctx-01,
                      test-ctx-02,
                      test-ctx-03,
                      test-ctx-04,
                      test-ctx-05,
                      test-ctx-06,
                      test-ctx-07,
                      test-ctx-08])

;;;

(deftest test-context?
  (with-testing-data [ctx testing-data]
    (context? ctx)))

(deftest test-context-to-string
  (is (= (context-to-string test-ctx-01 sort-by-second sort-by-second)
         (str "  |1 2 3 4 5 \n"
              "--+----------\n"
              "1 |x x x . . \n"
              "2 |. x x . . \n"
              "3 |. . . . x \n"
              "4 |. . . . . \n"
              "5 |. x x x . \n")))
  (is (= (context-to-string test-ctx-01 < >)
         (str "  |5 4 3 2 1 \n"
              "--+----------\n"
              "1 |. . x x x \n"
              "2 |. . x x . \n"
              "3 |x . . . . \n"
              "4 |. . . . . \n"
              "5 |. x x x . \n")))
  (is (= (context-to-string test-ctx-01 [1 3 5 4 2] [4 5 1 3 2])
         (str "  |4 5 1 3 2 \n"
              "--+----------\n"
              "1 |. . x x x \n"
              "3 |. x . . . \n"
              "5 |x . . x x \n"
              "4 |. . . . . \n"
              "2 |. . . x x \n")))
  (is (= (context-to-string test-ctx-01 [1 3 5 4] [4 5 1 3])
         (context-to-string test-ctx-01 [1 3 5 4 2] [4 5 1 3 2])))
  (is (= (context-to-string test-ctx-01 [0 1 3 5 4] ['a 4 5 1 3])
         (str "  |4 5 1 3 2 \n"
              "--+----------\n"
              "1 |. . x x x \n"
              "3 |. x . . . \n"
              "5 |x . . x x \n"
              "4 |. . . . . \n"
              "2 |. . . x x \n"))))

(deftest test-Formal-Context-equals
  (is (not= nil (make-context #{} #{} #{})))
  (is (not= (make-context #{} #{} #{}) nil))
  (is (not= (Object.) (make-context #{} #{} #{})))
  (are [objs atts inz] (= (make-context objs atts inz)
                          (make-context objs atts inz))
       #{1 2 3} #{'a 'b 'c} #{[1 'a] [1 'b] [3 'b] [3 'c]}
       #{} #{2} #{})
  (are [objs-1 atts-1 inzs-1 objs-2 atts-2 inzs-2]
       (not= (make-context objs-1 atts-1 inzs-1)
             (make-context objs-2 atts-2 inzs-2))
       #{1 2 3} #{1 2 3}   =  #{1 2 3} #{1 2 3} not=
       #{1 2 3} #{1 2 3 4} =  #{1 2 3} #{1 2 3} =
       #{'a 'b 'c} #{1 2}  =  #{'a 'b} #{1 2}   =))

;;; Testing context construction

(deftest test-make-context-again
  (are [thing] (context? thing)
       (make-context [1 2 3 4] [3 4 5 6] <)
       (make-context [1 2 3] '[a b c] '#{[1 a] [2 c]}))
  (are [thing] (thrown? Exception thing)
       (make-context [1 2 3 4] [3 4 5 6] [1 2 3])
       (make-context {1 2 3 4} {3 4 5 6} #{}))
  (is (= (incidence (make-context [1 2 3] [2 3 4] <=))
         #{[1 2] [1 3] [1 4] [2 2] [2 3] [2 4] [3 3] [3 4]}))
  (is (= (objects (make-context [nil 'a +] [] []))
         #{nil 'a +}))
  (is (= (attributes (make-context [1] [nil nil '+ *] [[1 nil]]))
         #{nil '+ *})))

(deftest test-make-context-from-matrix
  (is (= (incidence (make-context-from-matrix 2 2
                                              [1 1
                                               0 1]))
         #{[0 0] [0 1] [1 1]}))
  (let [ctx (make-context-from-matrix 2 2 [0 0 1 1])]
    (is (= (objects ctx) #{0 1}))
    (is (= (attributes ctx) #{0 1}))
    (is (= (incidence ctx) #{[1 0] [1 1]})))
  (let [ctx (make-context-from-matrix ['a +] 2 [0 1 0 1])]
    (is (= (objects ctx) #{'a +}))
    (is (= (attributes ctx) #{0 1}))
    (is (= (incidence ctx) #{['a 1] [+ 1]}))))

;;;

(deftest test-context-size
  (is (= [5 5 0.4]
         (context-size (make-context [0 1 2 3 4]
                                     [0 1 2 3 4]
                                     <)))))

(deftest test-rename-objects-attributes
  (is (let [ctx (make-context [0 1 2 3] [0 1 2 3] <),
            rct (rename-objects ctx inc)]
        (and (= #{1 2 3 4} (objects rct))
             (= (attributes ctx) (attributes rct))
             (= (set-of [(inc g) m] [[g m] (incidence ctx)])
                (incidence rct)))))
  (is (let [ctx (make-context [0 1 2 3] [0 1 2 3] <),
            rct (rename-attributes ctx dec)]
        (and (= #{-1 0 1 2} (attributes rct))
             (= (objects ctx) (objects rct))
             (= (set-of [g (dec m)] [[g m] (incidence ctx)])
                (incidence rct))))))

(deftest test-subcontext?
  (with-testing-data [ctx testing-data]
    (and (subcontext? ctx ctx)
         (subcontext? empty-context ctx))))

(deftest test-restrict-concept
  (is (let [ctx-1 (make-context (set-of-range 20)
                                (set-of-range 20)
                                (fn [a b] (= 1 (gcd a b)))),
            ctx-2 (make-context (set-of-range 10)
                                (set-of-range 10)
                                (incidence ctx-1))]
        (forall [[A B] (concepts ctx-1)]
          (let [[C D] (restrict-concept [A B] ctx-2)]
            (and (= C (intersection A (objects ctx-2)))
                 (= D (intersection B (attributes ctx-2)))))))))

(deftest test-object-derivation
  (are [ctx objs derived-attributes]
       (= (object-derivation ctx objs) derived-attributes)
       test-ctx-01 #{1 2 5} #{2 3}))

(deftest test-attribute-derivation
  (are [ctx atts derived-objects]
       (= (attribute-derivation ctx atts) derived-objects)
       test-ctx-01 #{2 3} #{1 2 5}))

(deftest test-concept?
  (with-testing-data [A (subsets (objects test-ctx-01))]
    (let [A-prime  (object-derivation test-ctx-01 A),
          A-pprime (attribute-derivation test-ctx-01 A-prime)]
      (concept? test-ctx-01 [A-pprime A-prime])))
  (is (not (concept? test-ctx-01 [#{} #{}])))
  (is (not (concept? test-ctx-01 [1 2]))))

(deftest test-object-clarififaction
  (with-testing-data [ctx testing-data]
    (object-clarified? (clarify-objects ctx))))

(deftest test-attribute-clarification
  (with-testing-data [ctx testing-data]
    (attribute-clarified? (clarify-attributes ctx))))

(deftest test-clarified?
  (is (not (clarified? test-ctx-03)))
  (is (clarified? test-ctx-04))
  (is (not (clarified? test-ctx-06))))

(deftest test-clarify-context
  (with-testing-data [ctx testing-data]
    (subcontext? (clarify-context ctx) ctx))
  (with-testing-data [ctx testing-data]
    (clarified? (clarify-context ctx))))

(deftest test-down-arrows
  (with-testing-data [ctx testing-data]
    (forall [[g m] (down-arrows ctx)]
      (and (not (contains? (incidence ctx) [g m]))
           (forall [h (objects ctx)]
             (=> (proper-subset? (object-derivation ctx #{g})
                                 (object-derivation ctx #{h}))
                 (contains? (incidence ctx) [h m])))))))

(deftest test-up-arrows
  (with-testing-data [ctx testing-data]
    (forall [[g m] (up-arrows ctx)]
      (and (not (contains? (incidence ctx) [g m]))
           (forall [n (attributes ctx)]
             (=> (proper-subset? (attribute-derivation ctx #{m})
                                 (attribute-derivation ctx #{n}))
                 (contains? (incidence ctx) [g n])))))))

(deftest test-up-down-arrows
  (with-testing-data [ctx testing-data]
    (= (up-down-arrows ctx)
       (intersection (up-arrows ctx)
                     (down-arrows ctx)))))

(deftest test-reduce-clarified-context
  (with-testing-data [ctx testing-data]
    (=> (clarified? ctx)
        (let [rctx (reduce-clarified-context ctx),
              arrs (up-down-arrows rctx)]
          (and (= (objects rctx) (set-of g | [g _] arrs))
               (= (attributes rctx) (set-of m | [_ m] arrs)))))))
    
(deftest test-reduce-objects
  (with-testing-data [ctx testing-data]
    (let [rctx (reduce-objects ctx),
          arrs (down-arrows rctx)]
      (= (objects rctx) (set-of g | [g _] arrs)))))

(deftest test-reduce-attributes
  (with-testing-data [ctx testing-data]
    (let [rctx (reduce-attributes ctx),
          arrs (up-arrows rctx)]
      (= (attributes rctx) (set-of m | [_ m] arrs)))))

(deftest test-object-reduced?
  (with-testing-data [ctx testing-data]
    (is (object-reduced? (reduce-objects ctx)))))

(deftest test-attribute-reduced?
  (with-testing-data [ctx testing-data]
    (is (attribute-reduced? (reduce-attributes ctx)))))

(deftest test-reduced?
  (is (not (reduced? test-ctx-01)))
  (is (not (reduced? test-ctx-03)))
  (is (reduced? test-ctx-04))
  (is (not (reduced? test-ctx-06))))

(deftest test-reduce-context
  (with-testing-data [ctx testing-data]
    (subcontext? (reduce-context ctx) ctx))
  (with-testing-data [ctx testing-data]
    (reduced? (reduce-context ctx))))

(deftest test-context-object-attribute-closure
  (is (forall [s (subsets (objects test-ctx-01))]
        (= (context-object-closure test-ctx-01 s)
           (->> s
                (object-derivation test-ctx-01)
                (attribute-derivation test-ctx-01)))))
  (is (forall [s (subsets (attributes test-ctx-08))]
        (= (context-attribute-closure test-ctx-08 s)
           (->> s
                (attribute-derivation test-ctx-08)
                (object-derivation test-ctx-08)))))
  (with-testing-data [ctx [test-ctx-01 test-ctx-02 test-ctx-04 test-ctx-08]]
    (= (set (intents ctx))
       (set (all-closed-sets (attributes ctx)
                             #(context-attribute-closure ctx %))))))

(deftest test-concepts
  (with-testing-data [ctx testing-data]
    (=> (and (<= (count (objects ctx)) 15)
             (<= (count (attributes ctx)) 15))
        (every? #(concept? ctx %) (concepts ctx)))))

(deftest test-intents
  (with-testing-data [ctx testing-data]
    (=> (and (<= (count (objects ctx)) 15)
             (<= (count (attributes ctx)) 15))
        (= (set (intents ctx))
           (set-of B [[_ B] (concepts ctx)])))))

(deftest test-extents
  (with-testing-data [ctx testing-data]
    (=> (and (<= (count (objects ctx)) 15)
             (<= (count (attributes ctx)) 15))
        (= (set (extents ctx))
           (set-of A [[A _] (concepts ctx)])))))

;;;

(deftest test-dual-context
  (with-testing-data [ctx testing-data]
    (= ctx (dual-context (dual-context ctx)))))

(deftest test-invert-context
  (with-testing-data [ctx testing-data]
    (= ctx (invert-context (invert-context ctx)))))

(deftest test-one-context
  (are [base-set] (let [ctx (one-context base-set)]
                    (and (= (cross-product base-set base-set) (incidence ctx))
                         (= base-set (objects ctx) (attributes ctx))))
       #{}
       #{1 2 3}
       #{3 2 1}
       #{'a * 3 nil}
       #{0 9 8 7 6 5 4 3 2 1}))

(deftest test-null-context
  (are [base-set] (let [ctx (null-context base-set)]
                    (and (= #{} (incidence ctx))
                         (= base-set (objects ctx) (attributes ctx))))
       #{}
       #{1 2}
       #{'a * nil + 34}))

(deftest test-diag-context
  (are [base-set] (let [ctx (diag-context base-set)]
                    (and (= (set-of [x x] [x base-set]) (incidence ctx))
                         (= base-set (objects ctx) (attributes ctx))))
       #{}
       #{1 2 3 4 5 6 7 8 9 10}
       #{'a 'b *}
       #{nil 1}))

(deftest test-adiag-context
  (are [base-set] (let [ctx (adiag-context base-set)]
                    (and (= (set-of [x y] [x base-set
                                           y base-set
                                           :when (not= x y)])
                            (incidence ctx))
                         (= base-set (objects ctx) (attributes ctx))))
       #{}
       #{1 2 3 282 392 23}
       #{nil 4 * -}))

(deftest test-context-union
  (with-testing-data [ctx-1 testing-data,
                      ctx-2 testing-data]
    (let [uctx   (context-union ctx-1 ctx-2),
          union? (fn [access]
                   (= (access uctx) (union (access ctx-1) (access ctx-2))))]
      (and (union? objects)
           (union? attributes)
           (union? incidence)))))

(deftest test-context-disjoint-union
  (with-testing-data [ctx-1 testing-data,
                      ctx-2 testing-data]
    (let [uctx   (context-disjoint-union ctx-1 ctx-2),
          union? (fn [access]
                   (= (access uctx) (disjoint-union (access ctx-1) (access ctx-2))))]
      (and (union? objects)
           (union? attributes)
           (= (incidence uctx)
              (set-of [[g i] [m j]] | [g i] (objects uctx)
                                      [m j] (attributes uctx)
                                      :when (or (contains? (incidence ctx-1) [g m])
                                                (contains? (incidence ctx-2) [g m]))))))))

(deftest test-context-intersection
  (with-testing-data [ctx-1 testing-data,
                      ctx-2 testing-data]
    (let [uctx          (context-intersection ctx-1 ctx-2),
          intersection? (fn [access]
                          (= (access uctx) (intersection (access ctx-1) (access ctx-2))))]
      (and (intersection? objects)
           (intersection? attributes)
           (intersection? incidence)))))

(deftest test-context-composition
  (is (= (context-composition (make-context-from-matrix 3 3 [1 1 0
                                                             0 1 0
                                                             0 0 0])
                              (make-context-from-matrix 3 3 [1 1 1
                                                             0 0 1
                                                             0 1 1]))
         (make-context-from-matrix 3 3 [1 1 1
                                        0 0 1
                                        0 0 0]))))

(deftest test-context-apposition
  (let [ctx-1 (make-context-from-matrix 3 3 [1 1 0
                                             0 1 1
                                             0 0 1]),
        ctx-2 (make-context-from-matrix 3 4 [0 1 1 1
                                             1 0 0 0
                                             0 1 1 0]),
        ctx-3 (make-context-from-matrix 3
                                        [[0 0] [1 0] [2 0] [0 1] [1 1] [2 1] [3 1]]
                                        [1 1 0 0 1 1 1
                                         0 1 1 1 0 0 0
                                         0 0 1 0 1 1 0])]
    (is (= (context-apposition ctx-1 ctx-2) ctx-3))
    (is (thrown? IllegalArgumentException
                 (context-apposition ctx-1 (dual-context ctx-2))))))

(deftest test-context-subposition
  (let [ctx-1 (make-context-from-matrix 3 3 [1 1 0
                                             0 1 1
                                             0 0 1]),
        ctx-2 (make-context-from-matrix 4 3 [0 1 1
                                             1 0 0
                                             0 1 1
                                             0 0 1]),
        ctx-3 (make-context-from-matrix [[0 0] [1 0] [2 0] [0 1] [1 1] [2 1] [3 1]]
                                        3
                                        [1 1 0
                                         0 1 1
                                         0 0 1
                                         0 1 1
                                         1 0 0
                                         0 1 1
                                         0 0 1])]
    (is (= (context-subposition ctx-1 ctx-2) ctx-3))
    (is (thrown? IllegalArgumentException
                 (context-subposition ctx-1 (dual-context ctx-2))))))
  
(deftest test-context-transitive-closure
  (is (= (context-transitive-closure (make-context-from-matrix 3 3 [0 1 1
                                                                    1 0 1
                                                                    1 0 0]))
         (make-context-from-matrix 3 3 [1 1 1
                                        1 1 1
                                        1 1 1])))
  (is (= (context-transitive-closure (make-context-from-matrix 3 3 [1 1 0
                                                                    0 1 0
                                                                    0 0 1]))
         (make-context-from-matrix 3 3 [1 1 0
                                        0 1 0
                                        0 0 1]))))

(deftest test-rand-context
  (is (context? (rand-context [1 2 3 4] 0.5)))
  (is (= #{1 2 3 4} (objects (rand-context [1 2 3 4] 0.5))))
  (is (= '#{a b c d} (objects (rand-context '[a b c d] 0.5))))
  (is (= '#{x y z v} (attributes (rand-context [1 2 3 4] '[x y z v] 0.6))))
  (is (thrown? IllegalArgumentException (rand-context [] 'a))))

(deftest test-random-contexts
  (with-testing-data [ctx (random-contexts 11 23)]
    (and (<= (count (objects ctx)) 23)
         (<= (count (attributes ctx)) 23))))
  
(deftest test-context-sum
  (let [ctx-1 (make-context-from-matrix 3 3 [1 1 0
                                             0 1 0
                                             0 0 1]),
        ctx-2 (make-context-from-matrix 2 2 [1 1
                                             0 1]),
        ctx-3 (make-context-from-matrix [[0 0] [1 0] [2 0] [0 1] [1 1]]
                                        [[0 0] [1 0] [2 0] [0 1] [1 1]]
                                        [1 1 0 1 1
                                         0 1 0 1 1
                                         0 0 1 1 1
                                         1 1 1 1 1
                                         1 1 1 0 1])]
    (is (= (context-sum ctx-1 ctx-2)
           ctx-3))))

(deftest test-context-product
  (let [ctx-1 (make-context-from-matrix 2 2 [1 1
                                             0 1]),
        ctx-2 (make-context-from-matrix 2 2 [0 1
                                             1 0]),
        ctx-3 (make-context-from-matrix [[0 0] [1 0] [0 1] [1 1]]
                                        [[0 0] [1 0] [0 1] [1 1]]
                                        [1 1 1 1
                                         0 1 1 1
                                         1 1 1 1
                                         1 1 0 1])]
    (is (= (context-product ctx-1 ctx-2)
           ctx-3))))

(deftest test-context-semiproduct
  (let [ctx-1 (make-context-from-matrix 2 2 [1 1
                                             0 1]),
        ctx-2 (make-context-from-matrix 2 2 [0 1
                                             1 0]),
        ctx-3 (make-context-from-matrix [[0 0] [1 0] [0 1] [1 1]]
                                        [[0 0] [1 0] [0 1] [1 1]]
                                        [1 1 0 1
                                         0 1 0 1
                                         1 1 1 0
                                         0 1 1 0])]
    (is (= (context-semiproduct ctx-1 ctx-2)
           ctx-3))))

(deftest test-context-xia-product
    (let [ctx-1 (make-context-from-matrix 2 2 [1 1
                                             0 1]),
        ctx-2 (make-context-from-matrix 2 2 [0 1
                                             1 0]),
        ctx-3 (make-context-from-matrix [[0 0] [1 0] [0 1] [1 1]]
                                        [[0 0] [1 0] [0 1] [1 1]]
                                        [0 0 1 1
                                         1 0 0 1
                                         1 1 0 0
                                         0 1 1 0])]
    (is (= (context-xia-product ctx-1 ctx-2)
           ctx-3))))

;;;

nil
