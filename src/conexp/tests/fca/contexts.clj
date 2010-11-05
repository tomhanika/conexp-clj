;; Copyright (c) Daniel Borchmann. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.tests.fca.contexts
  (:use	conexp.base
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

;; sort-by-first, sort-by-second

(deftest test-Formal-Context-toString
  (is (= (context-to-string test-ctx-01 sort-by-second sort-by-second)
	 (str "  |1 2 3 4 5 \n"
              "--+----------\n"
              "1 |x x x . . \n"
              "2 |. x x . . \n"
              "3 |. . . . x \n"
              "4 |. . . . . \n"
              "5 |. x x x . \n"))))

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

;; context-size, rename-{object,attribute}s

(deftest test-subcontext?
  (with-testing-data [ctx testing-data]
    (and (subcontext? ctx ctx)
         (subcontext? empty-context ctx))))

;; restrict-concept

(deftest test-object-derivation
  (are [ctx objs derived-attributes]
       (= (object-derivation ctx objs) derived-attributes)
       test-ctx-01 #{1 2 5} #{2 3}))

(deftest test-attribute-derivation
  (are [ctx atts derived-objects]
       (= (attribute-derivation ctx atts) derived-objects)
       test-ctx-01 #{2 3} #{1 2 5}))

;; concept?
;; clarify-{object,attribute}s
;; {object,attribute}-clarified?

(deftest test-clarified?
  (is (not (clarified? test-ctx-03)))
  (is (clarified? test-ctx-04))
  (is (not (clarified? test-ctx-06))))

(deftest test-clarify-context
  (with-testing-data [ctx testing-data]
    (subcontext? (clarify-context ctx) ctx))
  (with-testing-data [ctx testing-data]
    (clarified? (clarify-context ctx))))

;; down-arrows
;; up-arrows
;; up-down-arrows
;; reduce-clarified-context
;; reduce-context-{object,attribute}s

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

;; context-object-closure
;; context-attribute-closure

(deftest test-concepts
  (with-testing-data [ctx testing-data]
    (=> (and (<= (count (objects ctx)) 15)
             (<= (count (attributes ctx)) 15))
        (every? #(concept? ctx %) (concepts ctx)))))

(deftest test-context-intents
  (with-testing-data [ctx testing-data]
    (=> (and (<= (count (objects ctx)) 15)
             (<= (count (attributes ctx)) 15))
        (= (set (context-intents ctx))
           (set-of B [[_ B] (concepts ctx)])))))

(deftest test-context-extents
  (with-testing-data [ctx testing-data]
    (=> (and (<= (count (objects ctx)) 15)
             (<= (count (attributes ctx)) 15))
        (= (set (context-extents ctx))
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

;; context-{union,intersection,{comp,ap,sub}position,transitive-closure}
;; rand-context
;; random-contexts
;; context-{sum,product,semiproduct,xia-product}

;;;

nil
