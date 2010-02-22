;; Copyright (c) Daniel Borchmann. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.tests.fca.contexts
  (:use clojure.test
	conexp.base
	conexp.fca.contexts))

;;;

(deftest test-make-context
  (is (make-context #{1 2 3} #{1 2 3} #{[1 1] [2 2] [3 3]}))
  (is (make-context #{1 2 3} #{1 2 3} =))
  (is (make-context #{1 2 3} #{1 2 3} <))
  (is (make-context #{1 2 3} #{1 2 3} not=))
  (is (make-context [1 2 3] '(a d g) not=))
  (is (thrown? IllegalArgumentException (make-context 1 2 3))))

(def *empty-context* (make-context #{} #{} #{}))

(def *test-ctx-01* (make-context #{1 2 3 4 5} #{1 2 3 4 5}
				 #{[1 2] [1 1] [1 3]
				   [2 2] [2 3] [3 5]
				   [5 2] [5 3] [5 4]}))
(def *test-ctx-02* (make-context (set-of-range 0 30) (set-of-range 0 30) <))
(def *test-ctx-03* (make-context (set-of-range 0 50)
				 (set-of-range 0 50)
				 #(= 1 (gcd %1 %2))))
(def *test-ctx-04* (make-context #{} #{} #{}))
(def *test-ctx-05* (make-context (set-of-range 0 50)
				 (set-of-range 0 50)
				 (fn [_ _] (< 0.5 (rand)))))
(def *test-ctx-06* (make-context (set-of-range 0 10)
				 (cross-product (set-of-range 0 10)
						(set-of-range 0 10))
				 (fn [x [y z]]
				   (= x (mod (+ y z) 10)))))
(def *test-ctx-07* (make-context #{1 2 3 4}
				 #{1 2 3 4 5}
				 #{[1 1] [1 3] [1 4] [2 1] [2 2] [2 4] [3 4] [4 1] [4 2] [4 5]}))
(def *test-ctx-08* (make-context #{1 2 3 4} '#{a b c d e}
				 '#{[1 a] [1 c]
				    [2 a] [2 b] [2 c] [2 e]
				    [3 b] [3 e]
				    [4 c] [4 d] [4 e]}))

(defmacro test-for-every-test-ctx
  [var-spec test]
  `(are ~var-spec ~test
	*test-ctx-01*
	*test-ctx-02*
	*test-ctx-03*
	*test-ctx-04*
	*test-ctx-05*
	*test-ctx-06*
	*test-ctx-07*
	*test-ctx-08*))

(deftest test-Context-toString
  (is (= (str *test-ctx-01*)
	 "\n  |1 2 3 4 5 \n--+----------\n1 |x x x . . \n2 |. x x . . \n3 |. . . . x \n4 |. . . . . \n5 |. x x x . \n")))

(deftest test-Context-equals
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

(deftest test-object-derivation
  (are [ctx objs derived-attributes]
       (= (object-derivation ctx objs) derived-attributes)
       *test-ctx-01* #{1 2 5} #{2 3}))

(deftest test-attribute-derivation
  (are [ctx atts derived-objects]
       (= (attribute-derivation ctx atts) derived-objects)
       *test-ctx-01* #{2 3} #{1 2 5}))

(deftest test-concept?
  'to-be-done)

(deftest test-clarify-objects
  'to-be-done)

(deftest test-clarify-attributes
  'to-be-done)

(deftest test-clarify-context
  'to-be-done)

(deftest test-object-clarified?
  'to-be-done)

(deftest test-attribute-clarified?
  'to-be-done)

(deftest test-clarified?
  (is (not (clarified? *test-ctx-03*)))
  (is (clarified? *test-ctx-04*))
  (is (not (clarified? *test-ctx-06*))))

(deftest test-down-arrows
  'to-be-done)

(deftest test-up-arrows
  'to-be-done)

(deftest test-up-down-arrows
  'to-be-done)

(deftest test-reduce-clarified-context
  'to-be-done)

(deftest test-reduced?
  (is (not (reduced? *test-ctx-01*)))
  (is (not (reduced? *test-ctx-03*)))
  (is (reduced? *test-ctx-04*))
  (is (not (reduced? *test-ctx-06*))))

(deftest test-context-object-closure
  'to-be-done)

(deftest test-context-extents
  'to-be-done)

(deftest test-context-attribute-closure
  'to-be-done)

(deftest test-context-intents
  'to-be-done)

(deftest test-concepts
  'to-be-done)

(deftest test-dual-context
  (test-for-every-test-ctx
   [ctx] (= ctx (dual-context (dual-context ctx)))))

(deftest test-invert-context
  (test-for-every-test-ctx
   [ctx] (= ctx (invert-context (invert-context ctx)))))

(deftest test-context-union
  'to-be-done)

(deftest test-context-intersection
  'to-be-done)

(deftest test-context-composition
  'to-be-done)

(deftest test-context-apposition
  'to-be-done)

(deftest test-context-subposition
  'to-be-done)

(deftest test-context-transitive-closure
  'to-be-done)

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
       #{1 2 3 2 1 282 392 2 1 23}
       #{nil 4 '4 * -}))

(deftest test-context-sum
  'to-be-done)

(deftest test-context-product
  'to-be-done)

(deftest test-context-semiproduct
  'to-be-done)

(deftest test-context-xia-product
  'to-be-done)

(deftest test-subcontext?
  (test-for-every-test-ctx
   [ctx] (and (subcontext? ctx ctx)
	      (subcontext? *empty-context* ctx))))

(deftest test-compatible-subcontext?
  (test-for-every-test-ctx
   [ctx] (and (compatible-subcontext? ctx ctx)
	      (compatible-subcontext? *empty-context* ctx))))

(deftest test-compatible-subcontexts
  (are [ctx] (let [ctx (reduce-context ctx)]
	       (every? #(compatible-subcontext? % ctx)
		       (compatible-subcontexts ctx)))
       *test-ctx-01*
       *test-ctx-04*
       *test-ctx-06*
       *test-ctx-07*
       *test-ctx-08*)
  (is (let [some-context (make-context #{1 2 3} '#{c d e} '#{[1 c] [2 c] [2 e] [3 e]})]
	(some #(= some-context %) (compatible-subcontexts *test-ctx-08*)))))

nil
