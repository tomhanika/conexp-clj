(ns conexp.tests.fca.contexts
  (:use clojure.test
	conexp.base
	conexp.fca.contexts))

(deftest test-print-context
  'to-be-done)

(deftest test-Context-equals
  'to-be-done)

(deftest test-make-context
  'to-be-done)

(def *test-ctx-01* (make-context #{1 2 3 4 5} #{1 2 3 4 5}
				 #{[1 2] [1 1] [1 3]
				   [2 2] [2 3] [3 5]
				   [5 2] [5 3] [5 4]}))

(deftest test-object-derivation
  'to-be-done)

(deftest test-attribute-derivation
  'to-be-done)

(deftest test-concept?
  'to-be-done)

(deftest test-down-arrows
  'to-be-done)

(deftest test-up-arrows
  'to-be-done)

(deftest test-up-down-arrows
  'to-be-done)

(deftest test-reduce-context
  'to-be-done)

(deftest test-reduced?
  'to-be-done)

(deftest test-transpose-context
  'to-be-done)

(deftest test-invert-context
  'to-be-done)

(deftest test-context-object-closure
  'to-be-done)

(deftest test-context-extends
  'to-be-done)

(deftest test-context-attribute-closure
  'to-be-done)

(deftest test-context-intents
  'to-be-done)

(deftest test-concepts
  'to-be-done)

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

(deftest test-context-xia-product
  'to-be-done)
