(ns conexp.tests.fca.contexts
  (:use clojure.test
	conexp.base
	conexp.fca.contexts))

(deftest test-make-context
  'to-be-done)

(def *test-ctx-01* (make-context #{1 2 3 4 5} #{1 2 3 4 5}
				 #{[1 2] [1 1] [1 3]
				   [2 2] [2 3] [3 5]
				   [5 2] [5 3] [5 4]}))
(def *test-ctx-02* (make-context (set-of-range 0 30) (set-of-range 0 30) <))
(def *test-ctx-03* (make-context (set-of-range 0 50) (set-of-range 0 50) #(= 1 (gcd %1 %2))))
(def *test-ctx-04* (make-context #{} #{} #{}))
(def *test-ctx-05* (make-context (set-of-range 0 50) (set-of-range 0 50) (fn [_ _] (< 0.5 (rand)))))

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
  (is (not (reduced? *test-ctx-01*))))

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

(deftest test-context-product
  'to-be-done)

(deftest test-context-semiproduct
  'to-be-done)
