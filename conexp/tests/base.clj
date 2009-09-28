(ns conexp.tests.base
  (:use clojure.test
	conexp.base))

(deftest test-cross-product
  (is (= #{[1 1] [1 2] [2 1] [2 2]} (cross-product #{1 2} #{1 2}))
      "Testing cross-product.")
  (is (and (= #{} (cross-product #{} #{1 2}))
	   (= #{} (cross-product #{1 2} #{}))
	   (= #{} (cross-product #{} #{})))
      "Testing cross-product with empty set.")
  (is (= 12 (count (cross-product #{1 2 3} #{1 2 3 4})))
      "Testing cardinality of cross-product."))

(deftest test-subelts
  (is (= (seq #{1 2 3 4}) (subelts #{1 2 3 4 5 6 7 8 9 10} 5))
      "Testing subelts with mixed arguments.")
  (is (empty? (subelts #{1 2 3 4} 1))
      "Testing subelts with mixed arguments.")
  (is (= (seq #{1 2 3 4}) (subelts #{1 2 3 4} 5))
      "Testing subelts with non-contained index.")
  (is (= (seq [5 4 2 7 8]) (subelts [5 4 2 7 8 9 2 3] 9))
      "Testing subelts with vector."))

(deftest test-lectic-<_i
  (is (lectic-<_i [5 7 3 2 1] 2 #{5 3 1} #{5 3 2 1}))
  (is (lectic-<_i [5 7 3 2 1] 5 #{3} #{5 7}))
  (is (lectic-<_i [5 7 3 2 1] 3 #{2} #{3}))
  (is (lectic-<_i [5 7 3 2 1] 1 #{} #{1})
      "Testing lectic-<_i with empty word.")
  (is (not (lectic-<_i [5 7 3 2 1] 5 #{5 3 2} #{5 7 3 2})))
  (is (not (lectic-<_i [5 7 3 2 1] 2 #{5 7 3} #{5 7 2}))))

(deftest test-lectic-<
  (is (lectic-< [5 7 3 2 1] #{} #{5}))
  (is (lectic-< [5 7 3 2 1] #{7 2 1} #{5}))
  (is (not (lectic-< [5 7 3 2 1] #{5 7 3 2 1} #{7 3 2 1}))))

(deftest test-oplus
  (let [clop #(conj % 1)]
    (is (= (oplus [5 7 3 2 1] clop #{1 2 3} 3)
	   #{1 3}))
    (is (= (oplus [5 7 3 2 1] clop #{} 1)
	   #{1}))
    (is (= (oplus [5 7 3 2 1] clop #{7 3} 3)
	   #{7 3 1})))
  (let [clop identity]
    (is (= (oplus [5 7 3 2 1] clop #{1 2 3} 3)
	   #{3}))
    (is (= (oplus [5 7 3 2 1] clop #{} 5)
	   #{5}))))

(deftest test-next-closed-set
  'to-be-done)

(deftest test-all-closed-sets
  (is (= (all-closed-sets [5 7 3 2 1] #(union % #{3 2 1}))
	 (seq [#{3 2 1} #{7 3 2 1} #{5 3 2 1} #{5 7 3 2 1}])))
  (is (= (all-closed-sets [3 2 1] identity)
	 (seq [#{} #{1} #{2} #{2 1} #{3} #{3 1} #{3 2} #{3 2 1}]))))