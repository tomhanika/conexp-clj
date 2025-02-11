(ns conexp.fca.non-monotonic-context-test
  (:use conexp.base
        conexp.fca.contexts
        conexp.fca.non-monotonic-contexts)
  (:use clojure.test))


(def test-objs #{1 2 3 4 5})
(def test-attrs #{1 2 3 4 5})
(def test-incidence #{[1 1] [1 2] [2 3] [2 4] [3 1] [3 3] [4 2] [4 4]})
(def test-ctx (make-context test-objs test-attrs test-incidence))

(def ectx (make-extended-context test-ctx = 
                                          #{[1 1] [2 2] [3 3] [4 4] [5 5] 
                                            [1 2] [2 3] [1 3]}))


(deftest test-context-creation

  ;non-orders as argument
  (is (thrown? IllegalArgumentException (make-extended-context test-ctx #{[1 1]} =)))
  (is (thrown? IllegalArgumentException (make-extended-context test-ctx = #{[1 1]})))

  (is (thrown? IllegalArgumentException (make-extended-context test-ctx #{[1 1] [2 2] [3 3] [4 4] [5 5] [1 2] [2 3]} =)))
  (is (thrown? IllegalArgumentException (make-extended-context test-ctx = #{[1 1] [2 2] [3 3] [4 4] [5 5] [1 2] [2 3]})))

  (is (thrown? IllegalArgumentException (make-extended-context test-ctx #{[1 1] [2 2] [3 3] [4 4] [5 5] [1 2] [2 1]} =)))
  (is (thrown? IllegalArgumentException (make-extended-context test-ctx = #{[1 1] [2 2] [3 3] [4 4] [5 5] [1 2] [2 1]})))

  ;empty orders as input
  (is (make-extended-context test-ctx #{} =))
  (is (make-extended-context test-ctx = nil))


  (is (= (make-extended-context test-ctx = =)
         (make-extended-context test-objs test-attrs test-incidence = =)))
)

(deftest test-order-conversion

  (is (= (object-order ectx) =))

  (is (= (object-order-explicit ectx) #{[2 2] [3 3] [1 1] [5 5] [4 4]}))
  (is (= (attribute-order-explicit ectx) #{[2 2] [2 3] [3 3] [1 1] [1 3] [5 5] [4 4] [1 2]}))

)

(deftest minimized-derivation-test

  (is (= (minimized-object-derivation ectx #{}) #{1 4 5}))
  (is (= (minimized-object-derivation ectx #{1}) #{1}))
  (is (= (minimized-object-derivation ectx #{2}) #{3 4}))

  (is (= (minimized-attribute-derivation ectx #{}) #{1 2 3 4 5}))
  (is (= (minimized-attribute-derivation ectx #{1 2}) #{1}))

)

(deftest test-conditional

  (is (respects? ectx #{1 2} #{3 4}))
  (is (respects? ectx #{3 4} #{1 2 5}))
  (is (not (respects? ectx #{1} #{2})))
)
