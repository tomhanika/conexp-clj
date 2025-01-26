(ns conexp.fca.non-monotonic-context-test
  (:use conexp.base
        conexp.fca.contexts
        conexp.fca.non-monotonic-contexts)
  (:use clojure.test))


(def test-objs #{1 2 3 4 5})
(def test-attrs #{1 2 3 4 5})
(def test-incidence #{[1 1] [1 2] [2 3] [2 4] [3 1] [3 3] [4 2] [4 4]})
(def test-ctx (make-context test-objs test-attrs test-incidence))

(def nctx (make-non-monotonic-context test-ctx = #{[1 1] [2 2] [3 3] [4 4] [5 5] 
                                                   [1 2] [2 3] [1 3]}))


(deftest test-context-creation

  ;non-orders as argument
  (is (thrown? IllegalArgumentException (make-non-monotonic-context test-ctx #{[1 1]} =)))
  (is (thrown? IllegalArgumentException (make-non-monotonic-context test-ctx = #{[1 1]})))

  (is (thrown? IllegalArgumentException (make-non-monotonic-context test-ctx #{[1 1] [2 2] [3 3] [4 4] [5 5] [1 2] [2 3]} =)))
  (is (thrown? IllegalArgumentException (make-non-monotonic-context test-ctx = #{[1 1] [2 2] [3 3] [4 4] [5 5] [1 2] [2 3]})))

  (is (thrown? IllegalArgumentException (make-non-monotonic-context test-ctx #{[1 1] [2 2] [3 3] [4 4] [5 5] [1 2] [2 1]} =)))
  (is (thrown? IllegalArgumentException (make-non-monotonic-context test-ctx = #{[1 1] [2 2] [3 3] [4 4] [5 5] [1 2] [2 1]})))

  ;empty orders as input
  (is (make-non-monotonic-context test-ctx #{} =))
  (is (make-non-monotonic-context test-ctx = nil))


  (is (= (make-non-monotonic-context test-ctx = =)
         (make-non-monotonic-context test-objs test-attrs test-incidence = =)))
)

(deftest test-order-conversion

  (is (= (object-order nctx) =))

  (is (= (object-order-explicit nctx) #{[2 2] [3 3] [1 1] [5 5] [4 4]}))
  (is (= (attribute-order-explicit nctx) #{[2 2] [2 3] [3 3] [1 1] [1 3] [5 5] [4 4] [1 2]}))

)

