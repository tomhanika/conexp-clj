(ns conexp.fca.random-contexts-test
  (:require [conexp.fca.contexts :refer :all] 
            [conexp.fca.random-contexts :refer :all]
            ;[clojure.test :refer :all]
            )
  (:use clojure.test)
)

(def attribute-set #{"att1" "att2" "att3" "att4"})
(def object-set #{"obj1" "obj2" "obj3" "obj4"})

(deftest test-random-dirichlet-context
  (is (satisfies? Context (random-dirichlet-context :attributes attribute-set)))
  (is (satisfies? Context (random-dirichlet-context :attributes 3)))
  (is (thrown? java.lang.AssertionError (random-dirichlet-context)))
)

(deftest test-random-dirichlet-context-attribute-number
  (is (= 7  (count (attributes (random-dirichlet-context :attributes 7))))))

(deftest test-random-dirichlet-context-object-number
  (is (= 27  (count (objects (random-dirichlet-context :attributes 8 :objects 27))))))

(deftest test-random-dirichlet-context-attribute-set-object-set
  (let [rd-cxt (random-dirichlet-context :attributes attribute-set
                                         :objects object-set)]
    (is (satisfies? Context rd-cxt))
    (is (= attribute-set (attributes rd-cxt)))
    (is (= object-set (objects rd-cxt)))))


(deftest test-random-dirichlet-context-base-measure-and-precision-parameter
  (is (satisfies? Context (random-dirichlet-context :attributes 7
                                                    :objects 15
                                                    :base-measure [1 1 1 1 1 1 1 1])))
  (is (thrown? java.lang.AssertionError (random-dirichlet-context :attributes 7
                                                    :objects 15
                                                    :base-measure [1 1 1])))
  (is (satisfies? Context (random-dirichlet-context :attributes 7
                                                    :objects 15
                                                    :base-measure [1 1 1 1 1 1 1 1]
                                                    :precision-parameter (* 0.2 8))))
  (is (satisfies? Context (random-dirichlet-context :attributes 7
                                                    :objects 15
                                                    :precision-parameter (* 0.2 8)))))
