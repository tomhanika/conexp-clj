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

(def K (make-context #{0 7 1 4 6 3 2 9 5 8}
                     #{"att_0" "att_5" "att_6" "att_4" "att_1" "att_7" "att_2" "att_3"}
                     #{[2 "att_5"] [8 "att_6"] [9 "att_4"] [5 "att_3"] [0 "att_7"]
                       [7 "att_0"] [3 "att_1"] [4 "att_3"] [9 "att_5"] [3 "att_5"]
                       [0 "att_3"] [4 "att_2"] [1 "att_7"] [5 "att_4"] [4 "att_6"]
                       [5 "att_1"] [7 "att_7"] [8 "att_0"] [9 "att_1"] [8 "att_2"]
                       [0 "att_6"] [0 "att_5"] [6 "att_3"] [5 "att_7"] [1 "att_5"]
                       [3 "att_0"] [6 "att_4"] [2 "att_4"] [4 "att_5"] [6 "att_2"]
                       [9 "att_7"] [1 "att_4"] [2 "att_1"] [4 "att_7"] [0 "att_1"]
                       [1 "att_2"] [2 "att_7"] [7 "att_1"] [2 "att_0"] [9 "att_6"]
                       [6 "att_7"] [7 "att_6"] [8 "att_7"] [1 "att_3"] [8 "att_1"]
                       [7 "att_5"] [6 "att_1"] [5 "att_6"] [3 "att_6"] [3 "att_7"]}))

(deftest test-context-edge-swapping
  (is (= (objects K) (objects (randomize-context-by-edge-swapping K))))
  (is (= (attributes K) (attributes (randomize-context-by-edge-swapping K))))
  (doseq [r (range 10)]
    (is (= (count (incidence-relation K)) (count (incidence-relation (randomize-context-by-edge-swapping K)))))))


(deftest test-context-edge-rewiring
  (is (= (objects K) (objects (randomize-context-by-edge-rewiring K))))
  (is (= (attributes K) (attributes (randomize-context-by-edge-rewiring K))))
  (doseq [r (range 10)]
    (is (= (count (incidence-relation K)) (count (incidence-relation (randomize-context-by-edge-rewiring K)))))))
