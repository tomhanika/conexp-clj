;; Copyright ⓒ the conexp-clj developers; all rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.fca.smeasure-test
  (:use conexp.fca.contexts conexp.fca.smeasure conexp.base)
  (:use clojure.test))

(def- ctx1
  (make-context-nc #{1 2 3 4} #{1 2 3 4 5} 
                   #{[1 1][2 2][1 3][2 4][3 5][4 1][4 3]}))

(def- ctx2
  (make-context-nc #{1 2 3} #{1 2 3 4 5} 
                   #{[1 1][2 2][1 3][2 4][3 5]}))

(def- ctx3
  (make-context-nc #{1 2 3 4} #{1 2 5} 
                   #{[1 1][2 2][3 5][4 1]}))

(def- ctx4
  (make-context-nc #{1 2 3 4} #{3 5} 
                   #{[1 3][3 5][4 3]}))

(def- ctx-equality
  (make-context #{1 2 3} #{1 2 3} =))

(def- ctx-inequality
  (make-context #{1 2 3} #{1 2 3} not=))

(def- sm1
  (make-id-smeasure ctx1))

(def- sm1-str
  (str "  |1 4 3 2 5        |1 4 3 2 5 \n"
       "--+----------     --+----------\n"
       "1 |x . x . .  ⟶   1 |x . x . . \n"
       "--+----------     --+----------\n"
       "4 |x . x . .  ⟶   4 |x . x . . \n"
       "--+----------     --+----------\n"
       "3 |. . . . x  ⟶   3 |. . . . x \n"
       "--+----------     --+----------\n"
       "2 |. x . x .  ⟶   2 |. x . x . \n"))

(def- sm2
  (make-smeasure ctx1 ctx2 #(case % 1 1 4 1 2 2 3 3)))

(def- sm2-str
  (str "  |1 4 3 2 5        |1 4 3 2 5 \n"
       "--+----------     --+----------\n"
       "1 |x . x . .  ⟶   1 |x . x . . \n"
       "4 |x . x . .        |          \n"
       "--+----------     --+----------\n"
       "3 |. . . . x  ⟶   3 |. . . . x \n"
       "--+----------     --+----------\n"
       "2 |. x . x .  ⟶   2 |. x . x . \n"))

(def- sm3
  (make-smeasure ctx1 ctx3 identity))

(def- sm4
  (make-smeasure ctx1 ctx4 identity))

(def- no-sm
  (make-smeasure-nc ctx1 ctx2 #(case % 1)))

(def- sm5
  (make-smeasure-nc ctx-equality ctx-inequality identity))

(deftest test-smeasure-to-string
  (is (= (smeasure-to-string sm1)
         sm1-str))
  (is (= (smeasure-to-string sm2)
         sm2-str)))

(deftest test-pre-image-measure
  (are [sm pre-image] (= (#'conexp.fca.smeasure/pre-image-measure sm) pre-image)
    sm1 {1 #{1}, 2 #{2}, 3 #{3}, 4 #{4}}
    sm2 {1 #{1 4}, 2 #{2}, 3 #{3}}
    sm3 {1 #{1}, 2 #{2}, 3 #{3}, 4 #{4}}
    sm4 {1 #{1}, 2 #{2}, 3 #{3}, 4 #{4}}
    sm5 {1 #{1}, 2 #{2}, 3 #{3}}))

(deftest test-original-extents
  (is (= (original-extents sm2)
         (list #{} #{2} #{3} #{1 4} #{1 4 3 2}))))

(deftest test-smeasure?
  (is (smeasure? sm1))
  (is (smeasure? sm2))
  (is (not (smeasure? sm5))))

(deftest test-make-smeasure
  (is (= (smeasure-to-string (make-smeasure ctx1 ctx2 #(case % 1 1 4 1 2 2 3 3)))
         sm2-str))
  (is (thrown-with-msg?
       AssertionError
       #"The Input is no valid Scale Measure"
       (make-smeasure ctx1 ctx2 #(case % 1)))))

(deftest test-make-smeasure-nc
  (is (= (smeasure-to-string (make-smeasure-nc ctx1 ctx2 #(case % 1 1 4 1 2 2 3 3)))
         sm2-str))
  (is (= (smeasure-to-string (make-smeasure-nc ctx1 ctx2 #(case % 1))))
      (str "  |1 4 3 2 5        |1 4 3 2 5 \n"
           "--+----------     --+----------\n"
           "1 |x . x . .  ⟶   1 |x . x . . \n"
           "--+----------     --+----------\n"
           "4 |x . x . .  ⟶     |          \n"
           "--+----------     --+----------\n"
           "3 |. . . . x  ⟶     |          \n"
           "--+----------     --+----------\n"
           "2 |. x . x .  ⟶     |          \n")))

(deftest test-make-id-smeasure
  (is (= (smeasure-to-string (make-id-smeasure ctx1))
         sm1-str)))

(deftest test-smeasure-by-exts
  (let [exts #{#{} #{1}}]
    (is (= (scale (smeasure-by-exts ctx1 exts))
           (make-context (objects ctx1) exts #{[1 #{1}]})))))

(deftest test-canonical-smeasure-representation
  (is (= (scale (canonical-smeasure-representation sm2))
         (make-context (objects ctx1) 
                       #{#{} #{1 4} #{2} #{3} #{1 2 3 4}} 
                       #{[1 #{1 4}] [1 #{1 2 3 4}] 
                         [2 #{2}] [2 #{1 2 3 4}] 
                         [3 #{3}] [3 #{1 2 3 4}]
                         [4 #{1 4}] [4 #{1 2 3 4}]}))))

(deftest test-scale-apposition
  (is (= (scale (scale-apposition sm3 sm4))
         (make-context (objects ctx1)
                       #{[1 0] [2 0] [5 0] [3 1] [5 1]}
                       #{[1 [1 0]] [4 [1 0]] [2 [2 0]] [1 [3 1]] [4 [3 1]] [3 [5 0]] [3 [5 1]]})))
  (is (= (scale (scale-apposition sm2 sm4))
         (make-context (objects ctx1)
                       #{[#{} 0] [#{} 1] [#{1 4} 0] [#{1 4} 1] [#{2} 0] [#{3} 0] [#{3} 1] [#{1 2 3 4} 0] [#{1 2 3 4} 1]}
                       #{[1 [#{1 4} 0]] [1 [#{1 4} 1]] [4 [#{1 4} 0]] [4 [#{1 4} 1]] [2 [#{2} 0]] [3 [#{3} 0]] [3 [#{3} 1]] [1 [#{1 2 3 4} 0]] [1 [#{1 2 3 4} 1]] [2 [#{1 2 3 4} 0]] [2 [#{1 2 3 4} 1]] [3 [#{1 2 3 4} 0]] [3 [#{1 2 3 4} 1]] [4 [#{1 2 3 4} 0]] [4 [#{1 2 3 4} 1]]}))))

(deftest test-meet-smeasure
  (let [sm-meet-1 (meet-smeasure sm1 sm2)
        sm-meet-2 (meet-smeasure sm3 sm4)]
    (is (= (context sm-meet-1) ctx1))
    (is (= (scale sm-meet-1)
           (make-context (objects ctx1) #{#{} #{2} #{3} #{1 4} #{1 2 3 4}} 
                         #{[1 #{1 4}] [1 #{1 2 3 4}] [2 #{2}] [2 #{1 2 3 4}] [3 #{3}] [3 #{1 2 3 4}] [4 #{1 4}] [4 #{1 2 3 4}]})))
    (is (= (context sm-meet-2) ctx1))
    (is (= (scale sm-meet-2)
           (make-context (objects ctx3) #{#{} #{3} #{1 4} #{1 2 3 4}} 
                         #{[1 #{1 4}] [1 #{1 2 3 4}] [2 #{1 2 3 4}] [3 #{3}] [3 #{1 2 3 4}] [4 #{1 4}] [4 #{1 2 3 4}]})))))

(deftest test-join-complement
  (is (= (join-complement sm2)
         (make-smeasure ctx1
                        (make-context [1 2 3 4] 
                                      [#{1 2 3 4}] 
                                      (fn [o a] (contains? a o)))
                        identity)))
  (let [ctx1 (make-context-from-matrix [1 2 3] [4 5 6] [1 0 1 0 1 0 0 0 1])
        ctx2 (make-context-from-matrix [1 2] [4 5 6] [1 0 1 0 1 0])
        sm (make-smeasure ctx1 ctx2 #(case % 1 1 2 2 3 1))]
    (is (= (join-complement sm)
           (make-smeasure ctx1
                          (make-context [1 2 3] 
                                        [#{1} #{1 2 3}] 
                                        (fn [o a] (contains? a o)))
                          identity)))))

(deftest test-error-in-smeasure
  (are [sm] (= (error-in-smeasure sm) [])
    sm1 sm2 sm3 sm4)
  (is (= (error-in-smeasure sm5)
         [#{3 2} #{1 2} #{1 3}])))

(deftest test-valid-attributes
  (are [sm valid-atts] (= (set (valid-attributes sm)) (set valid-atts))
    sm1 [1 2 3 4 5]
    sm2 [1 2 3 4 5]
    sm3 [1 2 5]
    sm4 [3 5]
    sm5 []))

(deftest test-invalid-attributes
  (are [sm invalid-atts] (= (set (invalid-attributes sm)) (set invalid-atts))
    sm1 []
    sm2 []
    sm3 []
    sm4 []
    sm5 [1 2 3]))

(deftest test-conceptual-scaling-error
  (are [sm error] (= (conceptual-scaling-error sm) error)
    sm1 0
    sm2 0
    sm3 0
    sm4 0
    sm5 3)
  (are [sm error] (= (conceptual-scaling-error sm :relative true) error)
    sm1 0
    sm2 0
    sm3 0
    sm4 0
    sm5 (/ 3 8)))

(deftest test-attribute-scaling-error
  (are [sm error] (= (attribute-scaling-error sm) error)
    sm1 0
    sm2 0
    sm3 0
    sm4 0
    sm5 3)
  (are [sm error] (= (attribute-scaling-error sm :relative true) error)
    sm1 0
    sm2 0
    sm3 0
    sm4 0
    sm5 1))

(deftest test-remove-attributes 
  (let [ctx (rand-context (range 6) 0.5)
        sm (make-id-smeasure ctx)
        removed (remove-attributes-sm sm #{1 2})]
    (is (= (attributes (scale removed)) #{0 3 4 5}))
    (is (smeasure? removed))))

(deftest test-smeasure-valid-attr
  (are [sm scale-ctx] (= (scale (smeasure-valid-attr sm)) scale-ctx)
    sm1 ctx1
    sm2 ctx2
    sm3 ctx3
    sm4 ctx4)
  (is (= (scale (smeasure-valid-attr sm5))
         (make-context (objects ctx-equality) #{} identity))))

(deftest test-smeasure-invalid-attr
  (are [sm ctx] (= (scale (smeasure-invalid-attr sm))
                   (make-context (objects ctx) #{} identity))
    sm1 ctx1
    sm2 ctx2
    sm3 ctx3
    sm4 ctx4)
  (is (= (scale (smeasure-invalid-attr sm5))
         ctx-inequality)))

(deftest test-smeasure-valid-exts
  (are [sm] (= (attributes (scale (smeasure-valid-exts sm)))
               #{#{} #{2} #{3} #{1 4} #{1 2 3 4}})
    sm1
    sm2
    sm3
    sm4)
  (is (= (attributes (scale (smeasure-valid-exts sm5)))
         #{#{} #{1} #{2} #{3} #{1 2 3}})))

(deftest test-smeasure-invalid-exts
  (are [sm] (= (attributes (scale (smeasure-invalid-exts sm)))
               #{})
    sm1
    sm2
    sm3
    sm4)
  (is (= (attributes (scale (smeasure-invalid-exts sm5)))
         #{#{1 2} #{1 3} #{2 3}})))

(deftest test-rename-scale-objects
  ;; rename all objects in scale with a map
  (is (= (objects (scale (#'conexp.fca.smeasure/rename-scale :objects sm1 {1 "A" 2 "B" 3 "C" 4 "D"})))
         #{"A" "B" "C" "D"}))
  (is (= (scale (#'conexp.fca.smeasure/rename-scale :objects sm4 {1 "A" 2 "B" 3 "C" 4 "D"})))
      (make-context #{"A" "B" "C" "D"} #{3 5} #{["A" 3] ["C" 5] ["D" 3]}))
  ;; rename one object in scale
  (is (= (objects (scale (#'conexp.fca.smeasure/rename-scale :objects sm2 1 5)))
         #{5 2 3}))
  (is (= (scale (#'conexp.fca.smeasure/rename-scale :objects sm3 1 5))
         (make-context #{5 2 3 4} #{1 2 5} #{[5 1] [2 2] [3 5] [4 1]})))
  ;; rename object that does not exist
  (is (= (scale (#'conexp.fca.smeasure/rename-scale :objects sm5 4 5))
         (scale sm5))))

(deftest test-rename-scale-attributes
  ;; rename all attributes in scale with a map
  (is (= (attributes (scale (#'conexp.fca.smeasure/rename-scale :attributes sm1 {1 "A" 2 "B" 3 "C" 4 "D" 5 "E"})))
         #{"A" "B" "C" "D" "E"}))
  (is (= (scale (#'conexp.fca.smeasure/rename-scale :attributes sm2 {1 10 2 20 3 30 4 40 5 50}))
         (make-context #{1 2 3} #{10 20 30 40 50} #{[1 10] [1 30] [2 20] [2 40] [3 50]})))
  ;; rename one attribute in scale
  (is (= (attributes (scale (#'conexp.fca.smeasure/rename-scale :attributes sm3 2 "C")))
         #{1 "C" 5}))
  (is (= (scale (#'conexp.fca.smeasure/rename-scale :attributes sm4 3 1))
         (make-context #{1 2 3 4} #{1 5} #{[1 1] [3 5] [4 1]})))
  ;; rename attribute that does not exist
  (is (= (scale (#'conexp.fca.smeasure/rename-scale :attributes sm5 4 5))
         (scale sm5))))

(def- cnf-1
  (str "  |1 4 3 2 5        |() (5) (4 :and 2) (1 :and 3) (1 :and 4 :and 3 :and 2 :and 5) \n"
       "--+----------     --+-------------------------------------------------------------\n"
       "1 |x . x . .  ⟶   1 |x  .   .          x          .                               \n"
       "--+----------     --+-------------------------------------------------------------\n"
       "4 |x . x . .  ⟶   4 |x  .   .          x          .                               \n"
       "--+----------     --+-------------------------------------------------------------\n"
       "3 |. . . . x  ⟶   3 |x  x   .          .          .                               \n"
       "--+----------     --+-------------------------------------------------------------\n"
       "2 |. x . x .  ⟶   2 |x  .   x          .          .                               \n"))

(def- cnf-2
  (str "  |1 4 3 2 5        |() (5) (1 :and 3) (1 :and 4 :and 3 :and 2 :and 5) \n"
       "--+----------     --+--------------------------------------------------\n"
       "1 |x . x . .  ⟶   1 |x  .   x          .                               \n"
       "--+----------     --+--------------------------------------------------\n"
       "4 |x . x . .  ⟶   4 |x  .   x          .                               \n"
       "--+----------     --+--------------------------------------------------\n"
       "3 |. . . . x  ⟶   3 |x  x   .          .                               \n"
       "--+----------     --+--------------------------------------------------\n"
       "2 |. x . x .  ⟶   2 |x  .   .          .                               \n"))

(def- cnf-3
  (str "  |1 3 2        |(1 :and 3 :and 2) () (3) (2) (1) \n"
       "--+------     --+---------------------------------\n"
       "1 |x . .  ⟶   1 |.                 x  .   .   x   \n"
       "--+------     --+---------------------------------\n"
       "3 |. x .  ⟶   3 |.                 x  x   .   .   \n"
       "--+------     --+---------------------------------\n"
       "2 |. . x  ⟶   2 |.                 x  .   x   .   \n"))

(deftest test-conjunctive-normalform-smeasure-representation
  (are [sm cnf] (= (smeasure-to-string (conjunctive-normalform-smeasure-representation sm)) cnf)
    sm1 cnf-1
    sm2 cnf-1
    sm3 cnf-1
    sm4 cnf-2
    sm5 cnf-3))

nil
