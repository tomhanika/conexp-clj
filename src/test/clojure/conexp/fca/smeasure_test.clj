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

(def- sm2
  (make-smeasure ctx1 ctx2 #(case % 1 1 4 1 2 2 3 3)))

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
         (str "  |1 4 3 2 5        |1 4 3 2 5 \n"
              "--+----------     --+----------\n"
              "1 |x . x . .  ⟶   1 |x . x . . \n"
              "--+----------     --+----------\n"
              "4 |x . x . .  ⟶   4 |x . x . . \n"
              "--+----------     --+----------\n"
              "3 |. . . . x  ⟶   3 |. . . . x \n"
              "--+----------     --+----------\n"
              "2 |. x . x .  ⟶   2 |. x . x . \n")))
  (is (= (smeasure-to-string sm2)
         (str "  |1 4 3 2 5        |1 4 3 2 5 \n"
              "--+----------     --+----------\n"
              "1 |x . x . .  ⟶   1 |x . x . . \n"
              "4 |x . x . .        |          \n"
              "--+----------     --+----------\n"
              "3 |. . . . x  ⟶   3 |. . . . x \n"
              "--+----------     --+----------\n"
              "2 |. x . x .  ⟶   2 |. x . x . \n"))))

(deftest test-original-extents
  (is (= (original-extents sm2)
         (list #{} #{2} #{3} #{1 4} #{1 4 3 2}))))

(deftest test-smeasure?
  (is (smeasure? sm1))
  (is (smeasure? sm2))
  (is (not (smeasure? sm5))))

(deftest test-make-smeasure
  (is (= (make-smeasure ctx1 ctx2 #(case % 1 1 4 1 2 2 3 3)) sm2))
  (is (thrown-with-msg?
       AssertionError
       #"The Input is no valid Scale Measure"
       (make-smeasure ctx1 ctx2 #(case % 1)))))

(deftest test-make-smeasure-nc
  (is (= (make-smeasure-nc ctx1 ctx2 #(case % 1 1 4 1 2 2 3 3)) sm2))
  (is (= (make-smeasure-nc ctx1 ctx2 #(case % 1))) no-sm))

(deftest test-make-id-smeasure
  (is (= (make-id-smeasure ctx1)
         sm1)))

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

;(deftest scale-apposition)

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

;(deftest rename-scale)
;(deftest logical-conjunctive-smeasure-representation)

nil
