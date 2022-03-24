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

(def- sm1
  (make-id-smeasure ctx1))

(def- sm2
  (make-smeasure ctx1 ctx2 #(case % 1 1 4 1 2 2 3 3)))

(def- no-sm
  (make-smeasure-nc ctx1 ctx2 #(case % 1)))

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

(deftest test-remove-attributes 
  (let [ctx (rand-context (range 6) 0.5)
        sm (make-id-smeasure ctx)
        removed (remove-attributes-sm sm #{1 2})]
    (is (= (attributes (scale removed)) #{0 3 4 5}))
    (is (smeasure? removed))))

;(deftest rename-scale)
;(deftest logical-conjunctive-smeasure-representation)
;(deftest scale-apposition)
;(deftest remove-attributes sm)

(deftest test-smeasure?
  (is (smeasure? sm1))
  (is (smeasure? sm2))
  (is (not (smeasure? no-sm))))
