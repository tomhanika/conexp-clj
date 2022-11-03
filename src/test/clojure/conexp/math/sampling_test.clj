;; Copyright ⓒ the conexp-clj developers; all rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.math.sampling-test
  (:use conexp.math.algebra)
  (:use conexp.fca.posets)
  (:use conexp.math.sampling)
  (:use clojure.test))

;;;

(deftest test-minimals-plus
  (is (= '([][][])
         (minimals-plus (make-poset #{} <=) 3)))
  ;; paper example
  (let [amount 10000
        input  (make-poset #{'a 'b 'c 'd} 
                           (fn [x y] 
                             (some #{[x y]} 
                                   #{['a 'a]['b 'b]['c 'c]['d 'd]
                                     ['a 'c]['a 'd]['b 'd]})))
        linex  (minimals-plus input amount)
        linmap (group-by identity linex)]
    ;; correct amount
    (is (= amount (count linex)))
    ;; all possible linear extensions should have occured at least once
    (is (= (set (keys linmap))
           #{['a 'c 'b 'd]
             ['a 'b 'c 'd]
             ['a 'b 'd 'c]
             ['b 'a 'c 'd]
             ['b 'a 'd 'c]}))
    ;; are we close to the target probabilities?
    (is (> 0.05 
           (- (/ (count (get linmap ['a 'c 'b 'd])) amount) 
              (/ 8 35))))
    (is (> 0.05 
           (- (/ (count (get linmap ['a 'b 'c 'd])) amount) 
              (/ 6 35))))
    (is (> 0.05 
           (- (/ (count (get linmap ['a 'b 'd 'c])) amount) 
              (/ 6 35))))
    (is (> 0.05 
           (- (/ (count (get linmap ['b 'a 'c 'd])) amount) 
              (/ 3 14))))
    (is (> 0.05 
           (- (/ (count (get linmap ['b 'a 'd 'c])) amount) 
              (/ 3 14))))))
