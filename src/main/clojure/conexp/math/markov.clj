;; Copyright ⓒ the conexp-clj developers; all rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.math.markov
  (:require [conexp.math.numbers :refer :all]))

;;; Random number sequences

(defn generate-svdc
  "Generates the Scramble Van der Corput Sequence (vdc) value for the given
   base at 'number' position."
  [number base]
  (let [radical-num (from-base10 number base)
        new-num     (to-base10 (vec (concat [0 '.] (reverse radical-num)))
                               base)]
    (mod (+ new-num (rand)) 1)))

(defn generate-sobol
  "Generates the One-dimensional Sobol Sequence value at 'number' position."
  [number]
  (if (= number 0)
      0
      (let [base-num      (from-base10 number 2)
            base-half-num (from-base10 (int (+ (/ number 2) 0.5)) 2)
            xor-num       (vec (map 
                                 #(if (= 1 (+ %1 %2)) 1 0)
                                 base-num 
                                 (concat [0] base-half-num)))
            ;; list with length of dir nums
            dir-nums-len  (filter
                            identity
                            (map 
                              #(if (= 1 %1) (inc %2) nil) 
                              (reverse xor-num)
                              (range (count xor-num))))
            dir-nums-max  (apply max dir-nums-len)
            dir-nums      (map
                            #(vec (concat (repeat %1 1) 
                                          (repeat (- dir-nums-max %1) 0))) 
                            dir-nums-len)]
      (to-base10 
        (vec (concat
               [0 '.]
               (reduce
                 #(map (fn [a b] (if (= 1 (+ a b)) 1 0)) %1 %2) 
                 dir-nums)))
        2))))

;;;

nil
