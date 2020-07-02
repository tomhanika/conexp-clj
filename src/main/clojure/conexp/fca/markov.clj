;; Copyright ⓒ the conexp-clj developers; all rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.fca.markov
  (:require [clojure.core.reducers :as r]
            [conexp.base :refer :all]
            [conexp.fca.contexts :refer :all]))

;;; Base conversion

(defn- to-base10-frac
  "Given a base and a fraction in said base returns the number in base10."
  [number base]
  (let [sum (atom 0)]
    (doall
      (loop [remainder number
             pos-val   (/ 1 base)]
        (if (not (empty? remainder))
            (do (swap! sum + (* (first remainder) pos-val))
                (recur (drop 1 remainder) (/ pos-val base))))))
    @sum))

(defn- to-base10-int
  "Given a base and a integer in said base returns the number in base10."
  [number base]
  (let [sum (atom 0)]
    (doall
      (loop [remainder (reverse number)
             pos-val   1]
        (if (not (empty? remainder))
            (do (swap! sum + (* (first remainder) pos-val))
                (recur (drop 1 remainder) (* pos-val base))))))
    @sum))

(defn to-base10
  "Given a base and a number as vector  in said base returns the number in
   base10."
  [number base]
  (let [decimal (.indexOf number '.)]
    (if (= decimal -1)
        (to-base10-int number base)
        (+ (to-base10-int (subvec number 0 decimal) base)
           (to-base10-frac (subvec number (inc decimal)) base)))))

(defn- from-base10-frac
  "Given a fraction in base10 returns a number as vector in the new base."
  [number base precision]
  (let [result (atom [])]
    (doall
      (loop [remainder number
             pos-val   (/ 1 base)
             step      1]
        (if (> remainder 0)
            (let [divider (int (/ remainder pos-val))]
              (do (swap! result conj divider)
                  (if (< step precision)
                      (recur (- remainder (* divider pos-val))
                             (/ pos-val base)
                             (inc step))))))))
    @result))

(defn- from-base10-int
  "Given a integer in base10 returns a number as vector in the new base."
  [number base]
  (let [position (atom 1)
        result   (atom [])]
    ;; find the maximal position
    (doall 
      (loop [value base] 
        (if (< value (inc number))
            (do (swap! position inc)
                (recur (* value base))))))
    ;; fill the array
    (doall
      (loop [remainder number
             pos-val (expt base (dec @position))]
        (let [divider (min 
                        (int (/ remainder pos-val))
                        (dec base))]
          (do (swap! position dec)
              (swap! result conj divider)
              (if (> @position 0)
                  (recur (- remainder (* pos-val divider)) 
                         (/ pos-val base)))))))
    @result))

(defn from-base10
  "Given a number in base10 returns a number as vector in the new base. For 
   decimal numbers a precision may be given, the default value is 5."
  ([number base]
    (from-base10 number base 5))
  ([number base precision]
    (let [integer   (int number)
          fraction  (- number integer)
          base-int  (from-base10-int integer base)
          base-frac (from-base10-frac fraction base precision)]
      (if (> fraction 0)
          (vec (concat base-int ['.] base-frac))
          base-int))))

;; Markov Chain stability

(defn mcs-stability-approximation
  "Approximates concept stability with Markov Chains."
  [ctx concept sample-size]
  (let [counter (atom 0)]
    (doall
      (for [i (range sample-size)]
        (let [prem  (first concept)
              concl (second concept)
              limit (count concl)]
          (if (= (attribute-derivation ctx 
                                       (take (rand-int limit) 
                                             (shuffle concl)))
                 prem)
              (swap! counter inc)))))
    (/ @counter sample-size)))

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

(defn lds-stability-approximation
  "Approximates concept stability with Low-Discrepancy Sampling. The function
   takes a context and concept, the sample-size and base (both as integer). 
   An additional argument may be given to choose the sampling method. 
   The options are:
      :sobol One-dimensional Sobol Sequence (default)
      :svdc  Scrambled Van der Corput Sequence

   Function based on 'An Efficient Approximation of Concept
   Stability Using Low-Discrepancy Sampling' Ibrahim; Missaoui 2018
   https://link.springer.com/chapter/10.1007/978-3-319-91379-7_3 "
  ([ctx concept sample-size base]
    (lds-stability-approximation ctx concept sample-size base :sobol))
  ([ctx concept sample-size base sampling-method]
    (let [lds         (atom [])
          counter     (atom 0)
          intent      (second concept)
          intent-size (count intent)
          powerset    (subsets intent)]
      (doall 
        (for [i (range sample-size)]
          (swap! lds conj (case sampling-method
                              :sobol (generate-sobol i)
                              :svdc  (generate-svdc i base)
                              (generate-sobol i)))))
      (doall 
        (for [element @lds]
          (let [position (int (+ (* intent-size element) 0.5))
                subset   (nth powerset position)]
            (if (= (first concept)
                   (attribute-derivation ctx subset))
                (swap! counter inc)))))
      (/ @counter sample-size))))
