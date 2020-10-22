;; Copyright ⓒ the conexp-clj developers; all rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.math.numbers
  (:require [conexp.base :refer :all]))

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

;;;

nil
