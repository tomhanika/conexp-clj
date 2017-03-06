;; Copyright ⓒ the conexp-clj developers; all rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.contrib.nonsense
  "What it says."
  (:use [conexp.base :only (floor, illegal-argument)]))

;;; Doomsday Algorithm

(defn- doomsday-of-year
  "Returns the doomsday of the year given."
  [year]
  (mod (+ 2 year
          (floor (/ year 4))
          (- (floor (/ year 100)))
          (floor (/ year 400)))
       7))

(defn- leap-year?
  "Returns true iff year is a leap year."
  [year]
  (or (zero? (mod year 400))
      (and (zero? (mod year 4))
           (not (zero? (mod year 100))))))

(defn- doomsday-in-month
  "Returns number of day in month doomsday occurs on."
  [year month]
  (condp = month
    1  (if (leap-year? year) 4 3)
    2  (if (leap-year? year) 29 28)
    3  0
    4  4
    5  9
    6  6
    7  11
    8  8
    9  5
    10 10
    11 7
    12 12
    (illegal-argument month " is not a valid month.")))

(defn- number->weekday
  "Converts number to weekday."
  [n]
  ({0 "Sunday",
    1 "Monday",
    2 "Tuesday",
    3 "Wednesday",
    4 "Thursday",
    5 "Friday",
    6 "Saturday"}
   (mod n 7)))

(defn weekday-of-date
  "Returns the day of week of the given date, in Gregorian calendar."
  [year month day]
  (let [doomsday (doomsday-of-year year),
        doomsday-in-month (doomsday-in-month year month)]
    (number->weekday (+ doomsday (- day doomsday-in-month)))))

;;;

(defn prime?
  "Returns true iff n is prime with a certainty of (- 1 (/ 1 (expt 2 1000)))"
  [n]
  (let [actual-number (if (instance? BigInteger n)
                        n
                        (BigInteger. (str n)))]
    (.isProbablePrime ^java.math.BigInteger actual-number 1000)))

(defn crossfoot
  "Returns the crossfoot of n."
  [n]
  (reduce + (map #(Integer/parseInt (str %)) (str n))))

(defn factorial
  "Returns n!."
  [n]
  (reduce * (range 2 (inc n))))

;;;

nil
