;; Copyright ⓒ the conexp-clj developers; all rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.contrib.algorithms.util
  "Utility functions for algorithms.")

;;;

(defn string-to-ints
  "Given a string of numbers and spaces, and a vector returns the
  elements of the vector whose indices are represented by the string,
  in that order."
  [vec str]
  (loop [str         str,
         current-int -1,
         ints        (transient #{})]
    (if (empty? str)
      (if (neg? current-int)
        (persistent! ints)
        (persistent! (conj! ints (nth vec current-int))))
      (let [next-char (first str)]
        (if (= \space next-char)
          (recur (rest str)
                 -1
                 (if (neg? current-int)
                   ints
                   (conj! ints (nth vec current-int))))
          (recur (rest str)
                 (let [next-int (int (Character/digit ^Character next-char 10))]
                   (long (+ (* 10 (max 0 current-int)) next-int)))
                 ints))))))

;;;

nil
