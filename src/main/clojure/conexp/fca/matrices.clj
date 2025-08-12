;; Copyright ⓒ the conexp-clj developers; all rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.fca.matrices
  "Provides the implementation of formal contexts and functions on them."
  (:require [conexp.base :refer :all]))


(defn transpose [M]
  "Returns a transposed matrix."
  (into [] (apply map vector M))
)

(defn matrix-row [M index]
  "Returns the indicated row of the matrix."
    (M index)
)

(defn add-row [M row]
  "Returs the matrix with the new row added."
  (if (and (not= (count M) 0)
           (not= (count row) (count (first M))))
    (throw (Exception. "Row does not have the correct length. "))
    (conj M row))
)

(defn set-row [M pos row]
  "Replaces the row of *M* at index *pos* with *row*."
  (assoc M pos row)
)

(defn row-number [M]
  "Returns the number of rows of the matrix."
  (count M)
)

(defn matrix-column [M index]
  "Returns the indicated column of the matrix."
  (into [] (for [row M] (row index)))
)

(defn add-column [M col]
  "Returns the matrix with the new column added."
  (if (= M []) (transpose [col])
    (if (not= (count col) (count M))
      (throw (Exception. "Column does not have the correct length."))
      (transpose (add-row (transpose M) col))))
)

(defn set-column [M pos col]
  "Replaces the column of *M* at index *pos* with *col*."
  (map #(assoc %1 pos %2) M col)
)

(defn col-number [M]
  "Returns the number of columns of the matrix."
  (count (first M))
)

(defn scalar-product [V1 V2]
  "Computes the scalar/dot product of two vectors"
  (reduce + (map * V1 V2))
)

(defn boolean-matrix-product [M1 M2]
  "Computes the product of two matrices with addition being interpreted as boolean OR."
  (transpose (for [c (range (col-number M2))]
    (for [r (range (row-number M1))]
      (min (scalar-product (matrix-column M2 c)
                           (matrix-row M1 r))
           1))))
)

(defn matrix-boolean-difference [M1 M2]
  "Computes a new matrix from two boolean matrices of the same dimension by subtracting
   each of their entries pairwise, with 0 - 1 = 0"
  (into [] (for [r (range (row-number M1))]
    (into [] (for [c (range (col-number M1))]
               (max (- ((M1 r) c) ((M2 r) c))
               0)))))
)

(defn matrix-entrywise-product [M1 M2]
  "Computes a new matrix from two matrices of the same dimension by multiplying
   each of their entries pairwise."
  (into [] (for [r (range (row-number M1))]
    (into [] (for [c (range (col-number M1))]
               (* ((M1 r) c) ((M2 r) c))))))
)

(defn outer-prod [v1 v2]
  "Computes the outer product of two vectors."
  (into [] (for [x v1]
    (into [] (for [y v2] (* x y)))))
)
