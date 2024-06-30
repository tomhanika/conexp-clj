(ns conexp.fca.matrix-factorizations
    (:require [clojure.string :as str]
              [conexp.base :refer :all]
              [conexp.fca.contexts :refer :all]))


(defn generate-boolean-vectors [length]
  "Returns a collection of all boolean vectors of the specified length."
  (map #(concat (repeat (- length (count %)) 0) %)
       (map  #(map (fn [x] (Integer/parseInt x)) (str/split % #"")) 
             (map #(Integer/toString % 2) (range (Math/pow 2 length)))))
)

(defn matrix-row [M index]
  "Returns the indicated row of the matrix."
  ( M index)
)

(defn row-number [M]
  "Returns the number of rows of the matrix."
  (count M)
)

(defn matrix-column [M index]
  "Returns the indicated column of the matrix."
  (into [] (for [row M] (row index)))
)

(defn col-number [M]
  "Returns the number of columns of the matrix."
  (count (first M))
)


(defn scalar-product [V1 V2]
  "Computes the scalar/dot product of two vectors"
  (reduce + (map * V1 V2))
)


(defn transpose [M]
  "Returns a transposed matrix."
  (into [] (apply map vector M)))


(defn matrix-product [M1 M2]
  "Computes the product of two matrices."
  (transpose (for [c (range (col-number M2))]
    (for [r (range (row-number M1))]
      (scalar-product (matrix-column M2 c)
                      (matrix-row M1 r)))))
)

(defn boolean-matrix-product [M1 M2]
  "Computes the product of two matrices with addition being interpreted as boolean OR."
  (transpose (for [c (range (col-number M2))]
    (for [r (range (row-number M1))]
      (min (scalar-product (matrix-column M2 c)
                           (matrix-row M1 r))
           1))))
)

(defn matrix-entrywise-product [M1 M2]
  "Computes a new matrix from two matrices of the same dimension by multiplying
   each of their entries pairwise."
  (for [r (range (row-number M1))]
    (for [c (range (col-number M1))]
      (* ((M1 r) c) ((M2 r) c))))
)

(defn matrix-boolean-difference [M1 M2]
  "Computes a new matrix from two matrices of the same dimension by subtracting
   each of their entries pairwise, with 0 - 1 = 0"
  (for [r (range (row-number M1))]
    (for [c (range (col-number M1))]
      (max (- ((M1 r) c) ((M2 r) c))
           0)))
)


(defn- column-association [M i j]
  "Computes the confidence of an association between columns i and j of a matrix."
  (let [dividend (scalar-product (matrix-column M i) (matrix-column M j))
        divisor (scalar-product (matrix-column M i) (matrix-column M i))]
    (if (not= divisor 0) (/ dividend divisor)
                         0))
)

(defn indicator [cond]
  "Transforms true and false into 1 and 0."
  (if cond 1 0)
)

(defn- association-matrix [M t]
  "Computes the association matrix for the ASSO algorithm."
  (for [i (range (col-number M))]
    (for [j (range (col-number M))]
      (indicator (>= (column-association M i j) t))))
)

(defn argmax [function coll]
  "retruns the value in *coll* for which (function coll) returns the highest value."
  (apply max-key function coll)
)





(defn- cover [B S C w+ w-]
  "Computes a score of how well the boolean matrix product of S and B approximates C.
  w+ and w- are weights that determine how much correct entries are rewarded and 
  incorrect entries are penalized."

  (- (* w+ (reduce + (flatten (matrix-entrywise-product C (boolean-matrix-product S B)))))
     (* w- (reduce + (flatten (matrix-boolean-difference (boolean-matrix-product S B) C)))))
)



(defn ASSO [C k t w+ w-]
  (let [A (association-matrix C t) ; Association Matrix
        B []                       ; Basis Matrix
        S []]                      ; Usage Matrix
    (for [l (range k)]
      (let [best-basis-vector (argmax #())])



)



)


)


(map #(take 8 (concat % (repeat 0))) (map #(str/split % #"") (map #(Integer/toString % 2) (range 10))))

(map #(take 8 (concat % (repeat 0))) (map (fn [x] (Integer/parseInt x)) (str/split % #"") (map #(Integer/toString % 2) (range 10))))


 (map #(map (fn [x] (Integer/parseInt x)) (str/split % #"")) (map #(Integer/toString % 2) (range 10)))


(map #(take 8 (concat % (repeat 0))) (map  #(map (fn [x] (Integer/parseInt x)) (str/split % #"")) (map #(Integer/toString % 2) (range 10))))
((0 0 0 0 0 0 0 0)
