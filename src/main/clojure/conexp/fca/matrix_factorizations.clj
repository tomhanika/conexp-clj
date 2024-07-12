(ns conexp.fca.matrix-factorizations
    (:require [clojure.string :as str]
              [clojure.set :as set]
              [conexp.base :refer :all]
              [conexp.fca.contexts :refer :all]
              [conexp.fca.lattices :refer :all]))


(defn context-incidence-matrix [ctx]
  "Computes a representation of the context as an incidence matrix, with the object and 
   attribute lists in order."
  (let [objs (into [] (objects ctx))
        attrs (into [] (attributes ctx))]
    [objs
     attrs
     (into [] (for [obj objs]
               (into [] (for [ attr attrs]
                          (if (incident? ctx obj attr) 1 0)))))])
)

(defn argmax [function coll]
  "Returns the value in *coll* for which (function coll) returns the highest value."
  (apply max-key function coll)
)

(defn generate-boolean-vectors [length]
  "Returns a collection of all boolean vectors of the specified length."
  (map #(into [] %)
  (map #(concat (repeat (- length (count %)) 0) %)
       (map  #(map (fn [x] (Integer/parseInt x)) (str/split % #"")) 
             (map #(Integer/toString % 2) (range (Math/pow 2 length))))))
)

(defn transpose [M]
  "Returns a transposed matrix."
  (into [] (apply map vector M)))

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

(defn col-number [M]
  "Returns the number of columns of the matrix."
  (count (first M))
)


(defn scalar-product [V1 V2]
  "Computes the scalar/dot product of two vectors"
  (reduce + (map * V1 V2))
)


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
  "Computes a new matrix from two boolean matrices of the same dimension by subtracting
   each of their entries pairwise, with 0 - 1 = 0"
  (for [r (range (row-number M1))]
    (for [c (range (col-number M1))]
      (max (- ((M1 r) c) ((M2 r) c))
           0)))
)


;;Grecond Algorithm

(defn object-concepts [ctx]
  (set (for [obj (objects ctx)] (object-concept ctx obj)))
)

(defn attribute-concepts [ctx]
  (set (for [attr (attributes ctx)] (attribute-concept ctx attr)))
)


(defn- required-factors [ctx]
  (loop [S (set (concepts ctx))
         conc (set (concepts ctx))
         U (incidence-relation ctx)
         F #{}]

    (if (empty? conc)
      
      [S U F]

      (if (.contains (set/intersection (object-concepts ctx) (attribute-concepts ctx)) (first conc))
        (recur (disj S (first conc))
               (rest conc)
               (set/difference U (set (for [g (first (first conc)) m (second (first conc))] [g m])))
               (conj F (first conc)))
        (recur S
               (rest conc)
               U
               F))))
)

(defn- remaining-factors [S U F ctx]

  (loop [S' S
         U' U
         F' F]


    (if (empty? U')
      
      F'

      (let [best-conc (argmax #(count (set/intersection (set (for [g (first %) m (second %)] [g m])) U'))
                              S')]

        (recur (disj S' best-conc)
               (set/difference U' (set (for [g (first best-conc) m (second best-conc)] [g m])))
               (conj F' best-conc)))))
)

(defn- contexts-from-factors [factors]
  (let [objects (reduce #(set/union %1 (first %2)) #{} factors)
        attributes (reduce #(set/union %1 (second %2)) #{} factors)]

    (loop [remaining-factors factors
           factor-names ["F0"]
           obj-fac-incidence #{}
           fac-attr-incidence #{}]
      (if (empty? remaining-factors) 
        [(make-context objects factor-names obj-fac-incidence) 
         (make-context factor-names attributes fac-attr-incidence)]

        (recur (rest remaining-factors)
               (conj factor-names (str "F" (count factor-names)))
               (set/union obj-fac-incidence 
                          (set (for [g (first (first remaining-factors))] [g (last factor-names)])))
               (set/union fac-attr-incidence 
                          (set (for [m (second (first remaining-factors))] [(last factor-names) m])))))))
)

(defn grecond [ctx]
  (let [[S U F] (required-factors ctx)] 
   (contexts-from-factors (remaining-factors S U F ctx)))
)



;; ASSO Algorithm
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
  (into [] (for [i (range (col-number M))]
    (into [] (for [j (range (col-number M))]
               (indicator (>= (column-association M i j) t))))))
)


(defn- cover [B S C w+ w-]
  "Computes a score of how well the boolean matrix product of S and B approximates C.
  w+ and w- are weights that determine how much correct entries are rewarded and 
  incorrect entries are penalized."

  (- (* w+ (reduce + (flatten (matrix-entrywise-product C (boolean-matrix-product S B)))))
     (* w- (reduce + (flatten (matrix-boolean-difference (boolean-matrix-product S B) C)))))
)



(defn ASSO [C k t w+ w-]
  "Algorithm that greedily solves the discrete basis problem.
   The arguments are:
   C: matrix to be decomposed
   k: number of binary basis vectors. Must be smaller than the smallest dimension of C
   t: threshold value ]0, 1]
   w+: weight
   w-: weight"
  (let [A (association-matrix C t)  ; Association Matrix
        boolean-vectors (generate-boolean-vectors (row-number C))] 
    (loop [counter 0
           B []  ; Basis Matrix
           S []] ; Usage Matrix
(println counter)

    (if (= counter k)
      [S B]

            ;cartesian product of rows of A and boolean vectors of length n:
      (let [v-combos (for [a A v boolean-vectors] [a v]) 
            ;Pair of row of A and boolean vector that maximizes *cover*:
            new-vectors (argmax #(cover (add-row B (first %)) 
                                        (add-column S (second %))
                                        C
                                        w+
                                        w-) 
                                v-combos)]
        (recur (+ counter 1)
               (add-row B (first new-vectors))
               (add-column S (second new-vectors)))))))
)
