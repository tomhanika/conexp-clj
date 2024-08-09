(ns conexp.fca.matrix-factorizations
    (:require [clojure.string :as str]
              [clojure.set :as set]
              [conexp.base :refer :all]
              [conexp.fca.contexts :refer :all]
              [conexp.fca.lattices :refer :all]
              [conexp.fca.implications :refer [support]]))


(defn interval-context [ctx lower upper]
  "Returns the context of the interval [*lower* *upper*] in the concept lattice of *ctx*."
  (make-context (first upper) (second lower) (incidence ctx)))



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
  (into [] (for [r (range (row-number M1))]
    (into [] (for [c (range (col-number M1))]
               (* ((M1 r) c) ((M2 r) c))))))
)

(defn matrix-boolean-sum [M1 M2]
  "Computes a new matrix from two boolean matrices of the same dimension by computing
   the conjunction of each entry."
  (into [] (for [r (range (row-number M1))]
    (into [] (for [c (range (col-number M1))]
               (max ((M1 r) c) ((M2 r) c))))))
)

(defn matrix-boolean-difference [M1 M2]
  "Computes a new matrix from two boolean matrices of the same dimension by subtracting
   each of their entries pairwise, with 0 - 1 = 0"
  (into [] (for [r (range (row-number M1))]
    (into [] (for [c (range (col-number M1))]
               (max (- ((M1 r) c) ((M2 r) c))
               0)))))
)

(defn matrix-xor [M1 M2]
  "Computes the entrywise xor operation on two boolean matrices."
  (into [] (for [r (range (row-number M1))]
    (into [] (for [c (range (col-number M1))]
               (if (not= ((M1 r) c) ((M2 r) c))
                    1
                    0)))))
)

(defn factor-concept-product [ctx1 ctx2]
  "Computes a context in the form of the boolean matrix product of both contexts.
   The contexts need to have appropriate dimensions and the set of attributes of *ctx1*
   must be equal to the set of objects of *ctx2*."
  (make-context (objects ctx1)
                (attributes ctx2)
                (fn [g m] (not= (set/intersection (object-derivation ctx1 #{g})
                                                  (attribute-derivation ctx2 #{m}))
                                #{})))
)

(defn- contexts-from-factors [factors objects attributes]
  "Computes contexts from set of factor concepts."

  (loop [remaining-factors factors
         factor-names ["F0"]
         obj-fac-incidence #{}
         fac-attr-incidence #{}]

    (if (empty? remaining-factors) 
      [(make-context objects (drop-last factor-names) obj-fac-incidence) 
       (make-context (drop-last factor-names) attributes fac-attr-incidence)]

      (recur (rest remaining-factors)
             (conj factor-names (str "F" (count factor-names)))
             (set/union obj-fac-incidence 
                        (set (for [g (first (first remaining-factors))] [g (last factor-names)])))
             (set/union fac-attr-incidence 
                        (set (for [m (second (first remaining-factors))] [(last factor-names) m]))))))
)




;;topFiberM Algorithm



(defn- add-fiber [X mxr]
  "Lines 12 - 22."
  (let [[obj-order attr-order incidence] (context-incidence-matric X)
        Bi (matrix-row X mxr)])

)

(defn topFiberM [ctx k precision search-limit]

  (let [sr (min search-limit (count (objects ctx)) (count (attributes ctx)))]

    (loop [As (into [] (repeat (count (objects ctx)) (into [] (repeat search-limit 0))))
           Bs (into [] (repeat search-limit (into [] (repeat (count (attributes ctx)) 0))))
           tf []
           excluded-rows [] ;objects representing excluded fibers
           excluded-cols [] ;attributes representing excluded fibers
           X ctx
           i 1]

      (if (< sr i)
        ["result"]
        (let [best-row (max-key #(count (object-derivation X #{%})) (set/difference (objects ctx) excluded-rows)) ;objects with most 1s incident
              best-col (max-key #(count (attribute-derivation X #{%})) (set/difference (attributes ctx) excluded-cols)) ;attributes with most 1s incident
              best-fiber (if (< (count (object-derivation best-row)) (count (attribute-derivation best-col))) best-col best-row)
              


]) 
)




)




)
)




;; PaNDa Algorithm

(defn outer-prod [v1 v2]
  "computes the outer product of two vectors."
  (into [] (for [x v1]
    (into [] (for [y v2] (* x y)))))
)

(defn- cost [patterns ctx]
  "computes the cost function of the given patterns.
   Compare Problem 1"
  (let [ground-truth (reduce #(matrix-boolean-sum %1 (outer-prod (first %2) (second %2))) 
                             (outer-prod (repeat (count (objects ctx)) 0) (repeat (count (attributes ctx)) 0))
                             patterns)
        noise (matrix-xor ground-truth (last (context-incidence-matrix ctx)))]

    (+ (reduce + (flatten patterns))
       (reduce + (flatten noise))))
)

(defn- find-core [residual-data patterns ctx]

  (let [obj-order (into [] (objects ctx))
        attr-order (into [] (attributes ctx))
        S (sort-by #(support #{%} ctx) (attributes residual-data))]
    (loop [extension-list []
           Ci (assoc (into [] (repeat (count attr-order) 0)) (.indexOf attr-order (first S)) 1)
           Ct (into [] (for [obj obj-order] (if (.contains (attribute-derivation residual-data #{(first S)}) obj)
                                                1
                                                0)))
           remaining (rest S)]

      (if (empty? remaining)

        [[Ct Ci] extension-list]

        (let [C*i (assoc (into [] (repeat (count attr-order) 0)) (.indexOf attr-order (first remaining)) 1)
              C*t (into [] (for [obj obj-order] (if (.contains (attribute-derivation residual-data #{(first remaining)}) obj)
                                                    1
                                                    0)))]
          (if (< (cost (conj patterns [C*t C*i]) ctx) (cost (conj patterns [Ct Ci]) ctx))
              (recur extension-list
                     C*i
                     C*t
                     (rest remaining))
              (recur (conj extension-list (first remaining))
                     Ci
                     Ct
                     (rest remaining)))))))
)

(defn- add-transactions [C C*t patterns ctx]
  "Subroutine of the extend-core function (lines 9 - 15)."
  (loop [remaining-transactions C*t
         current-best-core C]

    (if (empty? remaining-transactions)
      current-best-core

      (if (= (first remaining-transactions) 0) 

            (recur (rest remaining-transactions)
                   current-best-core)

            (let [altered-core [(first current-best-core) 
                                (assoc (second current-best-core) 1 (- (count (second current-best-core)) (count remaining-transactions)))]]

              (if (< (cost (conj patterns altered-core) ctx) (cost (conj patterns current-best-core) ctx))
                (recur (rest remaining-transactions)
                       (altered-core))
                (recur (rest remaining-transactions)
                       current-best-core)
)))))
)


(defn- extend-core [core extension-list patterns ctx];abbruchbedingung fehlt

  (let [obj-order (into [] (objects ctx))
        attr-order (into [] (attributes ctx))]

    (loop [remaining extension-list
           current-core core]

      (if (empty? remaining)
        current-core
        (let [C*t (first current-core)
              C*i (assoc (second current-core) (.indexOf attr-order (first remaining)) 1)
              current-core (if (< (cost (conj patterns [C*t C*i]) ctx) (cost (conj patterns current-core) ctx)) [C*t C*i] current-core)]

          (recur (rest remaining)
                 (add-transactions current-core C*t patterns ctx))))))
)

(defn pattern-matrices [patterns]
  "Converts collection of patterns into factor matrices."
  [(reduce #(add-column %1 (first %2)) [] patterns)
   (reduce #(add-row %1 (second %2)) [] patterns)]

)


;attributes in factors are in the order given by *context-incidence-matrix*
(defn PaNDa [ctx k]

  (let [obj-order (into [] (objects ctx))
        attr-order (into [] (attributes ctx))]
  
    (loop [patterns #{}
           residual-data ctx
           counter 1]

      (if (< k counter)
       (pattern-matrices patterns)

        (let [[core extension-list] (find-core residual-data patterns ctx)
              ecore (extend-core core extension-list patterns ctx)]

          (if (< (cost patterns ctx) (cost (conj patterns ecore) ctx))
            patterns
            (recur (conj patterns ecore)
                   (make-context (objects ctx) (attributes ctx) (filter #(or (= 0 ((first ecore) (.indexOf obj-order (first %))))
                                                                             (= 0 ((second ecore) (.indexOf attr-order (second %)))))
                                                                        (incidence-relation residual-data)))
                   (+ counter 1)))))))
)


;;Tiling Algorithm

(defn- tiling [ctx k]
  (loop [factors #{}
         counter 1
         conc (concepts ctx)]

    (if (< k counter)
      (contexts-from-factors factors (objects ctx) (attributes ctx))
      (let [max-tile (argmax #(* (count (first %)) (count (second %))) conc)
            new-factors (conj factors max-tile)]
        (recur new-factors
               (+ counter 1)
               (concepts (make-context (objects ctx) 
                                       (attributes ctx) 
                                       (set/difference (incidence ctx) (for [c new-factors
                                                                             g (first c) m 
                                                                             (second c)] [g m]))))))))
)

;;Grecond Algorithm

(defn object-concepts [ctx]
  "Returns a set of all object-concepts of the specified context."
  (set (for [obj (objects ctx)] (object-concept ctx obj)))
)

(defn attribute-concepts [ctx]
   "Returns a set of all attribute-concepts of the specified context."
  (set (for [attr (attributes ctx)] (attribute-concept ctx attr)))
)


(defn- mandatory-factors [ctx]
  "Computes the concepts that are both object-concepts and attribute-concepts 
   and removes them from the incidence relation. These concepts are required 
   for any decomposition."
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
  "Computes the decomposition factors that are not mandatory."

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

(defn grecond [ctx]
  (let [[S U F] (mandatory-factors ctx)] 
   (contexts-from-factors (remaining-factors S U F ctx) (objects ctx) (attributes ctx)))
)

;; GreEss Algorithm

(defalias o-d object-derivation)
(defalias a-d attribute-derivation)

(defn- essential-context [ctx]
  "Computes the essential part E(ctx) of the context.
   Compare Theorem 2 and Lemma 2."
  (let [objs (objects ctx)
        attrs (attributes ctx)]
    (make-context-from-matrix objs 
                              attrs
                              (for [g objs m attrs] 
                                (if (and (incident? ctx g m)
                                         (= 0 (reduce + (for [g' objs] (if (and (not= g' g)
                                                                                (not= (o-d ctx #{g'}) (o-d ctx #{g}));proper subset
                                                                                (subset? (o-d ctx #{g'}) (o-d ctx #{g}));proper subset
                                                                                (incident? ctx g' m))
                                                                           1
                                                                           0))))
                                         (= 0 (reduce + (for [m' attrs] (if (and (not= m' m)
                                                                                 (not= (a-d ctx #{m'}) (a-d ctx #{m}));proper subset
                                                                                 (subset? (a-d ctx #{m'}) (a-d ctx #{m}));proper subset
                                                                                 (incident? ctx g m'))
                                                                            1
                                                                            0)))))
                                    1
                                    0))))
)


(defn- coverage [ctx attr E D U]
  "Generates a concept from the addition of *attr* to the set *D*, and computes the intersection with the incidence relatio *U*.
  *E* is the essential context of *ctx*."
  
  (count (set/intersection (set (for [g (a-d ctx (o-d ctx (a-d E (set/union D #{attr}))))
                                      m (o-d ctx (a-d ctx (o-d E (a-d E (set/union D #{attr})))))] [g m]))
                           U))
)


(defn- best-candidate [ctx E U]
  "Line 8-12 in GreEss algorithm."
  "Computes the concept with the largest coverage in *ctx*."
  (loop [D #{}
         C (a-d E D)
         s 0]

    (if (not-any? #(> (coverage ctx % E D U) s ) (set/difference (attributes ctx) D))
      [[C D] s]

      (let [new-attr-candidate (argmax #(coverage ctx % E D U) (set/difference (attributes ctx) D))]
        (let [new-D (o-d E (a-d E (set/union D #{new-attr-candidate})))
              new-C (a-d E (set/union D #{new-attr-candidate}))
              new-s (count (set/intersection (set (for [g (a-d ctx (o-d ctx new-C)) m (o-d ctx (a-d ctx new-D))] [g m])) U))]

        (recur new-D
               new-C
               new-s)))))
)


(defn- compute-intervals [ctx]
  "Compare Algorithm 2"
  (let [E (essential-context ctx)]
    (loop [G #{}
           U (incidence-relation E)]
      (if (empty? U)

        G
        (let [best-cand (first (best-candidate ctx E U))]

          (recur (conj G best-cand)
                 (set/difference U (set (for [g (a-d ctx (o-d ctx (first best-cand)))
                                              m (o-d ctx (a-d ctx (second best-cand)))] [g m]))))))))
)


(defn- find-factor [ctx G U]
  "Line 5-18 in GreEss algorithm."
  (loop [s 0
         remaining-intervals G
         current-best-cand nil
         current-best-interval nil]

    (if (empty? remaining-intervals) 

      [current-best-cand current-best-interval]

      (let [current-conc (first remaining-intervals)
            J (make-context (objects ctx)
                            (attributes ctx)
                            (set/intersection (incidence ctx) 
                                              (into #{} (for [g (a-d ctx (second current-conc)) 
                                                              m (o-d ctx (first current-conc))] [g m]))))
            [best-cand s_cd] (best-candidate ctx J U)]

        (if (< s s_cd)
          (recur s_cd
                 (rest remaining-intervals)
                 best-cand
                 current-conc)
          (recur s
                 (rest remaining-intervals)
                 current-best-cand
                 current-best-interval)))))
)


(defn GreEss [ctx e]
  "Computes a factorization of *ctx* that is accurate within an error *e*.
  These factorizations contain only underrepresentation errors."
  (loop [G (compute-intervals ctx)
         U (incidence-relation ctx)
         factors #{}]
    (if (<=  (count U) e) 

      (contexts-from-factors factors (objects ctx) (attributes ctx))

      (let [[best-cand current-conc] (find-factor ctx G U)]

        (recur (disj G current-conc)
               (set/difference U (into #{} (for [g (first best-cand) m (second best-cand)] [g m])))
               (conj factors best-cand)))))
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




(def testctx (make-context #{1 2 3 4 5 6} #{"a" "b" "c" "d" "e"}
                           #{[1 "a"] [1 "b"] [1 "d"]
                             [2 "a"] [2 "d"] [2 "e"]
                             [3 "b"] [3 "c"]
                             [4 "d"]
                             [5 "a"] [5 "b"] [5 "c"] [5 "d"]
                             [6 "a"] [6 "b"] [6 "e"]}))


(def ctx (make-context #{1 2 3 4 5} #{1 2 3 4 5 6}
                       #{[1 1] [1 3] [1 5] [1 6]
                         [2 3]
                         [3 1] [3 2] [3 4] [3 5] [3 6]
                         [4 3] [4 6]
                         [5 2] [5 3] [5 4] [5 6]}))


(defn- compute-intervals [ctx] #{[#{1} #{"a" "b"}] [#{1 6} #{"b"}] [#{6 2} #{"e"}] [#{3} #{"c"}]
  [#{4} #{"d"}]})
