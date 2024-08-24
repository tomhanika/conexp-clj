(ns conexp.fca.matrix-factorizations
    (:require [clojure.string :as str]
              [clojure.set :as set]
              [conexp.base :refer :all]
              [conexp.fca.contexts :refer :all]
              [conexp.fca.lattices :refer :all]
              [conexp.fca.implications :refer [support frequent-itemsets]]))

(defn factor-context-product [ctx1 ctx2]
  "Computes a context in the form of the boolean matrix product of both contexts.
   The contexts need to have appropriate dimensions and the set of attributes of *ctx1*
   must be equal to the set of objects of *ctx2*."
  (make-context (objects ctx1)
                (attributes ctx2)
                (fn [g m] (not= (set/intersection (object-derivation ctx1 #{g})
                                                  (attribute-derivation ctx2 #{m}))
                                #{})))
)

(defprotocol context-factorization-record
  (object-factor-context [this] "Returns the formal context representing the object-factor relation.")
  (factor-attribute-context [this] "Returns the formal context representing the factor-attribute relation.")
  (context [this] "Returns the boolean matrix product of the above contexts."))

(defrecord context-factorization [obj-fac-ctx fac-attr-ctx]
  context-factorization-record
  (object-factor-context [this] obj-fac-ctx)
  (factor-attribute-context [this] fac-attr-ctx)
  (context [this] (factor-context-product obj-fac-ctx fac-attr-ctx))
)


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

(defn factor-context-product [ctx1 ctx2]
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


;;Hyper Algorithm


(defn- hyperrectangle-candidates [ctx min-supp]
  "Computes set all all candidate hyperrectangles. (Compare Section 3.2)"
  (let [freq-items (disj (into #{} (frequent-itemsets ctx min-supp)) #{}) ;frequent itemsets without #{}
        itemsets (set/union (into #{} freq-items) 
                            (into #{} (for [attr (attributes ctx)] #{attr})))]

    (for [I itemsets] [(attribute-derivation ctx I) I]))
)

(defn- price [H R]
  "Computes the price of a hyperrectangle. (Compare Theorem 5.)"
  (if (empty? (set/difference (into #{} (for [obj (first H) attr (second H)] [obj attr]))
                              (incidence-relation R)))

  Double/POSITIVE_INFINITY

    (/ (+ (count (first H)) (count (second H)))
       (count (set/difference (into #{} (for [obj (first H) attr (second H)] [obj attr]))
                              (incidence-relation R)))))
)


(defn- find-hyper [H R]
  "Compare Algorithm 2."
  (let [S (into #{} (for [obj (first H)] [#{obj} (second H)])) ;single transaction hyperrectangles in H
        U (sort-by #(count (set/difference (into #{} (for [obj (first %) attr (second %)] [obj attr])) 
                                           (incidence-relation R))) S)]
    (loop [H' (first U)
           remaining (rest U)]

      (if (empty? remaining)
     
        H'
        ;if adding next entry in U increases the price of H':
        (if (< (price H' R) (price [(conj (first H') (first (first remaining))) (second H')] R))

          H'
      
          (recur [(set/union (first H') (first (first remaining))) (second H')]
                 (rest remaining))))))
)


(defn hyper [ctx min-supp]
  (let [C- (hyperrectangle-candidates ctx min-supp)]
    (loop [R (make-context (objects ctx) (attributes ctx) #{})
           CDB #{}]

      (if (= R ctx)

      (apply ->context-factorization (contexts-from-factors CDB (objects ctx) (attributes ctx)))

      (let [hypers (for [c C-] (find-hyper c R))
            H' (apply min-key #(price % R) hypers)]

        (recur (make-context (objects ctx) 
                             (attributes ctx) 
                             (set/union (incidence-relation R) 
                                        (into #{} (for [obj (first H') attr (second H')] [obj attr]))))
               (conj CDB H'))))))
)


;;topFiberM Algorithm

(defn- process-object-fiber [X ctx obj tP] ;obj = object representing row Bi
  "Lines 12 - 22. If best fiber is a row.
  Ai and Bi are stored as sets of objects/attributes instead of boolean vectors."
  (let [[obj-order attr-order incidence] (context-incidence-matrix X)
        Bi  (object-derivation X #{obj})
        rtp (map #(count (set/intersection (object-derivation X #{%}) Bi)) (objects X))
        rfp (map #(count (set/intersection (object-derivation (invert-context ctx) #{%}) Bi)) (objects X))
        
        Ai-bool (into [] (map #(and (< 0 (+ %1 %2)) (<= tP (/ %1 (+ %1 %2)))) rtp rfp))
        Ai (into #{} (filter #(Ai-bool (.indexOf obj-order %)) obj-order))
        ctp (map #(count (set/intersection (attribute-derivation X #{%}) Ai)) (attributes X))
        cfp (map #(count (set/intersection (attribute-derivation (invert-context ctx) #{%}) Ai)) (attributes X))
        Bi-bool (into [] (map #(and (< 0 (+ %1 %2)) (<= tP (/ %1 (+ %1 %2)))) ctp cfp))
        Bi (into #{} (filter #(Bi-bool (.indexOf attr-order %)) attr-order))]

    [Ai Bi])
)

(defn- process-attribute-fiber [X ctx attr tP]
  "Lines 12 - 22. If best fiber is a column.
  Ai and Bi are stored as sets of objects/attributes instead of boolean vectors."
  (let [[obj-order attr-order incidence] (context-incidence-matrix X)
        Ai  (attribute-derivation X #{attr})
        ctp (map #(count (set/intersection (attribute-derivation X #{%}) Ai)) (attributes X))
        cfp (map #(count (set/intersection (attribute-derivation (invert-context ctx) #{%}) Ai)) (attributes X))
        
        Bi-bool (into [] (map #(and (< 0 (+ %1 %2)) (<= tP (/ %1 (+ %1 %2)))) ctp cfp))
        Bi (into #{} (filter #(Bi-bool (.indexOf attr-order %)) attr-order))
        rtp (map #(count (set/intersection (object-derivation X #{%}) Bi)) (objects X))
        rfp (map #(count (set/intersection (object-derivation (invert-context ctx) #{%}) Bi)) (objects X))
        Ai-bool (into [] (map #(and (< 0 (+ %1 %2)) (<= tP (/ %1 (+ %1 %2)))) rtp rfp))
        Ai (into #{} (filter #(Ai-bool (.indexOf obj-order %)) obj-order))]

    [Ai Bi])
)

(defn- fiber-contexts [ctx As Bs tf]
  "Computes factor contexts from list of fibers A and B."

  (loop [remaining-fibers tf
         A-incidence #{}
         B-incidence #{}
         factor-names #{}]

    (if (empty? remaining-fibers)
      (->context-factorization (make-context (objects ctx) factor-names A-incidence) 
                                (make-context factor-names (attributes ctx) B-incidence))

      (let [fiber (first remaining-fibers)
            i (- (:i fiber) 1)]
        (if (= :obj (:fiber-type (first remaining-fibers)))
              (recur (rest remaining-fibers)
                     (conj A-incidence [(:index fiber) (str "F" i)])
                     (set/union B-incidence (into #{} (for [x (Bs i)] [(str "F" i) x])))
                     (conj factor-names (str "F" i)))
              (recur (rest remaining-fibers)
                     (set/union A-incidence (into #{} (for [x (As i)] [x (str "F" i)])))
                     (conj B-incidence [(str "F" i) (:index fiber)])
                     (conj factor-names (str "F" i)))))))
)


(defn topFiberM [ctx k tP search-limit]
  "ctx: Context to be decomposed.
   k: Rank of the resulting matrices.
   tP: Threshold on precision.
   search-limit: Maximum number of factors. Must not exceed either dimension of *ctx*."

  (let [sr (min search-limit (count (objects ctx)) (count (attributes ctx)))]

    (loop [;As (into [] (repeat (count (objects ctx)) (into [] (repeat search-limit 0))))
           ;Bs (into [] (repeat search-limit (into [] (repeat (count (attributes ctx)) 0))))
           As []
           Bs []
           tf []
           excluded-rows #{} ;objects representing excluded fibers
           excluded-cols #{} ;attributes representing excluded fibers
           X ctx
           i 1]
      (if (or (< sr i) (= 0 (reduce + (flatten (last (context-incidence-matrix X))))))

        (fiber-contexts ctx
                        As
                        Bs
                        tf)

        (let [best-row (apply max-key #(count (object-derivation X #{%})) (set/difference (objects ctx) excluded-rows)) ;object with most 1s incident
              best-col (apply max-key #(count (attribute-derivation X #{%})) (set/difference (attributes ctx) excluded-cols)) ;attribute with most 1s incident
              fiber-type (if (<= (count (object-derivation X #{best-row})) (count (attribute-derivation X #{best-col}))) :attr :obj) ;row/obj or col/attr
              best-fiber (if (= fiber-type :attr) best-col best-row) ;best row/obj or col/attr
 
              [Ai Bi] (if (= fiber-type :attr) (process-attribute-fiber X ctx best-fiber tP) 
                                               (process-object-fiber X ctx best-fiber tP))
              ix (into #{} (for [a Ai b Bi] [a b]))
              gain (- (count (filter #(incident? X (first %) (second %)) ix)) 
                      (count (filter #(not (incident? ctx (first %) (second %))) ix)))]

          (if (<= i k) 
            (recur (conj As Ai)
                   (conj Bs Bi)
                   (conj tf {:i i :fiber-type fiber-type :index best-fiber :gain gain})
                   excluded-rows
                   excluded-cols
                   (make-context (objects ctx) (attributes ctx) (set/difference (incidence-relation X) ix))
                   (+ i 1))
            (if (<= gain (apply min (map #(% :gain) tf)))
              (recur (conj As Ai)
                     (conj Bs Bi)
                     tf
                     (if (= fiber-type :obj) (conj excluded-rows best-fiber) excluded-rows)
                     (if (= fiber-type :attr) (conj excluded-cols best-fiber) excluded-cols)
                     X
                     (+ i 1))
              (let [min-fiber (apply min-key :gain tf) ;fiber in tf with least gain
                    [min-Ai min-Bi] (if (= (min-fiber :fiber-type) :attr) (process-attribute-fiber X ctx best-fiber tP) 
                                                                  (process-object-fiber X ctx best-fiber tP))
                    min-ix (into #{} (for [a min-Ai b min-Bi] [a b]))]
                (recur (conj As Ai)
                       (conj Bs Bi)
                       (conj tf (.indexOf min-fiber) {:i i :fiber-type fiber-type :index best-fiber :gain gain})
                       excluded-rows
                       excluded-cols ; (below) add back incidences from minimum fiber, remove those from new fiber
                       (make-context (objects ctx) (attributes ctx) (set/difference (set/union (set/intersection (incidence-relation ctx) min-ix)
                                                                                               (incidence-relation X))
                                                                                    ix))
                       (+ i 1)))))))))
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
(println patterns)
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
      (apply ->context-factorization (contexts-from-factors factors (objects ctx) (attributes ctx)))
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
   (apply ->context-factorization (contexts-from-factors (remaining-factors S U F ctx) (objects ctx) (attributes ctx))))
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

      (apply ->context-factorization (contexts-from-factors factors (objects ctx) (attributes ctx)))

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

(defn- find-max-pair [ctx C A B S w+ w-]
  "Greedily computes pair consisting of a row in the association matrix and
   an arbitrary binary vector of length equal to the number of objects in ctx,
   that maximizes the cover-function if added to matrices B and S, respectively.
  (Algorithm 1, Line 6)"
  (loop [current-best-pair [(first A) (into [] (repeat (count (objects ctx)) 0))]
         remaining-rows (rest A)]

    (if (empty? remaining-rows)

      current-best-pair

      (let [current-row (first remaining-rows)
            best-vector (loop [current-best-vector (into [] (repeat (count (objects ctx)) 0))
                               counter 0]

                          (if (not (< counter (count (objects ctx))))

                            current-best-vector

                            (if (< (cover (add-row B current-row)
                                          (add-column S current-best-vector)
                                          C w+ w-) 
                                   (cover (add-row B current-row) 
                                          (add-column S (assoc current-best-vector counter 1))
                                          C w+ w-))
                              (recur (assoc current-best-vector counter 1)
                                     (+ counter 1))
                              (recur current-best-vector
                                     (+ counter 1)))))]


        (if (< (cover (add-row B (first current-best-pair))
                      (add-column S (second current-best-pair))
                      C w+ w-) 
               (cover (add-row B current-row) 
                      (add-column S best-vector)
                      C w+ w-))
          (recur [current-row best-vector]
                 (rest remaining-rows))
          (recur current-best-pair
                 (rest remaining-rows))))))
)


(defn ASSO [ctx k t w+ w-]
  "Algorithm that greedily solves the discrete basis problem.
   The arguments are:
   ctx: context whose incidence matrix is to be decomposed
   k: number of binary basis vectors. Must be smaller than the smallest dimension of C
   t: threshold value ]0, 1]
   w+: weight
   w-: weight"
  (let [C (last (context-incidence-matrix ctx))
        A (association-matrix C t)  ; Association Matrix
        boolean-vectors (generate-boolean-vectors (row-number C))] 
    (loop [counter 0
           B []  ; Basis Matrix
           S []] ; Usage Matrix


    (if (= counter k)
      [S B]
           ;Pair of row of A and boolean vector that maximizes *cover*:
      (let [best-pair (find-max-pair ctx C A B S w+ w-)]
        (recur (+ counter 1)
               (add-row B (first best-pair))
               (add-column S (second best-pair)))))))
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

