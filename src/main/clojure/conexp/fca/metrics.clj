;; Copyright ⓒ the conexp-clj developers; all rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.fca.metrics
  (:require [clojure.core.reducers :as r]
            [clojure.math.combinatorics :refer [permuted-combinations combinations]]
            [clojure.set :as set]
            ;; [clojure.math.numeric-tower :refer [log]]
            [conexp.base :refer :all]
            [conexp.math.markov :refer :all]
            [conexp.fca
             [contexts :refer [make-context incidence dual-context
                               attribute-derivation
                               context-attribute-closure
                               context-object-closure
                               object-derivation random-context
                               objects attributes
                               context? concept?]]
             [exploration :refer :all]
             [fast :refer [with-binary-context
                           to-bitset
                           bitwise-context-attribute-closure
                           bitwise-object-derivation
                           bitwise-attribute-derivation concepts]]
             [implications :refer :all]
             [lattices :refer [inf 
                               sup 
                               lattice-base-set
                               make-lattice 
                               make-lattice-nc
                               concept-lattice 
                               lattice-order
                               distributive?
                               lattice-one
                               lattice-zero]]]
            [conexp.math.util :refer [eval-polynomial binomial-coefficient]])
  (:import [conexp.fca.lattices Lattice]
           [java.util ArrayList BitSet]))


;; help (outsource me)
(defn log2 [n]
  (/ (Math/log n) (Math/log 2)))

;;; Concept Stability and the like

(defn concept-stability
  "Compute the concept stability of `concept' in `context'."
  [context concept]

  (assert (context? context)
          "First argument must be a formal context.")
  (assert (and (vector? concept)
               (= 2 (count concept))
               (concept? context concept))
          "Second argument must be a formal concept of the given context.")

  (let [[extent intent] [(first concept) (second concept)]
        counter         (fn counter
                          ;; Perform depth-first search to count all subsets of
                          ;; `extent' whose derivation in context yields `intent'.
                          ;; For this we keep the list `fixed-included' of already
                          ;; considered elements to be included in the target
                          ;; subset of `extent', and the list `unfixed' of
                          ;; unconsidered elements for which it is still to be
                          ;; decided of whether they will be included or not.
                          [fixed-included unfixed]
                          (if (empty? unfixed)
                            1
                            (let [some-element (first unfixed)]
                              (+ (counter (conj fixed-included some-element)
                                          (disj unfixed some-element))
                                 (if (= intent
                                        (object-derivation context
                                                           (union fixed-included
                                                                  (disj unfixed some-element))))
                                   (counter fixed-included (disj unfixed some-element))
                                   0)))))]
    (/ (counter #{} extent)
       (expt 2 (count extent)))))

(defn separation-index
  "The concept separation is an importance measure for concepts. It
  computes the size AxB (c-inc) relative to uncovered incidences Ax(M-B)
  and (G-A)xB (o-inc). Max value is 1.

  Klimushkin M., Obiedkov S., Roth C. (2010) Approaches to the
  Selection of Relevant Concepts in the Case of Noisy Data. In: Kwuida
  L., Sertkaya B. (eds) Formal Concept Analysis. ICFCA 2010. Lecture
  Notes in Computer Science, vol 5986. Springer, Berlin,
  Heidelberg. https://doi.org/10.1007/978-3-642-11928-6_18"
 [context concept]
  (assert (context? context)
          "First argument must be a formal context.")
  (assert (and (vector? concept)
               (= 2 (count concept))
               (concept? context concept))
          "Second argument must be a formal concept of the given context.")
  (let [[extent intent] concept
        c-inc (* (count extent) (count intent))
        g-inc (reduce +
                 (map 
                  #(count (object-derivation context #{%})) 
                  extent))
        a-inc (reduce +
                 (map #(count (attribute-derivation context #{%})) 
                      intent))
        o-inc (- (+ g-inc a-inc)
                 c-inc)] ; is at least c-inc large
      (/ c-inc o-inc)))

(def ^:dynamic *fast-computation*
  "Enable computation of concept probability with floating point arithmetic
  instead of rationals"
  nil)

(defn concept-probability
  "Compute the probability of a `concept' in `context' 𝕂 in the following manner.
  Let pₘ be the relative frequence of attribute m in context.  The
  probability of a subset B ⊆ M in 𝕂 is the product of all pₘ for m ∈ B.
  Then the probability of a concept is defined by pr(A,B) := pr(B=B'')
  which is ∑_{k=0}^n {n choose k}·p_Bᵏ·(1-p_B)ⁿ⁻ᵏ·∏_{m ∈ M ∖ B}(1-p_mᵏ)."
  [context concept]
  (let [nr_of_objects (count (objects context))
        n  (if *fast-computation* (double nr_of_objects) nr_of_objects)
        M (attributes context)
        B (second concept)              ; intent of concept
        P_M_B (mapv #(/ (count (attribute-derivation context #{%})) n ) (difference M B))
        p_B (r/fold * (map #(/ (count (attribute-derivation context #{%})) n) B))
        one_minus_p_B_n (expt (- 1 p_B) n)]
    (if (not= (double p_B) 1.0)         ;; if concept's extent= n
      (loop [k 1 ;; since for k=0 the last term is 0, we can start with 1
             result 0
             binomial n ;; since k=0 the start binomial is n
             p_B_k  p_B
             one_minus_p_B_k  (/ one_minus_p_B_n (- 1 p_B))
             P_M_B_k P_M_B ]
        (if (or (== k n) (== p_B_k 0)) ;; either done or underflowed probability (double)
          result
          (let [new_res
                (* binomial p_B_k one_minus_p_B_k
                   (r/fold * (map #(- 1 %) P_M_B_k)))]
            (recur
             (inc k)
             (+ new_res result)
             (* binomial (/ (- n k) (inc k)))
             (* p_B_k p_B)
             (/ one_minus_p_B_k (- 1 p_B))
             (mapv (partial *) P_M_B_k P_M_B)))))
      1)))
  
  
;;; Robustness of Concepts

(defn- concept-robustness-add-next-entry
  "Helper-function for `concept-robustness-polynomial'.

  This function computes the value e(Y,`concept'), based on the already computed
  values e(Z,`concept'), which are given in the second parameter `cache' in the
  form [Z, e(Z, `concept')].

  This function is needed in the algorithm on page 19 in \"Finding Robust
  Itemsets under Subsampling\" from Tatti, Moerchen, and Calders."
  [concept cache]
  (let [newvalue (reduce
                  (fn [x V] (- x (second V)))
                  0
                  (filter #(subset? (second (first %)) (second concept))
                          cache))]
    (conj cache [concept, newvalue])))

(defn concept-robustness-polynomial
  "Return the coefficients of the robustness polynomial of `concept'.

  For the given `concept' of a context, the coefficients of the polynomial p
  corresponding to the robustness is computed by using the seq `concepts' of
  all concepts of the context.  The optional boolean parameter `sorted?' allows
  to declare the seq of concepts as being already sorted by increasing
  attribute-set.  Thus if v is the result of (robustness-polynomial concept
  concepts), then (eval-polynomial v (- 1 alpha)) computes the robustness with
  parameter alpha.

  For details see \"Finding Robust Itemsets under Subsampling\" from Tatti,
  Moerchen, and Calders, pages 17–19."
  ([concept concepts sorted?]
   (let [B (second concept)
         used-concepts (drop 1 (if sorted?
                                 (filter #(subset? B (second %)) concepts)
                                 (sort-by #(count (second %))
                                          (filter #(subset? B (second %))
                                                  concepts))))
         ;; store for all subconcepts (C_A,C_B) of concept the vector [C, e(C_B, concept)]
         concepts-with-values (reduce
                               (fn [x y]
                                 (concept-robustness-add-next-entry y x))
                               [[concept, 1]]
                               used-concepts)
         sup (count (first concept))]
     ;; use the above computed values [C, e(C_B, concept)] to compute the polynomial
     (reduce
      (fn [old-coefficients entry]
        (let [index (- sup (count (first (first entry))))]
          (assoc old-coefficients
                 index (+ (nth old-coefficients index) (second entry)))))
      (vec (take (+ 1 sup) (repeat 0)))
      concepts-with-values)))
  ([concept concepts]
   (concept-robustness-polynomial concept concepts false)))

(defn concept-robustness
  "Computes the robustness of a `concept' in a context with parameter `alpha' by
  using the seq `concepts' consisting of all concepts of the context.  The
  optional boolean parameter `sorted?' allows to declare the seq of concepts as
  beeing already sorted by increasing size of the attribute set.  This function
  uses the function concept-robustness-polynomial."
  ([concept concepts alpha sorted?]
   (assert (and (number? alpha)
                (<= 0 alpha 1))
           "Third argument must be between 0 and 1!")
   (eval-polynomial (concept-robustness-polynomial concept concepts sorted?) (- 1 alpha)))
  ([concept concepts alpha]
   (concept-robustness concept concepts alpha false)))

(defn average-concept-robustness
  "Takes the seq `concepts' consisting of all concepts of a context and computes
  the average concept robustness with parmater `alpha'."
  [concepts alpha]
  (assert (and (number? alpha)
               (<= 0 alpha 1))
          "Second argument must be between 0 and 1!")
  (let [sorted-concepts (sort-by
                         #(count (second %))
                         concepts)
        n (count sorted-concepts)
        robustness-values (map
                            #(concept-robustness
                               (nth sorted-concepts %)
                               (drop % sorted-concepts)
                               alpha
                               true)
                            (range 0 n))]
    (/ (reduce + robustness-values) n)))


;;; Similarity Measures for Concepts (implemented by Anselm von Wangenheim)

(defn jaccard-index
  "Computes the Jaccard index of two sets. This is |x ∩ y| / |x ∪ y|.
  Returns 1 if both sets are empty."
  [x y]
  (if (and (empty? x) (empty? y))
    1
    (/ (count (intersection x y)) (count (union x y)))))

(defn sorensen-coefficient
  "Computes the Sorensen coefficient of two sets.
  This is 2 * |x ∩ y| / (|x| + |y|).
  Returns 1 if both sets are empty."
  [x y]
  (if (and (empty? x) (empty? y))
    1
    (/ (* 2 (count (intersection x y))) (+ (count x) (count y)))))

(defn weighted-concept-similarity
  "Computes a weighted concept similarity for a given similatity measure `sim',
  two concepts [`c1' `c2'] and an optional weight `w' (default is 1/2).

  That is the weighted average of the similarity of the extents/object sets
  (weight `w') and the intents/attribute sets (weight 1-`w').

  This is from Alqadah, F. & Bhatnagar, R. (2011), 'Similarity measures in
  formal concept analysis.', Ann. Math. Artif. Intell. 61 (3), 249,
  https://doi.org/10.1007/s10472-011-9257-7"
  ([sim [c1 c2]] (weighted-concept-similarity sim [c1 c2] (/ 1 2)))
  ([sim [c1 c2] w]
   (assert (and (number? w)
                (<= 0 w 1))
           "Thrid argument must be between 0 and 1!")
   (+
    (* w       (sim (c1 0) (c2 0)))
    (* (- 1 w) (sim (c1 1) (c2 1))))))



;;;  Concept stability

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

;;; standard lattice properties to some degree

(defn satisfying-triples
  "Given lattice lat, compute triples (a,b,c)∈L³ (pairwise different)
  such that they fullfil a given condition. The triples may be
  filtered beforehand using prefilter."
  ([^Lattice lat condition]
  (let [base (into [] (lattice-base-set lat))]
    ;; (filter condition (filter prefilter (permuted-combinations base 3)))))
    (filter condition (permuted-combinations base 3))))
  ([lat condition prefilter]
  (let [base (into [] (lattice-base-set lat))]
    (filter condition (filter prefilter (permuted-combinations base 3))))))


(defn distributive-triples
  "Given lattice lat, compute triples (x,y,z)∈L³ (pairwise different)
  such that they fullfil the distributive property
  x∨(y∧z)=(x∨y)∧(x∨z)."
  ([^Lattice lat]
   (distributive-triples lat (fn [x] true)))
  ([^Lattice lat prefilter]
   (let [inf  (inf lat),
         sup  (sup lat)]
      (satisfying-triples lat (fn [[x y z]]
                                (= (sup x (inf y z ))
                                   (inf (sup x y) (sup x z))))
                          prefilter))))

(defn modular-triples
  "Given lattice lat, compute triples (x,y,z)∈L³ (pairwire different)
  such that they fullfil the modular property x ≤ z ⇒
  x∨(y∧z)=(x∨y)∧z."
  ([lat]
   (modular-triples lat (fn [x] true)))
  ([lat prefilter]
  (let [inf  (inf lat),
        sup  (sup lat),
        ord (lattice-order lat)
        base (into [] (lattice-base-set lat))]
    (satisfying-triples lat (fn [[x y z]]
                              (if (ord x z) ;; if x≤y
                                (= ;; check x∨(y∧z)=(x∨y)∧z
                                 (sup x (inf y z))
                                 (inf (sup x y) z))
                                true))
                                prefilter)))) ;; else always true 

(defn distributivity-degree
  "Computes the number of triples (a,b,c)∈L³ (pairwise different)
  that fullfil the distributivity law and divides it by the number of
  possible pw different triples."
  [lat]
  (let [n (count (lattice-base-set lat))]
    (if (< n 3)
      1 ;; in case we have less than 3 elements we have distributivity
    (/ (count (distributive-triples lat))
       (* 6 (binomial-coefficient n 3))))))

(defn modularity-degree
  "Computes the number of triples (a,b,c)∈L³ (pairwise different)
  that fullfil the modularity law and divides it by the number of
  possible pw different triples. (tbc later on)"
  [lat]
  (let [n (count (lattice-base-set lat))]
    (if (< n 3)
      1 ;; in case we have less than 3 elements we have modularity
    (/ (count (modular-triples lat))
       (* 6 (binomial-coefficient n 3)))))) 

(defn elements-distributivity
  "Computes the number of triples (a,b,c)∈L³ (pw different) where either
  a=e, b=e, or c=e, that fullfil the modularity law. This number is
  then divided it by the number of possible pw different triples of such kind." 
  [lat e]
  (assert (contains? (lattice-base-set lat) e))
  (let [n (count (lattice-base-set lat)),
        filterfunc (fn [[x y z]] (or (= x e) (= y e) (= z e)))]
    (/ (count (distributive-triples
               lat filterfunc))
       (* 6 (binomial-coefficient (- n 1) 2)))))

(defn elements-modularity
  [lat e]
  (assert (contains? (lattice-base-set lat) e))
  (let [n (count (lattice-base-set lat))]
    (/ (count (modular-triples lat (fn [[x y z]] (or (= x e) (= y e) (= z e)))))
       (* 6 (binomial-coefficient (- n 1) 2)))))

;;; Relevant Attributes (Objects) et Al

(defn attribute-information-entropy
  "Computes the attribute-information-entropy for a given context 𝕂using the formula
  (∑_{m ∈ M} 1-{m}''/|M|)/|M|, see https://doi.org/10.1007/978-3-030-23182-8_8"
  [ctx]
  (let [M (attributes ctx)
        nr_of_attributes (count M)]
    (/ (- nr_of_attributes
        (r/fold + (pmap
                   (fn [m] (/ (count (context-attribute-closure ctx #{m}))
                              nr_of_attributes))
                   M)))
       nr_of_attributes)))

(defn object-information-entropy
  "Computes the object-information-entropy for a given context 𝕂using the formula
  (∑_{g ∈ G} 1-{g}''/|G|)/|G|, see https://doi.org/10.1007/978-3-030-23182-8_8"
  [ctx]
  (attribute-information-entropy (dual-context ctx)))

(defn information-entropy
  "Computes the mean entropy of context ctx based on the equal
  weighted sum of attribute-information-entropy and
  object-information-entropy."
  [ctx]
  (/ (+ (attribute-information-entropy ctx) (object-information-entropy ctx)) 2))


(defn shannon-attribute-information-entropy
  "Computes the Shannon-attribute-information-entropy for a given context 𝕂using the formula
  ∑_{m ∈ M} -{m}''/|M|· log₂ ({m}''/|M|), see https://doi.org/10.1007/978-3-030-23182-8_8"
  [ctx]
  (let [M (attributes ctx)
        nr_of_attributes (count M)]
    (r/fold + (pmap (fn [x] (- (* x (log2 x))))
                    (pmap
                     (fn [m] (/ (count (context-attribute-closure ctx #{m}))
                                nr_of_attributes))
                     M)))))

(defn shannon-object-information-entropy
  "Computes the Shannon-object-information-entropy for a given context 𝕂using the formula
  ∑_{g ∈ G} -{g}''/|G|· log₂ ({g}''/|G|), see https://doi.org/10.1007/978-3-030-23182-8_8"
  [ctx]
  (shannon-attribute-information-entropy (dual-context ctx)))

(defn shannon-attribute-information-entropy-fast
  "Computes the Shannon-attribute-information-entropy for a given
  context 𝕂using the formula ∑_{m ∈ M} -{m}''/|M|· log₂ ({m}''/|M|),
  see https://doi.org/10.1007/978-3-030-23182-8_8. This implementation
  resorts to bit vectors in order to increase the computation
  speed. This conversion is not adviced for small data sets. "
  [ctx]
    (with-binary-context ctx
      (let [a-closure-counts
            (pmap (fn [y]
                    (let [^BitSet bs (BitSet.)]
                      (.set bs y)
                      (bitwise-context-attribute-closure
                       incidence-matrix
                       object-count
                       attribute-count
                       bs)))
                  (range attribute-count))]
      (-
       (r/fold + (map (fn [x] (* (/ (.cardinality ^BitSet x) ^long attribute-count) 
                                  (log2 (/ (.cardinality ^BitSet x) ^long attribute-count))))
                       a-closure-counts))))))


(defn shannon-object-information-entropy-fast
  "Computes the Shannon-object-information-entropy for a given context 𝕂using the formula
  ∑_{g ∈ G} -{g}''/|G|· log₂ ({g}''/|G|), see https://doi.org/10.1007/978-3-030-23182-8_8"
  [ctx]
  (shannon-attribute-information-entropy-fast (dual-context ctx)))

(defn shannon-information-entropy
  "Computes the mean shannon-entropy of context ctx based on the equal
  weighted sum of attribute-information-entropy and
  object-information-entropy."
  [ctx]
  (/ (+ (shannon-attribute-information-entropy ctx)
        (shannon-object-information-entropy ctx)) 2))

(defn shannon-information-entropy-fast
  "Computes the mean shannon-entropy of context ctx based on the equal
  weighted sum of attribute-information-entropy and
  object-information-entropy."
  [ctx]
  (/ (+ (shannon-attribute-information-entropy-fast ctx)
        (shannon-object-information-entropy-fast ctx)) 2))

(defn extent-label-function
  "Computes the label for given g ∈ G, i.e., the natural number n such
  that n=|{c ∈ 𝔅(𝕂) ∣ g ∈ ext(c)}|, see
  https://doi.org/10.1007/978-3-030-23182-8_8"
  ([ctx g]
   (extent-label-function ctx (concepts :in-close ctx) g))
  ([ctx concepts g]
   (count
    (filter (fn [x] (contains? (first x) g))
            concepts))))

(defn intent-label-function
  "Computes the label for given m ∈ M, i.e., the natural number n such
  that n=|{c ∈ 𝔅(𝕂) ∣ m ∈ int(c)}|, see
  https://doi.org/10.1007/978-3-030-23182-8_8"
  ([ctx m]
   (extent-label-function ctx (concepts :in-close ctx) m))
  ([ctx concepts m]
   (count
    (filter (fn [x] (contains? (second x) m))
            concepts))))

(defn attribute-removal-robust-concepts
  "Computes the set of concepts {c ∈ 𝔅(𝕂) ∣ (int(c) ∖ A)' = ext(c)}
  for some attribute set A ⊆ M."
  ([ctx A]
   (attribute-removal-robust-concepts ctx (concepts :in-close ctx) A))
  ([ctx theconcepts A]
   (into [] (r/filter (fn [x] (=
                    (attribute-derivation ctx (difference (second x) A))
                    (first x)))
           theconcepts))))

(defn object-removal-robust-concepts
  "Computes the set of concepts {c ∈ 𝔅(𝕂) ∣ (ext(c) ∖ A)' = int(c)}
  for some attribute set A ⊆ G."
  ([ctx A]
   (object-removal-robust-concepts ctx (concepts :in-close ctx) A))
  ([ctx theconcepts A]
   (into [] (r/filter (fn [x] (=
                    (object-derivation ctx (difference (first x) A))
                    (second x)))
           theconcepts))))


(defn relative-relevance
  "Computes for formal context (G,M,I) the relative relevance of
  attribute set A ⊆ M, using the formula from Proposition 3.5 in
  https://doi.org/10.1007/978-3-030-23182-8_8, i.e.,
  r(A)=1-∑_{c∈𝔅(𝕂_A)}|ext(c)|/∑_{c∈𝔅(𝕂)}|ext(c)|,
  where 𝔅(𝕂_A)={c∈𝔅(𝕂)∣(int(c)∖A)'=ext(c)}"
  ([ctx A]
   (relative-relevance ctx (concepts :in-close) A))
  ([ctx theconcepts A]
   (let [extsum (r/fold + (map (fn [x] (count (first x))) theconcepts))]
     (- 1
        (->> theconcepts
             (filter
              (fn [x] (= (count
                          (attribute-derivation ctx (difference (second x) A)))
                         (count (first x)))))
             (pmap (fn [x] (count (first x))))
             (r/fold +)
             (* (/ 1 extsum)))
        ))))

(defn relative-relevance-fast
  "Compute relative-relevance using bitsets"
  [ctx theconcepts A]
  (with-binary-context ctx
    (let [o-prime (partial bitwise-object-derivation incidence-matrix object-count attribute-count)
          a-prime (partial bitwise-attribute-derivation incidence-matrix object-count attribute-count)
          theextents (pmap (fn [x] (to-bitset object-vector (first x))) theconcepts)
          theatts (to-bitset attribute-vector A)]
      (- 1 
         (/
          (r/fold + (map (fn [x]
                            (if (=
                                 (let [thing (o-prime x)]
                                   (.andNot thing theatts) ;; remove attribute set
                                   (.cardinality (a-prime thing)))
                                 (.cardinality x))
                              (.cardinality x)
                              0))
                          theextents))
          (r/fold + (map (fn [x] (.cardinality x)) theextents)))))))

(defn next-maximal-relevant
  "Given a formal context (G,M,I), an attribute set A ⊆ M, this
  functions computes the most relevant m ∈ M ∖ A with respect to relative-relevance."
  ([ctx theconcepts] (next-maximal-relevant ctx theconcepts #{}))
  ([ctx theconcepts A]
   (let [atts (attributes ctx)
         available-atts (difference atts A)
         obs  (objects ctx)]
     (apply max-key (fn [x] (relative-relevance-fast ctx theconcepts (union A #{x})))
            available-atts))))

(defn next-n-maximal-relevant
  "Based on next-maximal-relevant, compute the the next n maximal
  relevant features in a consekutive manner. Please be advised, this
  is not necessiraly equivalent to compute the subset N ⊆ M with |N|=n
  being the most relevant wrt relative-relevance."
  ([ctx theconcepts n]
   (next-n-maximal-relevant ctx theconcepts n #{}))
  ([ctx theconcepts n A]
   (loop [[counter resultset] [0 []]]
     (if (< counter n)
       (recur [(inc counter)
               (conj resultset
                     (next-maximal-relevant ctx theconcepts (set (union resultset A))))])
       resultset))))

(defn n-maximal-relevant
  "Based on relative-relevance, compute the the subset N ⊆ M with |N|=n,
  such that there is no L ⊆ M with r(N)<r(L), i.e., N is less relevant
  thatn L, see https://doi.org/10.1007/978-3-030-23182-8_8."
  ([ctx theconcepts n]
   (let [atts (attributes ctx)
         combs (vec (combinations atts n))]
     (set
     (apply max-key (fn [x] (relative-relevance-fast ctx theconcepts (set x)))
            combs)))))


(defmulti next-maximal-relevant-approx
  "Compute next-maximal-relevant attribute using either Shannon or
  context entropy, cf https://doi.org/10.1007/978-3-030-23182-8_8."
  (fn [& args]
    (when-not (<= 1 (count args) 3)
      (illegal-argument "Wrong number of arg."))
    (when (and (= 3 (count args))
               (not (keyword? (first args))))
      (illegal-argument "First argument must be a keyword"))
    (when (and (= 2 (count args))
               (and
                (not (keyword? (first args)))
                (not (context? (first args)))))
      (illegal-argument "First argument to rel-approx must be a
      keyword and the second argument must be a context."))
    (when (and (= 1 (count args))
               (not (context? (first args))))
    (illegal-argument "If arity is 1, the argument must be a context."))
    (cond
      (= 3 (count args)) (first args)
      (= 1 (count args))  ::default-entropy
      (= 2 (count args)) (if (keyword? (first args))
                          (first args)
                          ::default-entropy))))


(defmethod next-maximal-relevant-approx ::default-entropy
  ([context]
   (next-maximal-relevant-approx :contextual context #{}))
  ([context A]
  (next-maximal-relevant-approx :contextual context A)))

(defmethod next-maximal-relevant-approx :contextual 
  [_ ctx A]
   (let [atts (attributes ctx)
         available-atts (difference atts A)
         obs  (objects ctx)]
     (apply max-key (fn [x]
            (let [pctx (make-context obs (union #{x} A) (incidence ctx))]
                (*
                 (count (concepts :in-close pctx))
                 (object-information-entropy pctx))))
            available-atts)))

(defmethod next-maximal-relevant-approx :shannon
  [_ ctx A]
  (let [atts (attributes ctx)
        available-atts (difference atts A)
        obs  (objects ctx)]
    (apply max-key (fn [x]
               (let [pctx (make-context obs (union #{x} A) (incidence ctx))]
                 (*
                  (count (concepts :in-close pctx))
                  (shannon-object-information-entropy-fast pctx))))
           available-atts)))

(defmulti next-n-maximal-relevant-approx
  "Compute next-n-maximal-relevant attributes in a consecutive manner
  using either Shannon or context entropy, cf
  https://doi.org/10.1007/978-3-030-23182-8_8."
  (fn [& args]
    (when-not (<= 2 (count args) 4)
      (illegal-argument "Wrong number of arg."))
    (when (and (= 4 (count args))
               (not (keyword? (first args))))
      (illegal-argument "First argument must be a keyword"))
    (when (and (= 3 (count args))
               (and
                (not (keyword? (first args)))
                (not (context? (first args)))))
      (illegal-argument "First argument to n-rel-approx must be a
      keyword and the second argument must be a context."))
    (when (and (= 2 (count args))
               (not (context? (first args))))
    (illegal-argument "If arity is 2, the argument must be a context."))
    (cond
      (= 4 (count args)) (first args)
      (= 2 (count args))  ::default-entropy
      (= 3 (count args)) (if (keyword? (first args))
                          (first args)
                          ::default-entropy))))

(defmethod next-n-maximal-relevant-approx ::default-entropy
  ([context n]
   (next-n-maximal-relevant-approx :contextual context n #{}))
  ([context n A]
  (next-n-maximal-relevant-approx :contextual context n A)))

(defmethod next-n-maximal-relevant-approx :contextual 
  [_ ctx n A]
  (loop [k 1 result []]
    (if (> k n)
      result
      (recur
       (inc k)
       (conj result
             (let [atts (attributes ctx)
                   available-atts (difference atts (union A result))
                   obs  (objects ctx)]
               (apply max-key (fn [x]
                                (let [pctx (make-context obs (union #{x} result A) (incidence ctx))]
                                  (*
                                   (count (concepts :in-close pctx))
                                   (object-information-entropy pctx))))
                      available-atts)))))))

(defmethod next-n-maximal-relevant-approx :shannon
  ([_ ctx n]
  (next-n-maximal-relevant-approx :shannon ctx n #{}))
  ([_ ctx n A]
   (loop  [k 1
           result []]
    (if (> k n)
      result
      (recur
       (inc k)
       (conj result (let [atts (attributes ctx)
                   available-atts (difference atts (union A result))
                   obs  (objects ctx)]
               (apply max-key (fn [x]
                                (let [pctx (make-context obs (union #{x} A result) (incidence ctx))]
                                  (*
                                   (count (concepts :in-close pctx))
                                   (shannon-object-information-entropy-fast pctx))))
                      available-atts))))))))


(defn nRandomAtts
  [ctx n]
  (let [atts (attributes ctx)]
    (take n (shuffle atts))))

;; (defn rel-consistency  ???
;;   "Computes the relative consistency of a subset $N ⊆ M$ with respect to
;;   a given formal context (G,M,I),  and some attribute subset N ⊆ M"
;;   [ctx N]
;;   (let [G (objects ctx)
;;         M (attributes ctx)
;;         MN (difference M N)
;;         ctx-N (make-context G N (incidence ctx))
;;         ctx-MN (make-context G MN (incidence ctx))]
;;     (let [derive-N (fn [x] (context-object-closure ctx-N x))
;;           derive-MN (fn [x] (context-object-closure ctx-MN x))]
;;       (/
;;        (count (filter (fn [x] (subset? (derive-N #{x}) (derive-MN #{x}))) G))
;;        (count G)
;;        ))))

;;   )

;returns destributive lattice generated by the concepts in "triple"
;triple must be a collection of three concepts in "lat"
(defn generate-from-triple [triple lat]

  (let [stage1-meets (set (filter some? (for [a triple b triple] (if (not= a b) ((inf lat) a b)))))
        stage1-joins (set (filter some? (for [a triple b triple] (if (not= a b) ((sup lat) a b)))))
        
        stage2-meets (set (filter some? (for [a stage1-joins b stage1-joins] (if (not= a b) ((inf lat) a b)))))
        stage2-joins (set (filter some? (for [a stage1-meets b stage1-meets] (if (not= a b) ((sup lat) a b)))))


        final-meet #{ (into [] ((inf lat) 
                                 ((inf lat) (first stage1-joins) (second stage1-joins)) 
                                 (last stage1-joins)))}

        base-set (set/union triple
                            stage1-meets 
                            stage1-joins
                            stage2-meets
                            stage2-joins
                            final-meet)]
     
        (make-lattice-nc base-set
                       (lattice-order lat)
                       (inf lat)
                       (sup lat))

))

(defn neutral? [a lat]
  "Verifies if *a* is a neutral element in *lat*."
  (let [base-set (lattice-base-set lat)
        join (sup lat)
        meet (inf lat)]
     (every? identity (for [x base-set y base-set] (= (meet (meet (join a x) (join a y)) (join x y))
                                                      (join (join (meet a x) (meet a y)) (meet x y)))))
)
)

(defn neutral-concepts [lat]
  "Returns all neutral elements in *lat*."
  (let [base-set (lattice-base-set lat)]
      (filter #(neutral? % lat) base-set)
)
)


(defn element-complement [concept lat]
  "Returns all complements of *concept* in *lat*."
  (let [base-set (lattice-base-set lat)]

      (filter #(and (not= % concept)
                    (= ((sup lat) concept %) (lattice-one lat))
                    (= ((inf lat) concept %) (lattice-zero lat)))
              base-set))
)

;;;
nil
