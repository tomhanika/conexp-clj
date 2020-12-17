;; Copyright ⓒ the conexp-clj developers; all rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.fca.metrics
  (:require [conexp.base :refer :all]
            [conexp.fca.contexts :refer :all]
            [conexp.math.markov :refer :all]
            [clojure.core.reducers :as r]
            [clojure.math.combinatorics :refer [permuted-combinations]]
            [conexp.fca
             [contexts :refer :all]
             [exploration :refer :all]
             [implications :refer :all]
             [lattices :refer [inf sup lattice-base-set make-lattice concept-lattice lattice-order]]]
            [conexp.math.util :refer [eval-polynomial binomial-coefficient]]))

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
        B (second concept) ; intent of concept
        P_M_B (mapv #(/ (count (attribute-derivation context #{%})) n ) (difference M B))
        p_B (r/fold * (map #(/ (count (attribute-derivation context #{%})) n) B))
        one_minus_p_B_n (expt (- 1 p_B) n)]
    (loop [k 1  ;; since for k=0 the last term is 0, we can start with 1
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
           (mapv (partial *) P_M_B_k P_M_B)))))))


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
  such that they fullfil a given condition cond."
  [lat condition]
  (let [base (into [] (lattice-base-set lat))]
    (filter condition (permuted-combinations base 3))))

(defn distributive-triples
  "Given lattice lat, compute triples (x,y,z)∈L³ (pairwise different)
  such that they fullfil the distributive property
  x∨(y∧z)=(x∨y)∧(x∨z)."
  [lat]
  (let [inf  (inf lat),
        sup  (sup lat)]
    (satisfying-triples lat (fn [[x y z]] (= (sup x (inf y z ))
                           (inf (sup x y) (sup x z)))))))
   
(defn modular-triples
  "Given lattice lat, compute triples (x,y,z)∈L³ (pairwire different)
  such that they fullfil the modular property x ≤ z ⇒
  x∨(y∧z)=(x∨y)∧z."
  [lat]
  (let [inf  (inf lat),
        sup  (sup lat),
        ord (lattice-order lat)
        base (into [] (lattice-base-set lat))]
    (satisfying-triples lat (fn [[x y z]]
                              (if (ord x z) ;; if x≤y
                                (= ;; check x∨(y∧z)=(x∨y)∧z
                                 (sup x (inf y z))
                                 (inf (sup x y) z))
                                true))))) ;; else always true 

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
;;;

nil
