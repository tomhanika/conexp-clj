;; Copyright ⓒ the conexp-clj developers; all rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.fca.misc
  "More on FCA."
  (:require [conexp.base :refer :all]
            [clojure.core.reducers :as r]
            [conexp.fca
             [contexts :refer :all]
             [exploration :refer :all]
             [implications :refer :all]]))

(def ^:dynamic *fast-computation* nil)

;;; Compatible Subcontexts

(defn compatible-subcontext?
  "Tests whether ctx-1 is a compatible subcontext of ctx-2."
  [ctx-1 ctx-2]
  (and (subcontext? ctx-1 ctx-2)
       (forall [[h m] (up-arrows ctx-2)]
         (=> (contains? (objects ctx-1) h)
             (contains? (attributes ctx-1) m)))
       (forall [[g n] (down-arrows ctx-2)]
         (=> (contains? (attributes ctx-1) n)
             (contains? (objects ctx-1) g)))))

(defn compatible-subcontexts
  "Returns all compatible subcontexts of ctx. ctx has to be reduced."
  [ctx]
  (if (not (context-reduced? ctx))
    (illegal-argument "Context given to compatible-subcontexts has to be reduced."))
  (let [up-arrows         (up-arrows ctx)
        down-arrows       (down-arrows ctx)
        transitive-arrows (transitive-closure
                           (union (set-of [[g 0] [m 1]] | [g m] up-arrows)
                                  (set-of [[m 1] [g 0]] | [g m] down-arrows)))
        down-down         (set-of [g m] [[[m idx-m] [g idx-g]] transitive-arrows
                                         :when (and (= 1 idx-m) (= 0 idx-g))])
        compatible-ctx    (make-context (objects ctx)
                                        (attributes ctx)
                                        (fn [g m]
                                          (not (contains? down-down [g m]))))]
    (for [[G-H N] (concepts compatible-ctx)]
      (make-context-nc (difference (objects ctx) G-H) N (incidence ctx)))))

;;; Bonds

(defn smallest-bond
  "Returns the smallest bond between ctx-1 and ctx-2 that has the
  elements of rel as crosses."
  [ctx-1 ctx-2 rel]
  (loop [ctx-rel (make-context-nc (objects ctx-1) (attributes ctx-2) rel)]
    (let [next-rel (union (set-of [g m] [m (attributes ctx-2),
                                         g (attribute-derivation
                                            ctx-1
                                            (object-derivation
                                             ctx-1
                                             (attribute-derivation
                                              ctx-rel
                                              #{m})))])
                          (set-of [g m] [g (objects ctx-1),
                                         m (object-derivation
                                            ctx-2
                                            (attribute-derivation
                                             ctx-2
                                             (object-derivation
                                              ctx-rel
                                              #{g})))]))]
      (if (= next-rel (incidence ctx-rel))
        ctx-rel
        (recur (make-context-nc (objects ctx-1) (attributes ctx-2) next-rel))))))

(defn bond?
  "Checks whether context ctx is a context between ctx-1 and ctx-2."
  [ctx-1 ctx-2 ctx]
  (and (context? ctx)
       (context? ctx-1)
       (context? ctx-2)
       (= (objects ctx-1) (objects ctx))
       (= (attributes ctx-2) (attributes ctx))
       (forall [g (objects ctx-1)]
         (let [g-R (object-derivation ctx #{g})]
           (= g-R (context-attribute-closure ctx g-R))))
       (forall [m (attributes ctx-2)]
         (let [m-R (attribute-derivation ctx #{m})]
           (= m-R (context-object-closure ctx m-R))))))

(defn all-bonds
  "Returns all bonds between ctx-1 and ctx-2."
  [ctx-1 ctx-2]
  (let [G-1     (objects ctx-1),
        M-2     (attributes ctx-2),
        impls-1 (set-of (make-implication (cross-product A #{m})
                                          (cross-product B #{m}))
                        [impl (stem-base (dual-context ctx-1)),
                         :let [A (premise impl),
                               B (conclusion impl)],
                         m M-2]),
        impls-2 (set-of (make-implication (cross-product #{g} A)
                                          (cross-product #{g} B))
                        [impl (stem-base ctx-2),
                         :let [A (premise impl),
                               B (conclusion impl)],
                         g G-1]),
        clop (clop-by-implications (union impls-1 impls-2))]
    (map #(make-context (objects ctx-1) (attributes ctx-2) %)
         (all-closed-sets (cross-product (objects ctx-1) (attributes ctx-2))
                          clop))))

;;; Shared Intents (implemented by Stefan Borgwardt)

(defn- next-shared-intent
  "The smallest shared intent of contexts ctx-1 and ctx-2 containing b."
  [ctx-1 ctx-2 b]
  (loop [shared-attrs b,
         objs-1       (attribute-derivation ctx-1 b),
         objs-2       (attribute-derivation ctx-2 b)]
    (let [new-shared-attrs-1 (set-of m [m (difference (attributes ctx-1) shared-attrs)
                                        :when (forall [g objs-1]
                                                ((incidence ctx-1) [g m]))]),
          new-shared-attrs-2 (set-of m [m (difference (attributes ctx-2) shared-attrs)
                                        :when (forall [g objs-2]
                                                ((incidence ctx-2) [g m]))])]
      (if (and (empty? new-shared-attrs-1) (empty? new-shared-attrs-2))
        shared-attrs
        (recur (union shared-attrs new-shared-attrs-1 new-shared-attrs-2)
               (set-of g [g objs-1
                          :when (forall [m new-shared-attrs-2]
                                  ((incidence ctx-1) [g m]))])
               (set-of g [g objs-2
                          :when (forall [m new-shared-attrs-1]
                                  ((incidence ctx-2) [g m]))]))))))

(defn all-shared-intents
  "All intents shared between contexts ctx-1 and ctx-2. ctx-1 and
  ctx-2 must have the same attribute set."
  [ctx-1 ctx-2]
  (assert (= (attributes ctx-1) (attributes ctx-2)))
  (all-closed-sets (attributes ctx-1) #(next-shared-intent ctx-1 ctx-2 %)))

(defn all-bonds-by-shared-intents
  "All bonds between ctx-1 and ctx-2, computed using shared intents."
  [ctx-1 ctx-2]
  (map #(make-context (objects ctx-1) (attributes ctx-2) %)
       (all-shared-intents (context-product (adiag-context (objects ctx-1)) ctx-2)
                           (dual-context (context-product ctx-1 (adiag-context (attributes ctx-2)))))))

;;; Context for Closure Operators

(defn- maximal-counterexample
  "For a given closure operator c on a set base-set, maximizes the set
  C (i.e. returns a superset of C) that is not a superset of
  B (i.e. there exists an element m in B without C that is also not in
  the result."
  [c base-set B C]
  (let [m (first (difference B C))]
    (loop [C C,
           elts (difference (disj base-set m)
                            C)]
      (if (empty? elts)
        C
        (let [n (first elts),
              new-C (c (conj C n))]
          (if (contains? new-C m)
            (recur C (rest elts))
            (recur new-C (rest elts))))))))

(defn context-from-clop
  "Returns a context whose intents are exactly the closed sets of the
  given closure operator on the given base-set."
  [base-set clop]
  (let [base-set (set base-set)]
    (:context
     (explore-attributes :context (make-context #{} base-set #{})
                         :handler (fn [_ _ impl]
                                    (let [A (premise impl),
                                          B (conclusion impl),
                                          C (clop A)]
                                      (when-not (subset? B C)
                                        [[(gensym) (maximal-counterexample clop base-set B C)]])))))))

;;; Certain contexts

(defn implication-context
  "Returns a context for a non-negative number n which has as it's
  extents the closure systems on the set {0 .. (n-1)} and as it's
  intents the corresponding implicational theory."
  [n]
  (assert (and (number? n) (<= 0 n))
          "Must be given a non-negative number.")
  (let [base-set  (set-of-range 0 n)
        base-sets (subsets base-set)]
    (make-context base-sets
                  (set-of (make-implication A #{x}) |
                          A base-sets
                          x (difference base-set base-sets))
                  respects?)))

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

(defn concept-probability
  "Compute the probability of a `concept' in `context' 𝕂 in the following manner.
  Let pₘ be the relative frequence of attribute m in context.  The
  probability of a subset B ⊆ M in 𝕂 is the product of all pₘ for m ∈ B.
  Then the probability of a concept is defined by pr(A,B):=pr(B=B'')
  which is: $\\sum_{k=0}^n {n\\choose k} p_B^k(1-p_B)^{n-k}\\prod_{m\\in
  M\\setminus B}(1-p_m^k).$"
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
