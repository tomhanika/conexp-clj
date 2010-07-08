;; Copyright (c) Daniel Borchmann. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.fca.more
  (:use conexp.base
        conexp.fca.contexts
        conexp.fca.implications)
  (:require [clojure.contrib.graph :as graph]))

(ns-doc "More on FCA.")


;;; Subcontexts

(defn subcontext?
  "Tests whether ctx-1 is a subcontext ctx-2 or not."
  [ctx-1 ctx-2]
  (let [objs-1 (objects ctx-1)
	objs-2 (objects ctx-2)
	atts-1 (attributes ctx-1)
	atts-2 (attributes ctx-2)]
    (and (subset? objs-1 objs-2)
	 (subset? atts-1 atts-2)
	 (forall [[g m] (incidence ctx-1)]
	   (=> (and (contains? objs-1 g)
		    (contains? atts-1 m))
	       (contains? (incidence ctx-2) [g m]))))))

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
  (if (not (reduced? ctx))
    (illegal-argument "Context given to compatible-subcontexts has to be reduced."))
  (let [up-arrows        (up-arrows ctx)
	down-arrows      (down-arrows ctx)
	subcontext-graph (graph/transitive-closure
			  (struct graph/directed-graph
				  (disjoint-union (objects ctx) (attributes ctx))
				  (fn [[x idx]]
				    (condp = idx
				      0 (for [[g m] up-arrows
					      :when (= g x)]
					  [m 1])
				      1 (for [[g m] down-arrows
					      :when (= m x)]
					  [g 0])))))
	down-down        (set-of [g m] [m (attributes ctx)
					[g idx] (graph/get-neighbors subcontext-graph [m 1])
					:when (= idx 0)])
	compatible-ctx   (make-context (objects ctx)
				       (attributes ctx)
				       (fn [g m]
					 (not (contains? down-down [g m]))))]
    (for [[G-H N] (concepts compatible-ctx)]
      (make-context (difference (objects ctx) G-H) N (incidence ctx)))))

(defn restrict-concept
  "Restricts the given concept to the given subcontext."
  [concept subcontext]
  [(intersection (first concept) (objects subcontext)),
   (intersection (second concept) (attributes subcontext))])

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
  (let [G-1 (objects ctx-1),
        M-2 (attributes ctx-2),
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

;;; Shared Intents (by Stefan Borgwardt)

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

;;;

nil
