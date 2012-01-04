;; Copyright (c) Daniel Borchmann. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.fca.context-automorphisms
  (:use conexp.base
        [conexp.util.graph :only (make-directed-graph, make-ordered-partition, automorphism-group)]
        conexp.fca.contexts))

(ns-doc "Context Automorphisms.")

;;;

(defn context-automorphism?
  "Returns true if and only if the pair [alpha beta] is a context automorphism of ctx."
  [ctx [alpha beta]]
  (and (or (map? alpha) (fn? alpha))
       (or (map? beta) (fn? beta))
       (forall [g (objects ctx)
                m (attributes ctx)]
         (<=> (contains? (incidence ctx) [g m])
              (contains? (incidence ctx) [(alpha g) (beta m)])))))

(defn- context-to-graph
  "Converts a given context to a bipartite graph." ;mention how objects and attributes are named in
                                                   ;the resulting graph
  [ctx]
  (make-directed-graph (disjoint-union (objects ctx) (attributes ctx))
                       (memoize (fn [[x n]]
                                  (if (= n 0)
                                    ;; object
                                    (set-of [m 1] | m (oprime ctx #{x}))
                                    ;; attribute
                                    (set-of [g 0] | g (aprime ctx #{x})))))))

(defn context-automorphisms
  "Computes the context automorphisms of ctx as pairs of bijective mappings acting on the objects
  and the attributes of ctx, respectively.  Returns its result as a lazy sequence."
  [ctx]
  (let [graph-automorphisms (automorphism-group (context-to-graph ctx)
                                                (make-ordered-partition [(set-of [g 0] | g (objects ctx))
                                                                         (set-of [m 1] | m (attributes ctx))]))]
    (for [tau graph-automorphisms]
      [(map-by-fn (fn [g]
                    (first (tau [g 0])))
                  (objects ctx)),
       (map-by-fn (fn [m]
                    (first (tau [m 1])))
                  (attributes ctx))])))

(defn isomorphic-contexts?
  "Tests whether ctx-1 and ctx-2 are isomorphic."
  [ctx-1 ctx-2]
  (and (= (count (objects ctx-1)) (count (objects ctx-2)))
       (= (count (attributes ctx-1)) (count (attributes ctx-2)))
       (= (count (incidence ctx-1)) (count (incidence ctx-2)))
       (= (canonical-isomorph ctx-1)
          (canonical-isomorph ctx-2))))

(defn context-object-automorphisms
  "Computes the parts of the context automorphisms of ctx which act on the objects of ctx.  Returns
  its result as a lasy sequence."
  [ctx]
  (map first (context-automorphisms ctx)))

(defn context-attribute-automorphisms
  "Computes the parts of the context automorphisms of ctx which act on the attributes of ctx.
  Returns its result as a lazy sequence."
  [ctx]
  (map second (context-automorphisms ctx)))

(defn rigid?
  "Returns true if and only if ctx does not have any other automorphisms than the identity."
  [ctx]
  (nil? (second (context-object-automorphisms ctx))))

(defn induced-object-automorphism
  "Returns the automorphism on the objects of ctx that is induced by beta, which is part of an
  context automorphism of ctx and acts on the attributes of ctx.  Note that ctx must be clarified."
  [ctx beta]
  (assert (clarified? ctx)
          "Given context must be clarified to uniquely determine induced object automorphism.")
  (let [pairs (set-of [g h] | g (objects ctx)
                              h (objects ctx) ;this choice can be made finer
                              :when (forall [m (attributes ctx)]
                                      (<=> (contains? (incidence ctx) [g m])
                                           (contains? (incidence ctx) [h (beta m)]))))]
    (into {} pairs)))

(defn induced-attribute-automorphism
  "Returns the automorphism on the attributes of ctx that is induced by alpha, which is part of an
  context automorphism of ctx and acts on the objects of ctx.  Note that ctx must be clarified."
  [ctx alpha]
  (assert (clarified? ctx)
          "Given context must be clarified to uniquely determine induced attribute automorphism.")
  (let [pairs (set-of [m n] | m (attributes ctx)
                              n (attributes ctx) ;see above
                              :when (forall [g (objects ctx)]
                                      (<=> (contains? (incidence ctx) [g m])
                                           (contains? (incidence ctx) [(alpha g) n]))))]
    (into {} pairs)))

;;;

nil
