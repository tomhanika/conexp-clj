;; Copyright (c) Daniel Borchmann. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.fca.context-automorphisms
  (:use conexp.base
        conexp.util.graph
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

(defn context-automorphisms-generators
  "Computes the context automorphisms of ctx as pairs of bijective mappings acting on the objects
  and the attributes of ctx, respectively.  Returns its result as a lazy sequence."
  [ctx]
  (let [objs (vec (objects ctx)),
        atts (vec (attributes ctx)),
        ctx  (make-context (range (count objs))
                           (range (count objs) (+ (count objs) (count atts)))
                           (fn [x y]
                             (incident? ctx (objs x) (atts (- y (count objs)))))),
        ;;
        obj-map (into {} (map-indexed vector objs)),
        rob-map (map-invert obj-map),
        att-map (into {} (map-indexed #(vector (+ (count objs) %1) %2) atts)),
        rat-map (map-invert att-map),
        ;;
        tn   (terminal-nodes ctx [(objects ctx) (attributes ctx)])]
    (distinct
     (for [pi    tn,
           sigma tn,
           :when (= (set-of [(pi g) (pi m)] | [g m] (incidence ctx))
                    (set-of [(sigma g) (sigma m)] | [g m] (incidence ctx)))]
       (let [pi      (into {} (map-indexed vector pi)),
             sigma-1 (map-invert (into {} (map-indexed vector sigma)))]
         [(map-by-fn (comp obj-map sigma-1 pi rob-map)
                     objs),
          (map-by-fn (comp att-map sigma-1 pi rat-map)
                     atts)])))))

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
