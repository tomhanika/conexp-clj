;; Copyright ⓒ the conexp-clj developers; all rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.fca.distributivity
  "Methods to complete a lattice or context to the a distributive
  lattice."
  (:require [conexp.base :refer :all]
            [conexp.fca.contexts :refer :all]
            [conexp.fca.implications :refer :all]))

(defn birkhoff-downset-completion 
  "Returns the downset birkhoff completion of a formal context. 
  (G,M,I) -> (G,M U J(G),I U {(g1,g2) in G x J(G) |  not g1''>= g2'' })."
  [ctx]
  (let [irreducible-objects (-> ctx reduce-objects objects)]
    (assert (empty? (intersection (attributes ctx) irreducible-objects)) "Object and Attribute sets should be disjoint")
    (make-context (objects ctx)
                  (union (attributes ctx) irreducible-objects)
                  (union (incidence-relation ctx)
                         (set-of [g1 g2] [g1 (objects ctx), g2 irreducible-objects 
                                          :when
                                          (not (subset? (context-object-closure ctx #{g2})
                                                        (context-object-closure ctx #{g1})))]) )) ))

(defn birkhoff-upset-completion 
  "Returns the downset birkhoff completion of a formal context. 
  (G,M,I) -> (G U M(M), M,I U {(m1,m2) in M x M(M) |  not m1''>= m2'' })."
  [ctx]
  (-> ctx dual-context birkhoff-downset-completion dual-context))


(defmulti non-distributive-proper-premises
  "Returns the set of proper premises that contradict the distributivity property."
  (fn [thing _] thing))

(defmethod non-distributive-proper-premises :attributes
  [_ ctx]
  (assert (context-reduced? ctx) "The context must be reduced")
  (filter (fn [A] (<= 2 (count A)))
          (proper-premises ctx)) )


(defmethod non-distributive-proper-premises :objects
  [_ ctx]
  (->> ctx dual-context 
           (non-distributive-proper-premises :attributes)) )


(defmulti proper-premise-height
  "Returns the number of super-concepts of the premise."
  (fn [thing _ _] thing))

(defmethod proper-premise-height :objects
  [_ ctx premise]
  (assert (context-reduced? ctx) "The context must be reduced")
  (let [exts (extents ctx)]
    (->>  exts
         (filter #(subset? premise %1))
         count)  ))

(defmethod proper-premise-height :attributes
  [_ ctx premise]
  (assert (context-reduced? ctx) "The context must be reduced")
  (let [ints (intents ctx)]
    (->>  ints
         (filter #(subset? premise %1))
         count)  ))

(defmulti proper-premise-support
  "Returns the number of super-concepts of the premise."
  (fn [thing _ _] thing))

(defmethod proper-premise-support :objects
  [_ ctx premise]
  (assert (context-reduced? ctx) "The context must be reduced")
  (support premise (dual-context ctx)) )

(defmethod proper-premise-support :attributes
  [_ ctx premise]
  (assert (context-reduced? ctx) "The context must be reduced")
  (support premise (dual-context ctx)) )


(defmulti truncated-birkhoff-downset-completion 
  (fn [ctx order amount] order))

(defmethod truncated-birkhoff-downset-completion :height
  [ctx _ amount]
  (truncated-birkhoff-downset-completion ctx 
                                 (fn [proper-premise] (proper-premise-height :objects ctx proper-premise))
                                 amount) )

(defmethod truncated-birkhoff-downset-completion :support
  [ctx _ amount]
  (truncated-birkhoff-downset-completion ctx 
                                 (fn [proper-premise] (proper-premise-support :objects ctx proper-premise))
                                 amount) )

(defmethod truncated-birkhoff-downset-completion :default
  [ctx order-fn amount]
  (assert (context-reduced? ctx) "The context must be reduced")
  (let [premises (-> ctx dual-context proper-premises )
        non-distributive (filter (fn [A] (not= 1 (count A))) 
                                 premises)
        removed (->> non-distributive
                     (sort-by order-fn > )
                     (take amount))
        truncated-premise-set (difference (set premises)
                                          (set removed))
        truncated-implication-set (map (fn [premise] (make-implication premise 
                                                          (context-object-closure ctx premise)))
                      truncated-premise-set)
        truncated-closure-system (->> truncated-implication-set
                                      clop-by-implications
                                      (all-closed-sets (objects ctx)))
        unreduced-completion (make-context (objects ctx)
                                           truncated-closure-system
                                           (fn [g m] (contains? m g)))]
    (make-context (objects ctx)
                  (union (attributes ctx)
                         (-> unreduced-completion reduce-context attributes))
                  (union (incidence-relation ctx)
                         (incidence-relation unreduced-completion))) ))

(defn truncated-birkhoff-upset-completion 
  [ctx order-fn amount]
  (-> ctx dual-context 
      (truncated-birkhoff-downset-completion order-fn amount)
      dual-context) )

(defn truncated-birkhoff-upset-comption-by-implications 
  [ctx imps & {:keys [clarify] :or {clarify false}}]
  (let [L (->> imps clop-by-implications (all-closed-sets (attributes ctx)))
        JL (-> L (make-context (attributes ctx) contains?)
               reduce-objects) 
        ctxJL (make-context (into (objects ctx) (objects JL))
                             (attributes ctx)
                             (into (incidence-relation ctx)
                                   (incidence-relation JL)))
        clarify-JL (filter (fn [o] (if (coll? o)
                                       (not (some (fn [g] (= (object-derivation ctx #{g})
                                                             (object-derivation JL #{o}))) 
                                                  (objects ctx)))
                                       true)) (objects ctxJL))]
    (if clarify 
      (make-context clarify-JL 
                    (attributes ctxJL)
                    (incidence-relation ctxJL))
      ctxJL ) ) )

(defn truncated-birkhoff-downset-comption-by-implications 
  [ctx imps & args]
  (-> ctx dual-context 
      (truncated-birkhoff-upset-comption-by-implications imps args)
      dual-context) )

(defn truncated-birkhoff-upset-completion
  [ctx]
  (let [P (proper-premise-implications ctx)
        truncated-imps (filter (fn [i] (let [p (premise i)] 
                                         (or (= 1 (count p))
                                             (= (attributes ctx) (context-attribute-closure ctx p))))) 
                               P)]
    (truncated-birkhoff-upset-comption-by-implications ctx truncated-imps) ) )

(defn truncated-birkhoff-downset-completion
  [ctx]
  (-> ctx dual-context truncated-birkhoff-upset-completion dual-context) )
