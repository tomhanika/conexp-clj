(ns conexp.fca.incomplete-contexts.random-experts
  (:require [clojure.set :refer [subset? intersection difference union]]
            [conexp.base :refer [with-str-out cross-product hash-combine-hash]]
            [conexp.fca.implications :refer :all]
            [conexp.fca.contexts :as cxt]
            [conexp.fca.random-contexts :as rcxt]
            [conexp.fca.incomplete-contexts.conexp-interop :refer :all]
            [conexp.fca.incomplete-contexts.incomplete-contexts :as icxt :refer :all]
            [conexp.fca.incomplete-contexts.experts :refer :all]))


(defn induce-random-questionmarks
  "Given a formal or incomplete context introduce random '?'s, by generating a random context and use its crosses as incidences of the '?'"
  [K]
  (let [partial-cxt (to-incomplete-context K)
        objs (icxt/objects partial-cxt)
        atts (icxt/attributes partial-cxt)
        unknown-incidences (into {} (for [[g m] (cxt/incidence-relation (rcxt/random-dirichlet-context :attributes atts :objects objs))]
                                     [[g m] icxt/unknown]))
        new-inz (-> (to-incomplete-context K)
                    (icxt/incidence)
                    (#(merge % unknown-incidences)))]
    (icxt/make-incomplete-context objs atts new-inz)
    ))


(defn- contains-maximal-contranominal-scale?
  "Given a formal context K, computes whether K contains the largest contranominal scale, i.e., if there are objects such that each set of |M|-1 attributes is an intent."
  [K]
  (let [K (to-formal-context K)
        M (set (cxt/attributes K))
        G (set (cxt/objects K))]
    (loop [g (first G)
           restG (rest G)
           S (set (for [m M] (difference M #{m})))]
      (cond (empty? S)
            true
            (nil? g)
            false
            true
            (recur (first restG)
                   (rest restG)
                   (difference S #{(set (cxt/object-derivation K #{g}))}))))))


(defn random-satisfiable-implication-theory-for-cxt
  "Given a formal or incomplete context K, create an implication theory of satisfiable implications.
   This works by generating a random context on the same set of attributes, forming the subposition and computing the canonical base.
   If 'retry-on-empty-result' is set to true we repeat the generation if the result would be empty because the random context contains a contranominal scale.
  Note: If K already contains a contranominal scale the result will always be an empty set."
  ([K]
   (random-satisfiable-implication-theory-for-cxt K true))
  ([K retry-on-empty-result]
   (let [K1 (incomplete-context->possible-incidences-context (to-incomplete-context K))
         K2 (if (and retry-on-empty-result (not (contains-maximal-contranominal-scale? K1)))
              (loop [trys 5
                     genK (rcxt/random-dirichlet-context-no-maximal-contranominal :attributes (cxt/attributes K1))]
                (if (and (contains-maximal-contranominal-scale? (icxt/subposition [(to-incomplete-context K1) (to-incomplete-context genK)])) (< 0 trys))
                  (recur (dec trys)
                         (rcxt/random-dirichlet-context-no-maximal-contranominal :attributes (cxt/attributes K1)))
                  genK))
              (rcxt/random-dirichlet-context :attributes (cxt/attributes K1)))
         subposK (icxt/subposition [(to-incomplete-context K1) (to-incomplete-context K2)])]
     (canonical-base (to-formal-context subposK))
     )))


(defn make-incomplete-expert-from-cxt
  "randomly decide which relations and which implications the expert knows about (the certainly valid implications ?)"
  ([cxt]
   (let [base (canonical-base cxt)
         cxt-part (induce-random-questionmarks cxt)
         impls-part (take  (rand-int (inc (count base))) (shuffle base))]
     (make-expert impls-part  cxt-part))))


(defn make-random-expert-for-universe
  "make a random expert for a universe K"
  ([K]
   (let [K1 (induce-random-questionmarks K)
         impls (random-satisfiable-implication-theory-for-cxt K)]
     (make-expert impls K1)))
  ([K name]
   (let [K1 (induce-random-questionmarks K)
         impls (random-satisfiable-implication-theory-for-cxt K)]
     (make-expert impls K1 name))))


(defn- rand-int-between-min-times-and-max-times-n
  [n min-times max-times]
  {:pre [(<= min-times max-times)]}
  (Math/round (* n (+ min-times (rand (- max-times min-times))))))


(defn make-random-expert-for-domain
  "make a random expert for a domain
  Input: either a formal context K or a pair [G M] of objects and attributes"
  ([K]
   (let [iK (to-incomplete-context K)]
     (make-random-expert-for-domain (icxt/objects iK) (icxt/attributes iK) (java.util.UUID/randomUUID))))
  ([K name]
   (let [iK (to-incomplete-context K)]
     (make-random-expert-for-domain (icxt/objects iK) (icxt/attributes iK) name)))
  ([G M name]
   (let [newG (rand-int-between-min-times-and-max-times-n (count G) 0.5 2)
         K (rcxt/random-dirichlet-context-no-maximal-contranominal :attributes M :objects newG)
         K1 (induce-random-questionmarks K)
         impls (random-satisfiable-implication-theory-for-cxt K)]
     (make-expert impls K1 name))))


(defn- valid-expert?
  [expert]
  (let [impls (known-implications expert)
        K (known-examples expert)]
    (every? true? (for [L impls] (satisfiable? L K)))))

