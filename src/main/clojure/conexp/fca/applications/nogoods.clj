(ns conexp.fca.applications.nogoods
  (:require [conexp.fca.contexts :refer [attributes objects
                                         make-context incidence-relation]]
            [conexp.base :refer :all]
            [clojure.math.combinatorics :as combo]))

(defn truth-values [n]
  (let [step (/ 1 (- n 1))
        end (+ 1 step)]
    (set (range 0 end step))))

(defn- variable-name [n]
  (keyword (format "x%d" n)))

(defn- to-assignment
  [idx assignment]
  {(variable-name idx) assignment})

(defn interpretations [n l]
  (set (map #(apply union (map-indexed to-assignment %))
            (combo/selections (truth-values n) l))))

(defn partial-interpretations [n l]
  (set (map #(apply sorted-map (apply union (set %))) (apply union
              (map #(combo/subsets (seq %)) (interpretations n l))))))

(defn domain
  "return the domain of a partial interpretation"
  [pint]
  (set (keys pint)))

(defn combine
  "combine the given partial interpretations"
  [& pints]
  (set (apply union pints)))

(defn total-interpretation?
  "is the partial interpretation total for n variables?"
  [n pint]
  (subset?
   (map variable-name (truth-values n))
   (domain pint)))

(defstruct formula :variables :semantics)
(def conjunction (struct formula 2 #(let [{x :x0, y :x1} %] (min x y))))
(def disjunction (struct formula 2 #(let [{x :x0, y :x1} %] (max x y))))

(defn totalise
  [arity pint]
  (combine pint (map-indexed hash-map (repeat arity 0))))

(defn evaluate
  "evaluate formula f on partial interpretation pint"
  [f pint]
  (let [arity (:variables f)]
    (when-not (total-interpretation? arity pint)
      (throw (Exception. "can only evaluate over total interpretations")))
    ((:semantics f) (into (sorted-map) pint))))

(defn truth-table
  "compute a truth table for n truth values of formula f"
  [n f]
  (let [arity (:variables f)]
    (map (fn [int]
          `{:interpretation ~int
            :value ~(evaluate f int)})
    (interpretations n arity))))

(defn models?
  "is pint a model for formula f?"
  [pint f]
  (= 1 (evaluate f pint)))

(defn disjoint-domains?
  "are g and m defined on disjoint domains?"
  [g m]
  (let [dom-g (domain g)
        dom-m (domain m)]
    (empty? (intersection dom-g dom-m))))

(defn completion?
  "is m a completion for g and formula f?"
  [g m f]
  (let [arity (:variables f)
        pint (combine g m)]
    (and (disjoint-domains? g m)
         (total-interpretation? arity pint)
         (models? pint f))))

(defn incidence-for-formula
  "incidence relation for n truth values and formula f"
  [n f]
  (let [arity (:variables f)
        pis (partial-interpretations n arity)]
    (set (for [g pis
               m pis
               :when (and (disjoint-domains? g m)
                      (total-interpretation? arity (combine g m))
                      (not (completion? g m f)))]
          (list g m)))))

(defn context-for-formula
  [n f]
  (let [arity (:variables f)
        pis (partial-interpretations n arity)]
    (make-context pis pis (incidence-for-formula n f))))
