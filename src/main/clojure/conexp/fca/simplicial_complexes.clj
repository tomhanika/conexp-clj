;; Copyright ⓒ the conexp-clj developers; all rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.fca.simplicial-complexes
  "Provides the implementation of simplicial complexes and functions on them."
  (:require [conexp.base :refer [clojure-coll
                                 clojure-set
                                 clojure-type
                                 difference
                                 hash-combine-hash
                                 illegal-argument
                                 subset?
                                 union]]
            [conexp.fca.lattices :refer [concept-lattice
                                         lattice-base-set
                                         lattice-order
                                         sup]]
            [conexp.fca.next-closure :refer [t-simplex-pseudo-intents]])
  (:import conexp.fca.contexts.Formal-Context
           conexp.fca.lattices.Lattice))

;;; Data structure

(defprotocol SimplicialComplex
  (base-set [this] "Returns the base set.")
  (simplices [this] "Returns the simplices of the simplicial complex, 
                     which are subsets of its base_set."))

(deftype FullSimplicialComplex [base-set simplices]
  Object
  (equals [this other]
    (and (= (class this) (class other))
         (= (.base-set this) (.base-set ^FullSimplicialComplex other))
         (= (set (.simplices this)) (set (.simplices ^FullSimplicialComplex other)))))
  (hashCode [this]
    (hash-combine-hash FullSimplicialComplex base-set simplices))
  (toString [this]
    (str (set (.simplices this))))
  ;;
  SimplicialComplex
  (base-set [this] base-set)
  (simplices [this] simplices))

(defmethod print-method FullSimplicialComplex [^FullSimplicialComplex simplicial-complex,
                                               ^java.io.Writer out]
  (.write out
          ^String (str simplicial-complex)))

;;; Constructors

(defmulti make-full-simplicial-complex-nc
  "Creates a full simplicial complex from the given arguments, without any checks."
  {:arglist '([simplices]
              [base-set simplices])}
  (fn [& args] (vec (map clojure-type args))))

(defmethod make-full-simplicial-complex-nc [clojure-set clojure-coll] [base-set simplices]
  (FullSimplicialComplex. base-set simplices))

(defmethod make-full-simplicial-complex-nc [clojure-coll] [simplices]
  (let [base-set (apply union simplices)]
    (make-full-simplicial-complex-nc base-set simplices)))

(defmethod make-full-simplicial-complex-nc :default [& args]
  (illegal-argument "The arguments " args " are not valid for a Full Simplicial Complex."))

(defn is-simplicial-complex?
  "Check if given object follows the definition of a simplicial complex."
  [simplicial-complex]
  (and
   (let [simplices-set (set (.simplices simplicial-complex))]
     (every? (fn [simplex]
               (if (not= simplex #{})
                 (let [direct-sub-simplices 
                       (map #(difference (set simplex) #{%}) simplex)]
                   (every? #(contains? simplices-set %) direct-sub-simplices))
                 true))
             simplices-set))
   ;; also check if base-set is correct (contains all elements contained in simplices)
   (let [base-set-derived-from-simplices (apply union (.simplices simplicial-complex))]
     (subset? base-set-derived-from-simplices (.base-set simplicial-complex)))))

(defmulti make-full-simplicial-complex
  "Creates a full simplicial complex from the given arguments, and checks that it really is a full simplicial complex."
  {:arglist '([simplices]
              [base-set simplices])}
  (fn [& args] (vec (map clojure-type args))))

(defmethod make-full-simplicial-complex [clojure-set clojure-coll] [base-set simplices]
  (let [simplicial-complex (make-full-simplicial-complex-nc base-set simplices)]
    (when-not (is-simplicial-complex? simplicial-complex)
      (illegal-argument "Given arguments do not describe a simplicial complex."))
    simplicial-complex))

(defmethod make-full-simplicial-complex [clojure-coll] [simplices]
  (let [base-set (apply union simplices)]
    (make-full-simplicial-complex base-set simplices)))

;; FCA

(defmulti t-simplex
  "Creates a t-simplex from a given object."
  (fn [object t] (type object)))

(defmethod t-simplex Lattice
  [lattice t]
  (let [not>= (fn [concept]
                (not ((lattice-order lattice) t concept))),
        one-element-simplices (set (filter not>= (lattice-base-set lattice))),
        queue (mapv #(set (list %)) one-element-simplices),
        simplices (set (union #{#{}} queue)),
        join (sup lattice)]
    (loop [element (first queue)
           queue (vec (next queue))
           simplices simplices]
      (let [join-elements (filter #(contains? one-element-simplices
                                              (join (first element) (first %))) queue),
            new-queue-elements (mapv #(union element %) join-elements),
            new-queue (vec (distinct (apply conj queue new-queue-elements)))]
        (if (empty? new-queue)
          (FullSimplicialComplex. (lattice-base-set lattice) simplices)
          (recur (first new-queue)
                 (vec (next new-queue))
                 (union simplices new-queue-elements)))))))

(defmethod t-simplex Formal-Context
  [ctx t]
  (let [lattice (concept-lattice ctx)]
    (t-simplex lattice t)))

(defmethod t-simplex :default
  [object & args]
  (illegal-argument "Cannot compute a simplicial complex from type " (type object) "."))

(defmulti t-simplex-next-closure
  "Creates a t-simplex from a given object with next closure algorithm."
  (fn [object t] (type object)))

(defmethod t-simplex-next-closure Lattice
  [lattice t]
  (FullSimplicialComplex. (lattice-base-set lattice) 
                          (second (t-simplex-pseudo-intents lattice t))))

(defmethod t-simplex-next-closure Formal-Context
  [ctx t]
  (let [lattice (concept-lattice ctx)]
    (t-simplex-next-closure lattice t)))

(defmethod t-simplex-next-closure :default
  [object & args]
  (illegal-argument "Cannot compute a simplicial complex from type " (type object) "."))
