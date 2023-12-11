;; Copyright ⓒ the conexp-clj developers; all rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.fca.simplicial-complexes
  "Provides the implementation of simplicial complexes and functions on them."
  (:require [conexp.base :refer :all]
            [conexp.fca.closure-systems :refer [next-closed-set-in-family]]
            [conexp.fca.contexts :refer [extents
                                         objects]]
            [conexp.fca.implications :refer [clop-by-implications
                                             close-under-implications
                                             implication?
                                             make-implication]]
            [conexp.fca.lattices :refer [concept-lattice
                                         inf
                                         lattice-base-set
                                         lattice-order
                                         sup]]
            [conexp.fca.ordinal-motifs :refer [generate-scale
                                               identify-full-scale-measures]])
  (:import conexp.fca.contexts.Formal-Context
           conexp.fca.lattices.Lattice))

;;; Data structure

(defprotocol SimplicialComplex
  (base [this] "Returns the base set.")
  (simplices [this] "Returns the simplices of the simplicial complex, 
                     which are subsets of its base set."))

(deftype FullSimplicialComplex [base simplices]
  Object
  (equals [this other]
    (and (= (class this) (class other))
         (= (.base this) (.base ^FullSimplicialComplex other))
         (= (set (.simplices this)) (set (.simplices ^FullSimplicialComplex other)))))
  (hashCode [this]
    (hash-combine-hash FullSimplicialComplex base simplices))
  (toString [this]
    (str (set (.simplices this))))
  ;;
  SimplicialComplex
  (base [this] base)
  (simplices [this] simplices))

(defmethod print-method FullSimplicialComplex [^FullSimplicialComplex simplicial-complex,
                                               ^java.io.Writer out]
  (.write out
          ^String (str simplicial-complex)))

;;; Constructors

(defmulti make-full-simplicial-complex-nc
  "Creates a full simplicial complex from the given arguments, without any checks."
  {:arglist '([simplices]
              [base simplices])}
  (fn [& args] (vec (map clojure-type args))))

(defmethod make-full-simplicial-complex-nc [clojure-set clojure-coll] [base simplices]
  (FullSimplicialComplex. base simplices))

(defmethod make-full-simplicial-complex-nc [clojure-coll] [simplices]
  (let [base (apply union simplices)]
    (make-full-simplicial-complex-nc base simplices)))

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
   ;; also check if base set is correct (contains all elements contained in simplices)
   (let [base-set-derived-from-simplices (apply union (.simplices simplicial-complex))]
     (subset? base-set-derived-from-simplices (.base simplicial-complex)))))

(defmulti make-full-simplicial-complex
  "Creates a full simplicial complex from the given arguments, and checks that it really is a full simplicial complex."
  {:arglist '([simplices]
              [base simplices])}
  (fn [& args] (vec (map clojure-type args))))

(defmethod make-full-simplicial-complex [clojure-set clojure-coll] [base simplices]
  (let [simplicial-complex (make-full-simplicial-complex-nc base simplices)]
    (when-not (is-simplicial-complex? simplicial-complex)
      (illegal-argument "Given arguments do not describe a simplicial complex."))
    simplicial-complex))

(defmethod make-full-simplicial-complex [clojure-coll] [simplices]
  (let [base (apply union simplices)]
    (make-full-simplicial-complex base simplices)))

;; FCA

(defn simplicial-complex-from-clop
  "Given a closure operator «clop» on the set «base», computes its canonical base,
   optionally using the set «background-knowledge» of implications on «base-set»
  as background knowledge.  The result will be a lazy sequence.  If «predicate»
  is given as third argument, computes only those implications whose premise
  satisfy this predicate.  Note that «predicate» has to satisfy the same
  conditions as the one of «next-closed-set-in-family»."
  ([clop base]
     (simplicial-complex-from-clop clop base #{} (constantly true)))
  ([clop base background-knowledge]
     (simplicial-complex-from-clop clop base background-knowledge (constantly true)))
  ([clop base background-knowledge predicate]
     (assert (fn? clop)
             "Given closure operator must be a function")
     (assert (coll? base)
             "Base must be a collection")
     (assert (fn? predicate)
             "Predicate must be a function")
     (assert (and (set? background-knowledge)
                  (forall [x background-knowledge]
                    (implication? x)))
             "Background knowledge must be a set of implications")
     (let [next-closure (fn [implications last]
                          (next-closed-set-in-family predicate
                                                     base
                                                     (clop-by-implications implications)
                                                     last)),
           runner       (fn runner [implications simplicial-complex candidate]
                          (when candidate
                            (if (not (clop candidate))
                              (let [impl  (make-implication candidate base),
                                    impls (conj implications impl)]
                                (recur impls simplicial-complex (next-closure impls candidate)))
                              (let [s-complex (conj simplicial-complex candidate)]
                                (cons candidate
                                      (lazy-seq (runner implications
                                                        s-complex
                                                        (next-closure implications candidate))))))))]
       (lazy-seq (runner background-knowledge
                         #{}
                         (close-under-implications background-knowledge #{}))))))

;;

(defn- join-operator
  "Join operator of a given lattice for an input set of arbitrary length."
  [lattice]
  (fn [concept-set]
    (if (= 0 (count concept-set))
      ;; bottom element
      (reduce (inf lattice) (lattice-base-set lattice))
      (if (= 1 (count concept-set))
        (first concept-set)
        (reduce (sup lattice) concept-set)))))

(defn- not>=t-operator
  [lattice t]
  (fn [concept]
    (not ((lattice-order lattice) t concept))))

(defn- t-simplex-operator
  [lattice t]
  (fn [concept-set]
    ((not>=t-operator lattice t) ((join-operator lattice) concept-set))))

(defmulti t-simplex-next-closure
  "Creates a t-simplex from a given object with next closure algorithm."
  (fn [object t] (type object)))

(defmethod t-simplex-next-closure Lattice
  [lattice t]
  (let [base (lattice-base-set lattice)
        closure-condition (t-simplex-operator lattice t)
        simplices (simplicial-complex-from-clop closure-condition base)]
    (FullSimplicialComplex. base simplices)))

(defmethod t-simplex-next-closure Formal-Context
  [ctx t]
  (let [lattice (concept-lattice ctx)]
    (t-simplex-next-closure lattice t)))

(defmethod t-simplex-next-closure :default
  [object & args]
  (illegal-argument "Cannot compute a simplicial complex from type " (type object) "."))

;;

(defn- closure-condition-operator
  [context scale-type]
  (let [context-extents (extents context)]
    (fn [object-set]
      (let [subset-size (count object-set)]
        (if (< subset-size 2)
          true
          (let [scale-extents (extents (generate-scale scale-type subset-size))]
            (identify-full-scale-measures 
             scale-type context-extents object-set scale-extents)))))))

(defn- compute-ordinal-motifs-next-closure
  [ctx scale-type]
  (let [base (objects ctx)
        closure-condition (closure-condition-operator ctx scale-type)
        simplices (simplicial-complex-from-clop closure-condition base)]
    (FullSimplicialComplex. base
                            simplices)))

(defmulti ordinal-motif-next-closure
  "Creates ordinal motifs from a given context and scale-type with next closure algorithm."
  (fn [context scale-type] scale-type))

(defmethod ordinal-motif-next-closure :ordinal
  [ctx scale-type]
  (compute-ordinal-motifs-next-closure ctx scale-type))

(defmethod ordinal-motif-next-closure :interordinal
  [ctx scale-type]
  (compute-ordinal-motifs-next-closure ctx scale-type))

(defmethod ordinal-motif-next-closure :nominal
  [ctx scale-type]
  (compute-ordinal-motifs-next-closure ctx scale-type))

(defmethod ordinal-motif-next-closure :contranominal
  [ctx scale-type]
  (compute-ordinal-motifs-next-closure ctx scale-type))

(defmethod ordinal-motif-next-closure :crown
  [ctx scale-type]
  (compute-ordinal-motifs-next-closure ctx scale-type))

(defmethod ordinal-motif-next-closure :default
  [ctx scale-type & args]
  (illegal-argument "Cannot compute ordinal motifs for scale " scale-type "."))
