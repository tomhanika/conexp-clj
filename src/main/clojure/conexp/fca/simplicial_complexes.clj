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
                                 hash-combine-hash
                                 illegal-argument
                                 union]]))

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

(comment (defn is-simplicial-complex?
           "Check if given object follows the definition of a simplicial complex.
  TODO: write definition"
           [complex]))

(comment (defn make-full-simplicial-complex
           "Create a simplicial complex from 
  TODO: whatever the input value is."
           [& args]
           (let [simplicial-complex (apply make-simplicial-complex-nc args)]
             (when-not (is-simplicial-complex? simplicial-complex)
               (illegal-argument "Given arguments do not describe a simplicial complex."))
             simplicial-complex)))
