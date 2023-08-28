;; Copyright ⓒ the conexp-clj developers; all rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.fca.simplicial-complexes
  "Provides the implementation of simplicial complexes and functions on them.")

(defrecord Simplicial-Complex [base-set subsets])

(defn make-simplicial-complex-nc
  "Create a simplicial complex from
  TODO: whatever the input value is
  without checks.")

(defn is-simplicial-complex?
  "Check if given object follows the definition of a simplicial complex.
  TODO: write definition"
  [complex])

(defn make-simplicial-complex
  "Create a simplicial complex from 
  TODO: whatever the input value is."
  [& args]
  (let [simplicial-complex (apply make-simplicial-complex-nc args)]
    (when-not (is-simplicial-complex? simplicial-complex)
      (illegal-argument "Given arguments do not describe a simplicial complex."))
    simplicial-complex))
