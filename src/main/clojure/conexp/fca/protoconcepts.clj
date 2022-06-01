;; Copyright ⓒ the conexp-clj developers; all rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.fca.protoconcepts
  "Basis datastructure and definitions for protoconcepts.
  DOI:https://doi.org/10.1007/11528784_2"
  (:require [conexp.base :refer :all]
            [conexp.fca.contexts :refer :all]))

(deftype Protoconcept [base-set]
  Object
  (equals [this other]
    (and (= (class this) (class other))
         (= (.base-set this) (.base-set ^Protoconcept other))))
  (hashCode [this]
    (hash-combine-hash Protoconcept base-set)))

(defn preconcept?
  "Tests whether given pair is preconcept in given context ctx."
  [ctx [set-of-obj set-of-att]]
  (let [att-derivation (attribute-derivation ctx set-of-att)
        obj-derivation (object-derivation ctx set-of-obj)]
    (or (clojure.set/subset? set-of-obj att-derivation)
        (clojure.set/subset? set-of-att obj-derivation))))

(defn protoconcept?
  "Tests whether given pair is protoconcept in given context ctx."
  [ctx [set-of-obj set-of-att]]
  (let [att-derivation (attribute-derivation ctx set-of-att)
        obj-derivation (object-derivation ctx set-of-obj)]
    (and (preconcept? ctx [set-of-obj set-of-att]))
         (concept? ctx [att-derivation obj-derivation])))

;; protoconcept
;; protoconcept-lattice
;; order-function

;; TODO: protoconcepts (create protoconcepts)
;; TODO: make-protoconcept-lattice or make-protoconcept-order
