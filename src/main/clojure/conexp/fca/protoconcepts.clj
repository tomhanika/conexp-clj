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
            [conexp.math.algebra :refer :all]
            [conexp.fca.contexts :refer :all]
            [conexp.fca.lattices :refer :all]))

(deftype Protoconcepts [base-set order-function]
  Object
  (equals [this other]
    (and (= (class this) (class other))
         (= (.base-set this) (.base-set ^Protoconcepts other))
         (let [order-this (.order this),
               order-other (.order other)]
           (or (= order-this order-other)
               (forall [x (.base-set this)
                        y (.base-set this)]
                       (<=> (order-this x y)
                            (order-other x y)))))))
  (hashCode [this]
    (hash-combine-hash Protoconcepts base-set))
  ;;
  Order
  (base-set [this] base-set)
  (order [this]
    (fn order-fn
      ([pair] (order-function (first pair) (second pair)))
      ([x y] (order-function x y)))))

(defmethod print-method Protoconcept [^Protoconcept protoconcept, ^java.io.Writer out]
  (.write out
          ^String (str "Protoconcept on " (count (base-set protoconcept)) " elements.")))

(defn preconcept?
  "Tests whether given pair is preconcept in given context ctx."
  [ctx [set-of-obj set-of-att]]
    ;; equivalent to (subset? set-of-obj (attribute-derivation ctx set-of-att))
    (subset? set-of-att (object-derivation ctx set-of-obj)))

(defn protoconcept?
  "Tests whether given pair is protoconcept in given context ctx."
  [ctx [set-of-obj set-of-att]]
  (or (= (object-derivation ctx set-of-obj)
         (context-attribute-closure ctx set-of-att))
      (= (context-object-closure ctx set-of-obj)
         (attribute-derivation ctx set-of-att))))

(defn semiconcept?
  "Tests whether given pair is a semiconcept in given context ctx."
  [ctx [set-of-obj set-of-att]]
  (or (= (object-derivation ctx set-of-obj) set-of-att)
      (= set-of-obj (attribute-derivation ctx set-of-att))))

(defn protoconcepts
  "Computes all protoconcepts of a context."
  [ctx]
  (let [object-equivalence-classes (group-by #(object-derivation ctx %) (subsets (objects ctx)))
        attribute-equivalence-classes (group-by #(context-attribute-closure ctx %) (subsets (attributes ctx)))]
    (into #{} (apply concat (map (fn [key] 
                                   (for [obj (get object-equivalence-classes key) 
                                         attr (get attribute-equivalence-classes key)] 
                                     [obj attr])) (keys object-equivalence-classes))))))

(defn make-protoconcepts-nc
  "Creates a new protoconcept order from the given base-set and order-function, without any checks."
  [base-set order-function]
  (Protoconcepts. base-set order-function))

(defn protoconcepts-order
  "Returns for a given context ctx its protoconcepts with order."
  [ctx]
  (make-protoconcepts-nc (protoconcepts ctx)
                         (fn <= [[A B] [C D]]
                           (and (subset? A C)
                                (subset? D B)))))
