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
            [conexp.fca.lattices :refer :all]
            [conexp.fca.posets :refer :all]))

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

(defmethod print-method Protoconcepts [^Protoconcepts protoconcepts, ^java.io.Writer out]
  (.write out
          ^String (str "Protoconcepts on " (count (base-set protoconcepts)) " elements.")))

(defn protoconcept?
  "Tests whether given pair is protoconcept in given context ctx."
  [ctx [set-of-obj set-of-att]]
  (or (= (object-derivation ctx set-of-obj)
         (context-attribute-closure ctx set-of-att))
      (= (context-object-closure ctx set-of-obj)
         (attribute-derivation ctx set-of-att))))

(defn protoconcepts?
  "Tests whether given tuples all are protoconcepts in given context ctx."
  [ctx protoconcepts]
  (every? (partial protoconcept? ctx) protoconcepts))

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

(defn make-protoconcepts
  "Creates a new protoconcepts order from the given base-set and order-function.
  Checks if the result has partial order, which may take some time. If you don't want this, use make-protoconcepts-nc."
  ([base-set order-function]
   (let [protoconcepts (make-protoconcepts-nc base-set order-function)]
     (when-not (has-partial-order? protoconcepts)
       (illegal-argument "Given arguments do not describe a partial order."))
     protoconcepts))
  ;; checks if result only contains protoconcepts of context ctx
  ([base-set order-function ctx]
   (let [protoconcepts (make-protoconcepts base-set order-function)]
     (when-not (protoconcepts? ctx (base-set protoconcepts))
       (illegal-argument "Given base-set and order-function do not describe protoconcepts in given context."))
     protoconcepts)))

(defn protoconcepts-order
  "Returns for a given context ctx its protoconcepts with order."
  [ctx]
  (make-protoconcepts-nc (protoconcepts ctx)
                         (fn <= [[A B] [C D]]
                           (and (subset? A C)
                                (subset? D B)))))
