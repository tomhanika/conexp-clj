(ns conexp.fca.non-monotonic-contexts
  (:require [conexp.base :refer :all]
            [conexp.fca.posets :refer :all]
            [conexp.fca.contexts :refer :all]
            [clojure.set :as set]))

(defn relation-set [base-set order]
  "Produces an explicit set representation of the relation defined by the supplied order."
  (if (coll? order) order
                    (into #{} (filter #(order (first %) (second %)) (for [x base-set y base-set] [x y]))))
)

(defn relation-function [base-set order]
  "Produces a membership function of the relation defined by the supplied set."
  (if (fn? order) order
                  #(.contains order [%1 %2]))
)

(defprotocol Extended-Context

  (context [this] "Returns the underlying context.")

  (object-order [this] "Returns the partial order on the context's objects.")
  (attribute-order [this] "Returns the partial order on the context's attributes.")
  (object-order-explicit [this] "Returns an explicit set representation of the object order.")
  (attribute-order-explicit [this] "Returns an explicit set representation of the attribute order order.")
)

(deftype extended-context [ctx object-order attribute-order]

  Object
  (equals [this other]
    (generic-equals [this other] extended-context [ctx object-order attribute-order]))
  (hashCode [this]
    (hash-combine-hash extended-context ctx object-order attribute-order))

  Context
  (objects [this] (objects ctx))
  (attributes [this] (attributes ctx))
  (incidence [this] (incidence ctx))

  Extended-Context
  (context [this] ctx)
  (object-order [this] object-order)
  (attribute-order [this] attribute-order)
  (object-order-explicit [this] (relation-set (objects this) object-order))
  (attribute-order-explicit [this] (relation-set (attributes this) attribute-order))
)

(defmethod print-method extended-context [ectx out]
  (.write ^java.io.Writer out
          ^String (context-to-string ectx))
)


(defn make-extended-context 
  ([objs attrs incidence obj-order attr-order]
   "Creates Extended Formal Context from Sets of Objects and Attributes, the Incidence Relation
    and a Partial Order on the Objects and on the Attributes."
   (make-extended-context (make-context objs attrs incidence) obj-order attr-order)
   )
  ([ctx obj-order attr-order]
   "Creates Extended Formal Context from a Pre-existing Formal Context Partial Order 
   on the Objects and on the Attributes. Empty Collections and Values of nil are Interpreted
   as the Reflexive, but Otherwise Empty Order Relation."
   (let [objs (objects ctx)
         attrs (attributes ctx)
         obj-order (if (or (nil? obj-order) 
                           (and (coll? obj-order) 
                                (empty? obj-order))) = obj-order);add reflexivity of order is empty
         attr-order (if (or (nil? attr-order) 
                            (and (coll? attr-order) 
                                 (empty? attr-order))) = attr-order);add reflexivity of order is empty
         obj-poset (make-poset objs (relation-function objs obj-order));convert to poset to verify ordering
         attr-poset (make-poset attrs (relation-function attrs  attr-order))];convert to poset to verify ordering

        (extended-context. ctx (relation-function objs obj-order) (relation-function attrs attr-order)))
   )
)

(defn minimized-object-derivation [ectx objs]
  "Computes the Minimized Object Derivation of the Supplied Set of Objects on the 
   Supplied Extended Formal Context."
  (let [attr-order (attribute-order ectx)
        derivation (object-derivation ectx objs)
        minimal (fn [attr] (not-any? #(attr-order % attr) (disj derivation attr)))]
    (into #{} (filter minimal derivation)))
)

(defn minimized-attribute-derivation [ectx attrs]
  "Computes the Minimized Attribute Derivation of the Supplied Set of Attributes on the 
   Supplied Extended Formal Context."
  (let [obj-order (object-order ectx)
        derivation (attribute-derivation ectx attrs)
        minimal (fn [obj] (not-any? #(obj-order % obj) (disj derivation obj)))]
    (into #{} (filter minimal derivation)))
)

