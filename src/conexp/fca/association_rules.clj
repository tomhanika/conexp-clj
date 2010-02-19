;; Copyright (c) Daniel Borchmann. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.fca.association-rules
  (:gen-class
   :name conexp.fca.AssociationRule
   :prefix "AssociationRule-"
   :init init
   :constructors { [ Object Object Object ] [] }
   :state state)
  (:use conexp.base
	conexp.fca.contexts
	conexp.fca.implications))

;;;

(defn AssociationRule-init [context premise conclusion]
  [ [] {:context context
	:premise premise
	:conclusion conclusion} ])

(defn AssociationRule-hashCode
  "Computes hash code for an association rule object."
  [this]
  (reduce bit-xor 0
	  [(hash ((.state this) :premise)),
	   (hash ((.state this) :conclusion)),
	   (hash ((.state this) :context))]))

(defmethod premise conexp.fca.AssociationRule [ar]
  ((.state ar) :premise))

(defmethod conclusion conexp.fca.AssociationRule [ar]
  ((.state ar) :conclusion))

(defn context [ar]
  ((.state ar) :context))

(defn support [B ctx]
  (/ (count (attribute-derivation ctx B))
     (count (objects ctx))))

(defn confidence [ar]
  (/ (support (union (premise ar) (conclusion ar))
	      (context ar))
     (support (premise ar)
	      (context ar))))

(defn AssociationRule-toString [this]
  (str "( " (premise this) " ==> " (conclusion this)
       "; support " (support (union (premise this)
				    (conclusion this))
			     (context this))
       ", confidence " (confidence this) " )"))

(defn AssociationRule-equals [this other]
  (and (instance? conexp.fca.AssociationRule other)
       (= (premise this) (premise other))
       (= (conclusion this) (conclusion other))))

;;;

(defn make-association-rule [context premise conclusion]
  (let [premise (set premise)
	conclusion (set conclusion)]
    (conexp.fca.AssociationRule. context premise (difference conclusion premise))))

(defn iceberg-intent-set
  [context minsupp]
  (let [mincount (round (ceil (* minsupp (count (objects context)))))]
    (all-closed-sets-in-family (fn [intent]
				 (>= (count (attribute-derivation context intent))
				     mincount))
			       (attributes context)
			       (partial context-attribute-closure context))))

(defn luxenburger-basis
  [context minsupp minconf]
  (let [closed-intents (iceberg-intent-set context minsupp)]
    (for [B_1 closed-intents
	  B_2 closed-intents
	  :when (and (proper-subset? B_1 B_2)
		     ; directly neighbored in iceberg concept set
		     (forall [x (difference B_2 B_1)]
		       (= B_2 (context-attribute-closure context (conj B_1 x)))))
	  :let [ar (make-association-rule context B_1 B_2)]
	  :when (>= (confidence ar) minconf)]
      ar)))

;;;

nil
