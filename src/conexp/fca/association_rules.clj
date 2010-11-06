;; Copyright (c) Daniel Borchmann. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.fca.association-rules
  (:use conexp.base
        conexp.fca.contexts
        conexp.fca.implications))

;;;

(deftype Association-Rule [premise conclusion support confidence]
  Object
  (equals [this other]
    (generic-equals [this other] Association-Rule [premise conclusion support confidence]))
  (hashCode [this]
    (hash-combine-hash Association-Rule premise conclusion support confidence)))

(defmethod premise Association-Rule [^Association-Rule ar]
  (.premise ar))

(defmethod conclusion Association-Rule [^Association-Rule ar]
  (.conclusion ar))

(defn support
  "Computes the support of the set of attributes B in context ctx. If
  an association rule is given, returns the support of the association
  rule in its context."
  ([set ctx]
     (if (empty? (objects ctx))
       1
       (/ (count (attribute-derivation ctx set))
          (count (objects ctx)))))
  ([^Association-Rule ar]
     (.support ar)))

(defn confidence
  "Computes the confidence of the association rule ar."
  ([premise conclusion context]
     (let [premise-count (count (attribute-derivation context premise))]
       (if (zero? premise-count)
         1
         (/ (count (attribute-derivation context
                                         (union (set premise) (set conclusion))))
            premise-count))))
  ([^Association-Rule ar]
     (.confidence ar)))

(defmethod print-method Association-Rule [ar out]
  (.write out
          (str "( " (premise ar) " ==> " (conclusion ar)
               "; support " (support ar)
               ", confidence " (confidence ar) " )")))

;;;

(defn make-association-rule-nc
  "Constructs an association rule from the given parameters, without
  checking for anything. Use with care."
  ([context premise conclusion]
     (let [premise    (set premise),
           conclusion (set conclusion),
           supp       (support (union premise conclusion) context),
           conf       (confidence premise conclusion context)]
       (make-association-rule-nc premise conclusion supp conf)))
  ([premise conclusion support confidence]
     (let [premise    (set premise),
           conclusion (difference (set conclusion) premise)]
       (Association-Rule. premise conclusion support confidence))))

(defn make-association-rule
  "Constructs an association rule for context form premise and
  conclusion. Note that if support turns out to be 0, the confidence
  of the returned association rule will be 1."
  ([context premise conclusion]
     (let [premise    (set premise)
           conclusion (set conclusion)]
       (when-not (and (subset? premise (attributes context))
                      (subset? conclusion (attributes context)))
         (illegal-argument "Premise and conclusion sets must be subsets "
                           "of the attributes of the given context when constructing an "
                           "association rule."))
       (make-association-rule-nc context premise conclusion)))
  ([premise conclusion support confidence]
     (when-not (and (<= 0 support 1)
                    (<= 0 confidence 1))
       (illegal-argument "Support and confidence must be numbers between 0 and 1."))
     (when-not (and (coll? premise) (coll? conclusion))
       (illegal-argument "Premise and conclusion must be collections."))
     (make-association-rule-nc premise conclusion support confidence)))

;;;

(defn iceberg-intent-seq
  "Computes in context for given minimal support minsupp the
  corresponding iceberg intent seq (i.e. the iceberg-lattice)."
  [context minsupp]
  (let [mincount (* minsupp (count (objects context)))]
    (all-closed-sets-in-family (fn [intent]
                                 (>= (count (attribute-derivation context intent))
                                     mincount))
                               (attributes context)
                               (partial context-attribute-closure context))))

(defn luxenburger-basis
  "Computes the luxenburger-basis for context with minimal support
  minsupp and minimal confidence minconf."
  [context minsupp minconf]
  (let [closed-intents (iceberg-intent-seq context minsupp)]
    (for [[B_1, B_2] (transitive-reduction closed-intents proper-subset?)
          :let [ar (make-association-rule-nc context B_1 B_2)]
          :when (>= (confidence ar) minconf)]
      ar)))

;;;

nil
