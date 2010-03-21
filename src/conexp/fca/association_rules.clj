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

(deftype Association-Rule [context premise conclusion])

(defmethod premise ::Association-Rule [ar]
  (:premise ar))

(defmethod conclusion ::Association-Rule [ar]
  (:conclusion ar))

(defn context [ar]
  (:context ar))

(defn support [B ctx]
  (/ (count (attribute-derivation ctx B))
     (count (objects ctx))))

(defn confidence [ar]
  (/ (support (union (premise ar) (conclusion ar))
	      (context ar))
     (support (premise ar)
	      (context ar))))

(defmethod print-method  ::Association-Rule [ar out]
  (.write out
	  (str "( " (premise ar) " ==> " (conclusion ar)
	       "; support " (support (union (premise ar)
					    (conclusion ar))
				     (context ar))
	       ", confidence " (confidence ar) " )")))

;;;

(defn make-association-rule [context premise conclusion]
  (let [premise (set premise)
	conclusion (set conclusion)]
    (Association-Rule context premise (difference conclusion premise))))

;;;

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
