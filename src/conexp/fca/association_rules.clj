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

(defn AssociationRule-init [context premise conclusion]
  [ [] {:context context
	:premise premise
	:conclusion conclusion} ])

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

(defn make-association-rule [context premise conclusion]
  (let [premise (set premise)
	conclusion (set conclusion)]
    (conexp.fca.AssociationRule. context premise (difference conclusion premise))))

(defn iceberg-concept-set
  ; change
  [context minsupp]
  (filter (fn [concept]
	    (>= (support (second concept) context) minsupp))
	  (concepts context)))

(defn luxenburger-basis
  ; change
  [context minsupp minconf]
  (let [closed-intents (map second (iceberg-concept-set context minsupp))]
    (for [B_1 closed-intents
	  B_2 closed-intents
	  :when (and (proper-subset? B_1 B_2)
		     ; directly neighbored in iceberg concept set
		     (exists [x (difference B_2 B_1)]
		       (= B_2 (context-attribute-closure context (conj B_1 x)))))
	  :let [ar (make-association-rule context B_1 B_2)]
	  :when (>= (confidence ar) minconf)]
      ar)))

nil
