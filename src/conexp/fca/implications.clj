(ns conexp.fca.implications
  (:gen-class
   :name conexp.fca.Implication
   :prefix "Implication-"
   :init init
   :constructors { [ Object Object ] [] }
   :state state)
  (:use conexp.base
	conexp.fca.contexts))

(defn Implication-init [premise conclusion]
  [ [] {:premise premise :conclusion conclusion} ])

(defn premise [#^conexp.fca.Implication impl]
  ((.state impl) :premise))

(defn conclusion [#^conexp.fca.Implication impl]
  ((.state impl) :conclusion))

(defn Implication-toString [this]
  (str "( " (premise this) "  ==>  " (conclusion this) " )"))

(defn Implication-equals [this other]
  (and (instance? conexp.fca.Implication other)
       (= (premise this) (premise other))
       (= (conclusion this) (conclusion other))))

(defn make-implication
  "Creates an implication (premise => conclusion $\\setminus$ premise)."
  [premise conclusion]
  (let [premise (set premise)
	conclusion (set conclusion)]
    (conexp.fca.Implication. premise (difference conclusion premise))))

(defn respects? [set impl]
  (or (not (subset? (premise impl) set))
      (subset? (conclusion impl) set)))

(defn holds? [impl ctx]
  (forall [intent (context-intents ctx)]
	  (respects? intent impl)))

(defn add-immediate-elements [implications initial-set]
  (loop [conclusions []
	 impls implications
	 unused-impls []]
    (if (empty? impls)
      [(apply union initial-set conclusions) unused-impls]
      (let [impl (first impls)]
	(if (and (subset? (premise impl) initial-set)
		 (not (subset? (conclusion impl) initial-set)))
	  (recur (conj conclusions (conclusion impl))
		 (rest impls)
		 unused-impls)
	  (recur conclusions
		 (rest impls)
		 (conj unused-impls impl)))))))

(defn close-under-implications [implications set]
  (loop [set set
	 impls implications]
    (let [[new impls] (add-immediate-elements impls set)]
      (if (= new set)
	new
	(recur new impls)))))

(defn clop-by-implications [implications]
  (partial close-under-implications implications))

(defn follows-semantically? [implication implications]
  (subset? (conclusion implication)
	   (close-under-implications implications (premise implication))))

(defn add-immediate-elements* [implications initial-set]
  (loop [conclusions []
	 impls implications
	 unused-impls []]
    (if (empty? impls)
      [(apply union initial-set conclusions) unused-impls]
      (let [impl (first impls)]
	(if (and (proper-subset? (premise impl) initial-set)
		 (not (subset? (conclusion impl) initial-set)))
	  (recur (conj conclusions (conclusion impl))
		 (rest impls)
		 unused-impls)
	  (recur conclusions
		 (rest impls)
		 (conj unused-impls impl)))))))

(defn clop-by-implications* [implications]
  (binding [add-immediate-elements add-immediate-elements*]
    (partial close-under-implications implications)))

(defn stem-base [ctx]
  (let [double-prime (partial context-attribute-closure ctx)
	attributes   (attributes ctx)]
    (loop [implications #{}
	   last         #{}]
      (let [conclusion-from-last (double-prime last)
	    implications (if (not= last conclusion-from-last)
			   (conj implications (make-implication last conclusion-from-last))
			   implications)
	    clop (clop-by-implications* implications)
	    next (next-closed-set attributes clop last)]
	(if next
	  (recur implications next)
	  implications)))))

nil
