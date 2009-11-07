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
  "Creates an implication (premise => conclusion \\setminus premise)."
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

(defn add-immediate-elements
  [implications old-set]
  (let [conclusions-to-add (for [impl implications
				 :when (and (subset? (premise impl) old-set)
					    (exists [x (conclusion impl)] (not (old-set x))))]
			     (conclusion impl))]
    (apply union old-set conclusions-to-add)))

(defn close-under-implications [implications set]
  (loop [set set]
    (let [new (add-immediate-elements implications set)]
      (if (= new set)
	new
	(recur new)))))

(defn clop-by-implications [implications]
  (partial close-under-implications implications))

(defn follows-semantically? [implication implications]
  (subset? (conclusion implication) (close-under-implications implications (premise implication))))

(defn add-immediate-elements*
  [implications old-set]
  (let [conclusions-to-add (for [impl implications
				 :when (and (proper-subset? (premise impl) old-set)
					    (exists [x (conclusion impl)] (not (old-set x))))]
			     (conclusion impl))]
    (apply union old-set conclusions-to-add)))

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
