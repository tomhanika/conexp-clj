(ns conexp.fca.implications
  (:gen-class
   :name conexp.fca.Implication
   :prefix "Implication-"
   :init init
   :constructors { [ clojure.lang.PersistentHashSet clojure.lang.PersistentHashSet ] [] }
   :state state)
  (:use clojure.set
	conexp.util
	conexp.base
	conexp.fca.contexts))

(defn Implication-init [premise conclusion]
  [ [] {:premise premise :conclusion conclusion} ])

(defn premise [impl]
  ((.state impl) :premise))

(defn conclusion [impl]
  ((.state impl) :conclusion))

(defn Implication-toString [this]
  (str "( " (premise this) "  ==>  " (conclusion this) " )"))

(defn Implication-equals [this other]
  (and (instance? conexp.fca.Implication other)
       (= (premise this) (premise other))
       (= (conclusion this) (conclusion other))))

(defn make-implication [premise conclusion]
  (conexp.fca.Implication. (set premise) (set conclusion)))

(defn respects? [set impl]
  (or (not (subset? (premise impl) set))
      (subset? (conclusion impl) set)))

(defn holds? [impl ctx]
  (forall [intent (context-intents ctx)]
	  (respects? intent impl)))

(defn add-immediate-elements
  ([implications set]
     (add-immediate-elements implications set #{}))

  ([implications new-set old-set]
     (if (= new-set old-set)
       new-set
       (let [conclusions-to-add (for [impl implications
				      :when (and (subset? (premise impl) new-set)
						 (exists [x (premise impl)] (not (old-set x))))]
				  (conclusion impl))]
	 (apply union new-set conclusions-to-add)))))

(defn close-under-implications [implications set]
  (first (first (drop-while (fn [[old new]] (not= old new))
			    (iterate (fn [[old new]]
				       [new (add-immediate-elements implications new old)])
				     [#{} set])))))

(defn clop-by-implications [implications]
  (partial close-under-implications implications))

(defn follows-semantically? [implication implications]
  (subset? (conclusion implication) (close-under-implications implications (premise implication))))

;;; copied, thus needs a good idea for refactoring
(defn add-immediate-elements*
  ([implications set]
     (add-immediate-elements* implications set #{}))

  ([implications new-set old-set]
     (if (= new-set old-set)
       new-set
       (let [conclusions-to-add (for [impl implications
				      :when (and (proper-subset? (premise impl) new-set)
						 (exists [x (premise impl)] (not (old-set x))))]
				  (conclusion impl))]
	 (apply union new-set conclusions-to-add)))))

(defn close-under-implications* [implications set]
  (first (first (drop-while (fn [[old new]] (not= old new))
			    (iterate (fn [[old new]]
				       [new (add-immediate-elements* implications new old)])
				     [#{} set])))))

(defn clop-by-implications* [implications]
  (partial close-under-implications* implications))

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
