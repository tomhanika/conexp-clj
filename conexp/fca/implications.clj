(ns conexp.fca.implications
  (:gen-class
   :name conexp.fca.Implication
   :prefix "Implication-"
   :init init
   :constructors { [ clojure.lang.PersistentHashSet clojure.lang.PersistentHashSet ] [] }
   :state state)
  (:use conexp.util
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
  (and (= (premise this) (premise other))
       (= (conclusion this) (conclusion other))))

(defn make-implication [premise conclusion]
  (conexp.fca.Implication. (set premise) (set conclusion)))

(defn respects? [set impl]
  (or (not (subset? (premise impl) set))
      (subset? (conclusion impl) set)))

(defn holds? [impl ctx]
  (forall [intent (context-intents ctx)]
	  (respects? intent impl)))