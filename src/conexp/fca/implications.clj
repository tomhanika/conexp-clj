;; Copyright (c) Daniel Borchmann. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.fca.implications
  (:gen-class
   :name conexp.fca.Implication
   :prefix "Implication-"
   :init init
   :constructors { [ Object Object ] [] }
   :state state)
  (:use conexp.base
	conexp.fca.contexts))

;;;

(defn Implication-init [premise conclusion]
  [ [] {:premise premise,
	:conclusion conclusion} ])

(defmulti premise
  "Returns premise of given object."
  class)

(defmethod premise conexp.fca.Implication [#^conexp.fca.Implication impl]
  (-> impl .state :premise))

(defmulti conclusion
  "Returns conclusion of given object."
  class)

(defmethod conclusion conexp.fca.Implication [#^conexp.fca.Implication impl]
  (-> impl .state :conclusion))

(defn Implication-hashCode
  "Implements hashCode for implications."
  [this]
  (bit-xor (hash (-> this .state :premise))
	   (hash (-> this .state :conclusion))))

(defn Implication-toString [this]
  (str "( " (premise this) "  ==>  " (conclusion this) " )"))

(defn Implication-equals [this other]
  (and (instance? conexp.fca.Implication other)
       (= (premise this) (premise other))
       (= (conclusion this) (conclusion other))))

;;;

(defn make-implication
  "Creates an implication (premise => conclusion $\\setminus$ premise)."
  [premise conclusion]
  (let [premise (set premise)
	conclusion (set conclusion)]
    (conexp.fca.Implication. premise (difference conclusion premise))))

(defn respects?
  "Returns true iff set respects given implication impl."
  [set impl]
  (or (not (subset? (premise impl) set))
      (subset? (conclusion impl) set)))

(defn holds?
  "Returns true iff impl holds in given context ctx."
  [impl ctx]
  (forall [intent (context-intents ctx)]
	  (respects? intent impl)))

(defn- add-immediate-elements
  "Adds all elements which follow from implications with premises in
  initial-set."
  [implications initial-set]
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

(defn- close-under-implications
  "Computes smallest superset of set being closed under given implications."
  [implications set]
  (loop [set set
	 impls implications]
    (let [[new impls] (add-immediate-elements impls set)]
      (if (= new set)
	new
	(recur new impls)))))

(defn clop-by-implications
  "Returns closure operator given by implications."
  [implications]
  (partial close-under-implications implications))

(defn follows-semantically?
  "Returns true iff implication follows semantically from given
  implications."
  [implication implications]
  (subset? (conclusion implication)
	   (close-under-implications implications (premise implication))))

;; Stem Base

(defn- add-immediate-elements*
  "Add all elements from conclusion of implications whose premises are
  proper subsets of initial-set. This is needed for computing the
  stem-base."
  [implications initial-set]
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

(defn clop-by-implications*
  "Returns closure operator given by implications. Closed sets are
  computed from implications with premises being proper subsets."
  [implications]
  (binding [add-immediate-elements add-immediate-elements*]
    (partial close-under-implications implications)))

(defn stem-base
  "Returns stem base of given context."
  [ctx]
  (let [double-prime (partial context-attribute-closure ctx)
	attributes   (attributes ctx)]
    (loop [implications #{}
	   last         #{}]
      (let [conclusion-from-last (double-prime last)
	    implications (if (not= last conclusion-from-last)
			   (conj implications
				 (make-implication last conclusion-from-last))
			   implications)
	    clop (clop-by-implications* implications)
	    next (next-closed-set attributes clop last)]
	(if next
	  (recur implications next)
	  implications)))))

;;;

nil
