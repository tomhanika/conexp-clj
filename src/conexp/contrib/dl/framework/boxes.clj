;; Copyright (c) Daniel Borchmann. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.contrib.dl.framework.boxes
  (:use conexp.main
	conexp.contrib.dl.framework.syntax
        conexp.contrib.dl.util.graphs))

;;; TBox definitions

(defrecord TBox [language definition-map])

(defn tbox-language
  "Returns language for which tbox is a tbox."
  [#^TBox tbox]
  (.language tbox))

(defn tbox-definition-map
  "Returns a hash-map mapping symbols to their corresponding definition in tbox."
  [#^TBox tbox]
  (.definition-map tbox))

(defmethod print-method TBox [tbox out]
  (let [#^String output (with-out-str
                          (print (list 'TBox
                                       (vals (tbox-definition-map tbox)))))]
    (.write out (.trim output))))

(defn make-tbox
  "Creates and returns a tbox for language from the given
  definition-map, which must be a mapping from symbols to definitions."
  [language definition-map]
  (TBox. language definition-map))

;;; accessing used role names, primitive and defined concepts

(defn tbox-definitions
  "Returns the definitions in a tbox."
  [tbox]
  (set (vals (tbox-definition-map tbox))))

(defn defined-concepts
  "Returns all defined concepts in tbox."
  [tbox]
  (set (keys (tbox-definition-map tbox))))

(defn used-role-names
  "Returns all used role names in tbox."
  [tbox]
  (apply union #{}
	 (map #(role-names-in-expression (definition-expression %))
	      (tbox-definitions tbox))))

(defn used-concept-names
  "Returns all used concept names in tbox."
  [tbox]
  (apply union #{}
	 (map #(concept-names-in-expression (definition-expression %))
	      (tbox-definitions tbox))))

;;;

(defn tbox?
  "Returns true iff thing is a tbox."
  [thing]
  (instance? TBox thing))

(defn tbox-target-pair?
  "Returns true iff dl-expr is a tbox-target-pair."
  [dl-expr]
  (and (dl-expression? dl-expr)
       (let [expr (expression-term dl-expr)]
	 (and (vector? expr)
	      (= 2 (count expr))
	      (tbox? (first expr))
	      (contains? (defined-concepts (first expr)) (second expr))))))

;;;

(defmacro tbox
  "Returns tbox defined for language containing given
  definitions."
  [language & definitions]
  (let [definitions (partition 2 definitions)]
    `(make-tbox
      ~language
      (conj {} ~@(for [pair definitions]
                   `['~(first pair) (make-dl-definition '~(first pair)
                                                        (dl-expression ~language ~(second pair)))])))))

(add-dl-syntax! 'tbox)

(defmacro define-tbox
  "Defines a TBox. Definitions are names interleaved with dl-sexps."
  [name language & definitions]
  `(def ~name (tbox ~language ~@definitions)))

;;;

(defn find-definition
  "Returns definition of target A in tbox, if it exists."
  [tbox A]
  (let [result (get (tbox-definition-map tbox) A nil)]
    (if (nil? result)
      (illegal-argument "Cannot find definition for " A " in tbox " (print-str tbox) ".")
      result)))

(defn tbox-union
  "Returns the union of tbox-1 and tbox-2."
  [tbox-1 tbox-2]
  (when-not (= (tbox-language tbox-1)
               (tbox-language tbox-2))
    (illegal-argument "Cannot unify tboxes of different description logics."))
  (TBox. (tbox-language tbox-1)
         (merge-with (fn [old new]
                       (if-not (= old new)
                         (illegal-state "Cannot unify tbox with different definitions for the same target.")
                         new))
                     (tbox-definition-map tbox-1)
                     (tbox-definition-map tbox-2))))

;;;

(defn uniquify-ttp
  "Substitutes for every defined concept name in tbox a new, globally
  unique, concept name and finally substitutes traget with its new name."
  [[tbox target]]
  (let [symbols     (defined-concepts tbox)]
    (when-not (some #(= target %) symbols)
      (illegal-argument "Target given to uniquify-ttp does not occur in the defined concepts of the given tbox."))
    (let [new-symbols (map-by-fn (fn [_] (gensym)) symbols)]
      [(make-tbox (tbox-language tbox)
                  (into {} (for [[sym sym-def] (tbox-definition-map tbox)]
                             [(new-symbols sym)
                              (make-dl-definition (new-symbols sym)
                                                  (substitute (definition-expression sym-def) new-symbols))])))
       (new-symbols target)])))

(defn uniquify-tbox
  "Substitutes for every defined concept name in tbox a new, globally
  unique, concept name."
  [tbox]
  (if (empty? (tbox-definitions tbox))
    tbox
    (first (uniquify-ttp [tbox (first (keys (tbox-definition-map tbox)))]))))

(defn usage-graph
  "Returns usage graph of a given tbox, i.e. a graph on the defined
  concepts of tbox where a concept C is connected to a concept D via
  an edge when D is contained in the definition of C."
  [tbox]
  (make-directed-graph (defined-concepts tbox)
                       (fn [A]
                         (free-symbols-in-expression
                          (definition-expression (find-definition tbox A))))))

(defn acyclic?
  "Returns true iff tbox is acyclic."
  [tbox]
  (zero? (count (self-recursive-sets (usage-graph tbox)))))

(defn tidy-up-ttp
  "In a given tbox-target-pair take for a set of syntactically
  equivalent defined concepts one representative and replace every
  occurence of an equivalent symbol by this representative. Use
  clarify-ttp (or reduce-ttp) afterwards to remove all unused
  definitions."
  [[tbox target]]
  (let [reversed-map (reduce (fn [hash-map definition]
			       (let [name (definition-target definition)]
				 (update-in hash-map [(expression-term (definition-expression definition))]
					    conj (definition-target definition))))
			     {}
			     (tbox-definitions tbox)),
	rename-map (into {} (for [definition (tbox-definitions tbox)]
			      [(definition-target definition),
			       (first (sort (reversed-map (expression-term (definition-expression definition)))))])),
	new-tbox (make-tbox (tbox-language tbox)
			    (into {} (for [definition (tbox-definitions tbox)]
                                       [(definition-target definition)
                                        (make-dl-definition (tbox-language tbox)
                                                            (definition-target definition)
                                                            (substitute (definition-expression definition) rename-map))])))]
    (if (not= tbox new-tbox)
      (tidy-up-ttp [new-tbox target])
      [tbox target])))

(defn collect-targets
  "Collects all targets reachable in the usage graph of tbox, starting
  from targets."
  [tbox targets seen]
  (if (empty? targets)
    seen
    (let [target  (first targets),
	  seen    (conj seen target),
	  targets (difference (into targets (free-symbols-in-expression
					     (definition-expression (find-definition tbox target))))
			      seen)]
      (recur tbox targets seen))))

(defn clarify-ttp
  "Clarifies tbox-target-pair for target, i.e. removes all definitions
  from tbox which are not needed to define target."
  [[tbox target]]
  (let [needed-targets (collect-targets tbox #{target} #{}),
	new-tbox       (make-tbox (tbox-language tbox)
                                  (select-keys (tbox-definition-map tbox) needed-targets))]
    [new-tbox target]))

(defn expand-ttp
  "Substitutes defined concepts in the definition of target by their
  definitions. Note that this function does not finish when the tbox is
  recursive in target."
  [[tbox target]]
  (let [symbols (free-symbols-in-expression (definition-expression (find-definition tbox target)))]
    (if (empty? symbols)
      [tbox target]
      (let [target-definition     (find-definition tbox target),
	    needed-definitions    (into {} (for [[_ sym-def] (select-keys (tbox-definition-map tbox) symbols)]
                                             [(definition-target sym-def) (definition-expression sym-def)])),

	    new-target-definition (make-dl-definition target
						      (substitute (definition-expression target-definition)
								  needed-definitions)),

	    [new-tbox target]     (clarify-ttp [(make-tbox (tbox-language tbox)
                                                           (assoc (tbox-definition-map tbox)
                                                             target new-target-definition))
                                                target])]
	(recur [new-tbox target])))))

(defn reduce-ttp
  "Reduces tbox-target-pair for target as much as possible, returning
  a pair of the reduced tbox and target."
  [[tbox target]]
  (let [ttp (clarify-ttp [tbox target])]
    (if (acyclic? (first ttp))
      (expand-ttp ttp)
      ttp)))

;;;

nil
