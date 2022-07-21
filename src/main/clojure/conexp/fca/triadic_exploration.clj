(ns conexp.fca.triadic-exploration
  (:require [clojure.algo.generic.functor :refer [fmap]]
            [conexp.fca.contexts :as cxt]
            [conexp.fca.implications :as impl]
            [conexp.fca.exploration :as expl]
            [conexp.base :as base]
            ))



(defn triadic-context->context-family
  "Transform a triadic context into a family of contexts.  A triadic context is a quadrupel (G,M,B,Y) of objects G, attributes M, conditions B, and an incidence relation Y\\subseteq G \times M \\times B where [g m b]\\in Y reads as object g has attribute m under condition b.  The triadic context is sliced into one formal context per condition, i.e., contexts K^c (G,M,I^c), c\\in B where (g,m)\\in I^c \\Leftrightarrow (g,m,c)\\in Y.  The function returns a map {:c K^c ...} mapping the conditions to their respective contexts.  The triadic context must be represented as a map with keys :objects :attributes :conditions and :incidence, otherwise an error is thrown."
  [triadic-context]
  (if (and (contains? triadic-context :objects)
           (contains? triadic-context :attributes)
           (contains? triadic-context :conditions)
           (contains? triadic-context :incidence))
    (let [objects (:objects triadic-context)
          attributes (:attributes triadic-context)
          incidences-by-condition (group-by #(nth % 2) (:incidence triadic-context))]
      (into {}
            (for [c (:conditions triadic-context)]
              [c (cxt/make-context objects attributes (map drop-last (c incidences-by-condition)))])))
    (throw (Throwable. "triadic context is not a map with keys :objects :attributes :conditions and :incidence"))
    ))


(defn is-context-family-triadic?
  "Tests if the contexts in a context family {:cond1 cxt1 :cond2 cxt2 ...} form a triadic context, i.e., have the same sets of objects and attributes"
  [context-family]
  (let[[first-key & rest-keys :as conds] (keys context-family)
       first-context (first-key context-family)
       objs (cxt/objects first-context)
       atts (cxt/attributes first-context)]
    (->> rest-keys
         (map #(% context-family))
         (map (fn [cxt] (and
                         (= (cxt/objects cxt) objs)
                         (= (cxt/attributes cxt) atts))))
         (reduce #(every? identity [%1 %2])))))


(defn context-family->triadic-context
  "Transforms a context family into a triadic context.  The contexts in the family must have the same object and attribute sets otherwise an error is thrown. A context family is a map of conditions and their context on the same set of attributes, i.e., a map {:cond1 (G1,M,I1) :cond2 (G2,M,I2) ...}"
  [cxt-family]
  (if (is-context-family-triadic? cxt-family)
    (let [conditions (keys cxt-family)
          objects (cxt/objects ((first conditions) cxt-family))
          attributes (cxt/attributes ((first conditions) cxt-family))
          incidence (reduce into #{} (for [c conditions]
                                       (map #(conj % c) (cxt/incidence-relation (c cxt-family)))))]
      {:objects objects
       :attributes attributes
       :conditions conditions
       :incidence incidence})
    (throw (Throwable. "context-family is not triadic"))))


(defn- context-union-nf
  "compute (G_1 u G_2, M, I_1 u I_2) for contexts (G_1,M,I_1) and (G_2,M,I_2); In contrast to contexts/context-union this functions returns a context and *not* a function"
  [cxt1 cxt2]
  (let [objs (clojure.set/union (set (cxt/objects cxt1))
                                (set (cxt/objects cxt2)))
        atts (clojure.set/union (set (cxt/attributes cxt1))
                                (set (cxt/attributes cxt2)))
        incidence (clojure.set/union (set (cxt/incidence-relation cxt1))
                                     (set (cxt/incidence-relation cxt2)))]
    (cxt/make-context objs atts incidence)))


(defn- contradicts?
  "takes an object, an implication and a context, returns true if the object is a counterexample to the implication in the context"
  [o impl cxt]
  (if (impl/holds? impl (cxt/make-context #{o}
                                           (cxt/attributes cxt)
                                           (cxt/incidence-relation cxt)))
    false
    true))


(defn- counterexample
  "takes a formal context and an implication and 
  returns an counterexample with all its incidences if it exists or nil"
  [context implication]
  (let [objs (cxt/objects context)
        prem (impl/premise implication)
        concl (impl/conclusion implication)]
    (loop [o (first objs)
           other (rest objs)]
      (if (contradicts? o implication context)
        [o (into #{} (for [a (cxt/object-derivation context #{o})]
                       [o a]))]
        (if (empty? other)
          nil
          (recur (first other)
                 (rest other)))))))

(defn- triadic-object-incidences
  "takes a triadic context and an object and returns all incidences of that object"
  [triadic-context object]
  (->> (:incidence triadic-context)
       (filter (fn [i] (= object (first i))))
       (into #{})))


(defn triadic-expert-from-triadic-context
  "takes a triadic context and returns the corresponding triadic expert, i.e.,
  a function that takes a set of conditions D and an implication and returns
  the conditions $C \\subseteq D$ for which the implications hold and if $C \\not = D$
  also a counterexample [g triadic-incidence-of-g]

  triadic-expert
  input: conditions D and implication
  output:
  [D true] if the implication holds for all conditions in D
  or
  [subset-of-D-for-which-the-implication-holds counterexample]
  where counterexample = [g triadic-incidence-of-g]
  if the implication does not hold for all conditions
  "
  [tcxt]
  (fn [D question]
    (let [context-family (triadic-context->context-family tcxt)
          ;; test if question holds in all contexts in D
          ;; if not respond with a counterexample
          counterex-conditions (->> D
                                   (map #(hash-map % (% context-family)) ,,,)
                                   (into {} ,,,)
                                   (fmap #(impl/holds? question %) ,,,)
                                   (filter (fn [[k v]] (false? v)) ,,,)
                                   (into {} ,,,)
                                   (keys))]
      (if (some? counterex-conditions)
        (let [counterex (->> (counterexample ((first counterex-conditions) context-family) question)
                             (first)
                             (triadic-object-incidences tcxt)
                             ((fn [coll] (vector (ffirst coll) coll))))]  ;make the counterexample [g triadic-incidence]
          [(clojure.set/difference (set D) counterex-conditions) counterex]          
          )  
        [D true]))))


(defn- subposition-context-coll [cxts]
  "For a collection of contexts on the same attributes returns the subposition context"
  (reduce cxt/context-subposition cxts))


(defn- triadic-context+conditions->subposition-context
  "Given a triadic context and a set of conditions returns the subposition context consisting of the respective condition contexts"
  [triadic-context D]
  (subposition-context-coll (vals (select-keys (triadic-context->context-family triadic-context) D))))


(defn- next-closure-by-implications
  "Given a set of attributes A from the base set M and a set of implications L on M, returns the next closed set for A"
  [A M L]
  (base/next-closed-set
   M
   (impl/clop-by-implications L)
   A))


(defn- add-counterexample-to-triadic-context
  "The counterexample [g incidence] consists of the object g and its incidence"
  [triadic-context counterexample]
  (-> triadic-context
      (update-in [:objects] conj (first counterexample))
      (update-in [:incidence] into (second counterexample))))


(defn- add-row-to-context
  "Given a context and an [object incidence] pair adds the object to the context"
  [context [object incidence]]
  (cxt/make-context (conj (cxt/objects context) object)
                    (cxt/attributes context)
                    (into (cxt/incidence-relation context) incidence)))


(defn explore-conditions
  "
  required:
  triadic-expert = the expert answering the questions
  D = the set of conditions to explore
  E = a triadic context of examples (possibly empty)
  L0 = a set of background implications known to hold for all conditions in D

  optional:
  collect-all-implications = true(default)/false to collect either all
  implications that are asked about or only the ones that are valid for all conditions.  
  "
  ([triadic-expert D E L0]
   (explore-conditions triadic-expert D E L0 true))
  ([triadic-expert D E L0 collect-all-implications]
   (let [M (:attributes E)
         B (:conditions E)
         C0 (cxt/make-context #{}
                              B
                              #{})]
     (loop [A #{}
            L L0
            E E
            C C0]
       (if (not A)
         [L C E]
         (let [S (triadic-context+conditions->subposition-context E D)
               AJJ (cxt/context-attribute-closure S A)
               question (impl/make-implication A AJJ)]
           (if (= A AJJ)
             (recur (next-closure-by-implications A M (conj L question))
                    L
                    E
                    C)
             (let [full-answer (triadic-expert D question)
                   conditions-for-which-question-holds (first full-answer)
                   answer (second full-answer)
                   Cnew (if (true? answer)  
                          (add-row-to-context C [question (for [d D] [question d])])
                          (if collect-all-implications 
                            (add-row-to-context C [question (for [d conditions-for-which-question-holds]
                                                              [question d])])
                            C)
                          )]
               (if (true? answer)              ;no counterexample
                 (recur (next-closure-by-implications A M (conj L question))
                        (conj L question)
                        E
                        Cnew)
                 (recur A
                        L
                        (add-counterexample-to-triadic-context E answer)
                        Cnew))))))))))


(defn explore-all-conditional-theories
  "takes a triadic context of examples E
  and a context of implications known to hold C
  required:
  triadic-expert = a triadic expert for the domain
  E = context of examples (at least an empty context of attributes and conditions)
  C = context of implications and the conditions for which they hold (at least an empty context of conditions)
  optional:
  collect-all-implications = either collect all implications that are asked about or just the ones that hold for all conditions
  "
  ([triadic-expert E C]
   (explore-all-conditional-theories triadic-expert E C true))
  ([triadic-expert E C collect-all-implications]
   (let [conditions (:conditions E)
         condition-sets (->> (clojure.math.combinatorics/subsets conditions)
                             (map #(into #{} %))
                             (filter (comp not empty?))
                             (sort-by count)
                             (reverse))]
     (loop [D (first condition-sets)
            other (rest condition-sets)
            E E
            C C]
       (if (nil? D)
         [C E]
         (let [L (cxt/attribute-derivation C D)
               [LD CD ED] (explore-conditions triadic-expert D E L collect-all-implications)]
           (recur (first other)
                  (rest other)
                  ED
                  (context-union-nf C CD))))))))


(defn empty-triadic-context-from-triadic-context
  "Given a triadic context [G M B Y] returns an empty triadic context [#{} M B #{}]"
  [tcxt] {:objects #{}
          :attributes (:attributes tcxt)
          :conditions (:conditions tcxt)
          :incidence #{}})


(defn empty-implications-context-from-triadic-context
  "Given a triadic context [G M B Y] returns an empty context [#{} B #{}] where the conditions are the attributes"
  [tcxt] (cxt/make-context #{} (:conditions tcxt) #{}))


(defn compute-all-conditional-theories
  "Compute all conditional theories of a triadic context.
  Given a triadic context computes a context of implications and the conditions for which they hold.
  The extents of the concepts of this context are generating sets for the implication theory of implications that hold for all conditions of the respective intents."
  [triadic-context & {:keys [collect-all-implications] :or {collect-all-implications true}}]
  (let [triadic-expert (triadic-expert-from-triadic-context triadic-context)
        E (empty-triadic-context-from-triadic-context triadic-context)
        C (empty-implications-context-from-triadic-context triadic-context)]
    (first (explore-all-conditional-theories triadic-expert E C collect-all-implications))))



;;;;;;
;;; EXAMPLE
;;;;;;

;;; The following shows the situation of public transport at the train station Bf. Wilhelmshöhe with direction to the city center in Kassel.
;;; This example is part of the paper \"Triadic Exploration and Exploration with Multiple Experts\", see arXiv:2102.02654. 
;;; From Bf. Wilhelmshöhe you can travel by one of four bus lines (52, 55, 100 and 500), four tram lines (1, 3, 4 and 7), one night tram (N3) and one regional tram (RT5) to the city center.
;;; These are the objects of our context.
;;; The buses and trams leave the station at different times throughout each day of the week.
;;; The attributes of our context are the aggregated the leave-times, more specifically, we have split each day in five distinct time slots: early morning from 4:00 to 7:00, working hours from 7:00 to 19:00, evening from 19:00 to 21:00, late evening from 21:00 to 24:00 and night from 0:00 to 4:00.
;;; A bus or tram has a cross the formal context if there is at least one leave-time in the time slot.
;;; Furthermore, we have split the days of the week into Monday to Friday, Saturday and Sunday as conditions.


(def ^:private objs [:1 :3 :4 :7 :N3 :RT5 :52 :55 :100 :500])
(def ^:private atts [:night :early-morning :working-hours :evening :late-evening ])

(def ^:private mo-fr (cxt/make-context-from-matrix objs atts [0 1 1 1 1
                                                    0 1 1 1 1
                                                    0 1 1 1 1
                                                    0 1 1 1 0
                                                    0 0 0 0 0
                                                    0 1 1 1 1
                                                    0 1 1 1 1
                                                    0 1 1 0 0
                                                    0 1 1 1 1
                                                    1 1 1 1 1]))

(def ^:private sa (cxt/make-context-from-matrix objs atts [0 0 1 1 1
                                                 0 1 1 1 1
                                                 0 1 1 1 1
                                                 0 0 1 0 0
                                                 1 0 0 0 0
                                                 0 1 1 1 1
                                                 0 1 1 0 0
                                                 0 0 0 0 0
                                                 0 1 1 1 1
                                                 0 0 1 1 1]))

(def ^:private so (cxt/make-context-from-matrix objs atts [0 0 1 1 1
                                                 0 1 1 1 1
                                                 0 1 1 1 1
                                                 0 0 0 0 0
                                                 1 0 0 0 0
                                                 0 0 1 1 1
                                                 0 0 0 0 0
                                                 0 0 0 0 0
                                                 0 1 1 1 1
                                                 0 0 1 1 1]))

(def ^{:doc "This example is part of the paper \"Triadic Exploration and Exploration with Multiple Experts\", see arXiv:2102.02654"}
  cxt-family-paper {:Mo-Fr mo-fr
                    :Sat sa
                    :Sun so})

;; (def ^{:doc "This example is part of the paper \"Triadic Exploration and Exploration with Multiple Experts\", see arXiv:2102.02654"}
;;   tcxt-paper (context-family->triadic-context cxt-family-paper))

(def cxt-family-conceptual-exploration-book {:mo-fr (cxt/make-context-from-matrix [1 6 7 8]
                                                                [:very-early
                                                                 :working-hours
                                                                 :evening
                                                                 :late-night]
                                                                [0 1 1 1
                                                                 1 1 0 0
                                                                 1 1 1 1
                                                                 1 0 1 1])
                           :sat (cxt/make-context-from-matrix [1 6 7 8]
                                                                [:very-early
                                                                 :working-hours
                                                                 :evening
                                                                 :late-night]
                                                                [1 0 0 1
                                                                 0 0 0 0
                                                                 1 1 0 1
                                                                 1 1 1 1])
                           :sun (cxt/make-context-from-matrix [1 6 7 8]
                                                                [:very-early
                                                                 :working-hours
                                                                 :evening
                                                                 :late-night]
                                                                [0 0 1 0
                                                                 0 0 0 0
                                                                 1 1 0 1
                                                                 0 1 1 0])})


(def cxt-family-paper2 {:A (cxt/make-context [1] [:a :b ] [[1 :a]])
                        :B (cxt/make-context [1] [:a :b ] [])})
