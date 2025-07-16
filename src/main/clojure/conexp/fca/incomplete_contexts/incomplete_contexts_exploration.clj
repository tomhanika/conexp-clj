(ns conexp.fca.incomplete-contexts.incomplete-contexts-exploration
  "Incomplete-Context-Exploration"
  (:require [clojure.set :refer [subset? intersection difference union]]
            [clojure.core.reducers :as r]
            [conexp.base :refer [ask exists]]
            [conexp.fca.closure-systems :refer [next-closed-set]]
            [conexp.fca.implications :refer :all]
            [conexp.fca.incomplete-contexts.incomplete-contexts :refer :all]))


;; TODO validate counterexample
(defn- counterexample-from-expert
  [implication context]
  (let [pre (premise implication)
        conc (conclusion implication)
        attributes (attributes context)
        objects (objects context)
        counterexample (ask (str "Please enter a counterexample for \n\t(" pre " \u2192 " conc ")\n in the following form: \n\t\"name\" [\"attributes it has\"] [\"attributes it has not\"]\n")
                            #(read-string (str "[" (read-line) "]"))
                            #(and (not (empty? (re-find #"([^\[\]\t\n]*)(\[[^\[\]\t\n]*\])(\[[^\[\]\t\n]*\])" (str 2 " " ['a 'b] ['c 'd])))) ; check for form obj [atts][atts]
                             (subset? (nth % 1) attributes)
                                  (subset? (nth % 2) attributes)
                                  (not (contains? objects (nth % 0)))
                                  (empty? (intersection (set (nth % 1)) (set (nth % 2)))))
                            "something went wrong")
        out [(str (first counterexample)) (set (nth counterexample 1)) (set (nth counterexample 2))]]
    out))




(defn- unknown-attributes-from-expert
  "ask the expert for the attributes from the conclusion where the implication holds."
  [implication]
  (let [pre (premise implication)
        conc (conclusion implication)
        premise-implies (ask (str "For which attributes m " \u2208 " " conc  " does " pre " " \u2192 " m hold?\n Simply press RETURN if it holds for none or you do not know.\n")
                             #(read-string (str "#{" (read-line) "}"))
                             #(subset? % conc)
                             (str "Please enter the attributes m " \u2208 " " conc " where (" pre " " \u2192 " m) holds."))
        unknown (difference conc premise-implies)]
    {:premise-implies premise-implies :unknown unknown}))


(defn- yes-no-unknown-abort?
  "Asks string, expecting 'yes', 'no', 'unknown' or 'abort'. Returns the corresponding input."
  [question]
  (ask (str question)
       #(read-string (str (read-line)))
       #{'yes 'no 'unknown 'abort}
       "Please answer 'yes', 'no' or 'unknown' (or 'abort'): \n"))


(defn- expert-interaction [initial-state]
  "Implements expert interaction.  The initial state is given by
   «initial-state», from which on the expert is queried for commands that modify
   this state.  If eventually one command returns nil, a vector is returned
   which arises from applying all functions in the sequence «result-fns» to
   state, collecting the return values in the vector.  In other words,
   «result-fns» is a sequence of functions, which are applied to the final state
   in the sequence given, and their return values (collected in a vector)
   constitute the return value of the call to this function."
  (let [answer (yes-no-unknown-abort? (str "Does the implication "
                                           (print-str (:implication initial-state))
                                           " hold? (yes/no/unknown) "))]
    (try
      (cond
        ;; counterexample
        (= answer 'no)
        (let [counterexample (counterexample-from-expert
                              (:implication initial-state)
                              (:examples-context initial-state))]
          [:counterexample [counterexample]])
        ;; implication
        (= answer 'yes)
        [:implication]
        ;; unknown
        (= answer 'unknown)
        (do
          (let [unknowns (unknown-attributes-from-expert (:implication initial-state))]
            [:unknown unknowns]))
        ;; abort interaction
        (= answer 'abort)
        :abort
        ;; failsave
        true
        (throw "impossible answer recieved..."))
      (catch Exception e
        :abort))))


(defn- incomplete-counterexamples-via-repl
  "Starts a repl for counterexamples, which may be incomplete."
  [examples-context knowledge impl]
  (expert-interaction {:examples-context examples-context
                       :knowledge knowledge
                       :implication impl}))


;; TODO add proper check at input

(defn- valid-counterexample?
  "Checks the given example for being valid."
  [state attributes impl]
  (let [new-atts    (union (set (:positives state)) (set (:negatives state)))
        return      (atom true)]
    (when-not (contains? state :object),
      (println "You need to set a name for the new object.")
      (reset! return false))
    (when (and (:complete-counterexamples? state)
               (not= attributes new-atts))
      (println "You have not specified a complete counterexample.")
      (reset! return false))
    (let [not-respected (filter (fn [impl]
                                  (not (respects? (set (:positives state)) impl)))
                                (:knowledge state))]
      (when-not (empty? not-respected)
        (println "Your example does not respect the following confirmed implications:")
        (doseq [impl not-respected]
          (println impl))
        (reset! return false)))
    (when-not (and (subset? (premise impl) (set (:positives state)))
                   (exists [m (:negatives state)]
                           (contains? (conclusion impl) m)))
      (println "Your example does not falsify the given implication.")
      (reset! return false))
    @return))


;;; Exploration
(defn- incidences-from-incomplete-counterexamples
  [counterexamples examples-context]
  (let [atts (attributes examples-context)]
    (into {} (r/map
              (fn [ex] (let [
                             [obj positives negatives] ex]
                         (r/reduce #(let [[g m w] %2] (assoc %1 [g m] w))
                                   {}
                                   (r/map (fn [att]
                                            (let [value (if (contains? positives att) known-true
                                                            (if (contains? negatives att) known-false unknown))]
                                              [obj att value])) atts))))
              counterexamples))))



(defn- incidences-from-incomplete-counterexamples-OLD
  [counterexamples examples-context]
  (into {} (map (fn [ex] (let [atts (attributes examples-context)
                               [obj positives negatives] ex]
                           (reduce #(let [[g m w] %2] (assoc %1 [g m] w))
                                   {}
                                   (map (fn [att]
                                          (let [value (if (contains? positives att) known-true
                                                          (if (contains? negatives att) known-false unknown))]
                                            [obj att value])) atts))))
                counterexamples)))


(defn- make-ficticious-examples
  "generates for all unknown attributes m a ficticious counterexample
   that has the form [\"g_(premise,m)\" premise #{m}]"
  [impl unknown-attributes]
  (let [pre (premise impl)]
    (reduce #(into %1 [[(str "$g_{" "\\text{"(clojure.string/join "," pre) "}\\not\\rightarrow \\text{" %2 "} }$") pre #{%2}]]) [] unknown-attributes)))


(defn- make-known-true-incidences
  "In: seq [obj #{attributes}]"
  [in]
  (persistent! (reduce #(assoc! %1 [(first in) %2] known-true) (transient {}) (second in))))


(defn- make-incidences-for-known-?-from-objects
  [objs context clop]
  (reduce #(into %1 (make-known-true-incidences %2))
          {}
          (map (fn [g] (let [atts (certain-intent context #{g})]
                         [g (clop atts)]))
               objs)))


(defn- explore-attributes-with-unknown-and-incomplete-counterexamples
  "Performs attribute exploration allowing for incomplete counterexamples"
  [incomplete-ctx background-knowledge handler]
  (let [M (attributes incomplete-ctx)]
    (loop [implications background-knowledge
           A         (close-under-implications implications #{})
           examples-context incomplete-ctx
           ficticious-examples #{}
           qa-pairs []]
      (cond
        ;; exploration finished
        (not A)
        {:implications     (difference implications background-knowledge),
         :examples-context examples-context
         :ficticious-examples ficticious-examples
         :question-answer-pairs qa-pairs}
        ;; exploration continues
        true
        (let [conclusion-from-A (odiamond examples-context (abox examples-context A))]
          (if (= A conclusion-from-A) ;; check satisfiability
            (recur implications
                   (next-closed-set M
                                    (clop-by-implications implications)
                                    A)
                   examples-context
                   ficticious-examples
                   qa-pairs)
            (let [new-impl (make-implication A conclusion-from-A)
                  answer (handler examples-context implications new-impl)]
              (cond
                (= answer :abort)  ; abort exploration
                (recur implications nil examples-context ficticious-examples qa-pairs)
                ;;
                (= (first answer) :counterexample)                     ; add counterexample
                (let [counterexamples (second answer)
                      new-objs (map first counterexamples)
                      new-incidences (incidences-from-incomplete-counterexamples
                                      counterexamples examples-context)
                      ;; collect all counterexamples in one context
                      ccxtSup (reduce incomplete-context-supremum
                                    (for [c counterexamples]
                                      (make-single-object-incomplete-context (first c) M (second c) (last c))))
                      ;; construct the subposition context from old and new examples
                      examples-context (incomplete-context-supremum examples-context ccxtSup)

                      ;; ?-reduce the non-ficticious objects
                      clop (clop-by-implications implications)
                      ficticious-objects (into #{} (map first ficticious-examples))
                      
                      ;; reduced-incidences (make-incidences-for-known-?-from-objects
                      ;;                     (difference (union new-objs (objects examples-context))
                      ;;                                 ficticious-objects) examples-context clop)
                      ;; updated-incidences (into (into (incidence examples-context) new-incidences)
                      ;;                          reduced-incidences)

                      reduced-incidences (make-incidences-for-known-?-from-objects
                                          (difference (objects examples-context)
                                                      ficticious-objects) examples-context clop)
                      updated-incidences (into (incidence examples-context) reduced-incidences)
                      
                      updated-objs (into (objects examples-context) new-objs)
                      
                      updated-examples-context (make-incomplete-context updated-objs
                                                                        M
                                                                        updated-incidences)
                      next-set A
                      ;; _ (println updated-examples-context) 
                      ]
                  (recur implications
                         next-set
                         updated-examples-context
                         ficticious-examples
                         (conj qa-pairs [new-impl :false new-objs])))
                (= (first answer) :implication)                     ; add implication
                (let [new-implications (conj implications new-impl)
                      new-clop         (clop-by-implications new-implications)
                      ficticious-objects (into #{} (map first ficticious-examples))
                      reduced-incidences (make-incidences-for-known-?-from-objects
                                          (difference (objects examples-context) ficticious-objects)
                                          examples-context new-clop)
                      updated-incidences  (into (incidence examples-context) reduced-incidences)
                      updated-examples-context (make-incomplete-context (objects examples-context)
                                                                        M
                                                                        updated-incidences)
                      next-set (next-closed-set M new-clop A)]
                  (recur new-implications
                         next-set
                         updated-examples-context
                         ficticious-examples
                         (conj qa-pairs [new-impl :true])))
                (= (first answer) :unknown)
                (let [premise-implies (:premise-implies (second answer))
                      unknowns (:unknown (second answer))
                      new-implx (make-implication A premise-implies)
                      updated-implications  (if (empty? (conclusion new-implx))
                                              implications
                                              (conj implications new-implx))
                      new-clop (clop-by-implications updated-implications)
                      implied-by-A  (new-clop A)
                      unknown-not-implied (difference unknowns implied-by-A)
                      updated-ficticious-examples (into ficticious-examples
                                                        (make-ficticious-examples new-impl unknown-not-implied))
                      ficticious-objects (into #{} (map first updated-ficticious-examples))
                      updated-objects (into (objects examples-context) ficticious-objects)
                      new-incidences (incidences-from-incomplete-counterexamples
                                      updated-ficticious-examples examples-context)
                      reduced-incidences (make-incidences-for-known-?-from-objects
                                          (difference (objects examples-context) ficticious-objects)
                                          examples-context new-clop)
                      updated-incidences (into (into (incidence examples-context) new-incidences)
                                               reduced-incidences)
                      new-examples-context (make-incomplete-context updated-objects
                                                                    M
                                                                    updated-incidences)
                      next-set (if (empty? unknown-not-implied)
                                 (next-closed-set M new-clop A) 
                                 A)
                      ]
                  (recur updated-implications
                         next-set
                         new-examples-context
                         updated-ficticious-examples
                         (conj qa-pairs [new-impl :unknown unknown-not-implied])))))))))))


(defn default-handler-for-incomplete-counterexamples
  "Default handler for attribute exploration with incomplete counterexamples. Does it's
    interaction on the console."
  [examples-context  known impl]
  (incomplete-counterexamples-via-repl examples-context known impl))


(defn explore-attributes
  "Performs attribute exploration on the given context(s).  Returns a hashmap of
  implications computed and the final context, stored with keys :implications
  and :context (in the case of complete counterexamples)
  or :possible-context/:certain-context (in the case of incomplete counterexamples),
  respectively.

  Arguments are passed as keyword arguments like so
  (explore-attributes
  :examples-context context
  :background-knowledge set-of-implications
  "
  [& {:keys [examples-context background-knowledge handler]}]
  ;; first check for arguments
  (assert (not (nil? examples-context))
          "examples-context must be given (can be a context without objects)")

  ;; dispatch
  (explore-attributes-with-unknown-and-incomplete-counterexamples
   examples-context
   (or background-knowledge #{})
   (or handler default-handler-for-incomplete-counterexamples)))

