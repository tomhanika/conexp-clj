;; Copyright ⓒ the conexp-clj developers; all rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.fca.exploration
  "Provides function for exploration and computing proper premises."
  (:use conexp.base
        conexp.fca.contexts
        conexp.fca.implications)
  (:require [clojure.core.reducers :as r]))


(defn exploration-step
  "Conduct one exploration step using counterexamples and background knowledge about implications"
  [ctx input-implications]
  (loop [implications input-implications
         last         (close-under-implications implications #{}),
         ctx          ctx]
    (if (not last)
      {:implications (difference (set implications) (set input-implications)) :context ctx}
      (let [conclusion-from-last (context-attribute-closure ctx last)]
        (if (= last conclusion-from-last)
          (recur implications
                 (next-closed-set (attributes ctx)
                                  (clop-by-implications implications)
                                  last)
                 ctx)
          (let [newimp (make-implication last conclusion-from-last)]
            (recur (conj implications newimp) nil ctx)) ;; new candidate implication
          )))))

;;; Helpers

(defn falsifies-implication?
  "Returns true iff set of new attributes does not respect implication impl."
  [new-atts impl]
  (and (subset? (premise impl) new-atts)
       (not (subset? (conclusion impl) new-atts))))

(defn examples-by-automorphisms
  "Generates for the given context ctx and a given example row [g
  atts] a sequence of new examples (as rows of the same form) by
  applying the context automorphism in auts. The context automorphisms
  are applied to the attributes in atts only, the corresponding object
  will be a newly generated."
  [ctx [g atts] auts]
  (distinct (conj (map (fn [alpha]
                         [(gensym (str g "-")), (set-of (alpha m) [m atts])])
                       auts)
                  [g atts])))

(defn saturate-partial-example
  "Saturates the partial example given by positives, negatives and
  unknown. Uses the set impls of implications for saturation."
  [impls positives negatives unknown]
  (let [clop    (clop-by-implications impls),
        new-pos (clop positives)]
    (when (exists [n negatives] (contains? new-pos n))
      (illegal-argument "Example given contradicts already known facts."))
    [new-pos,
     (union negatives
            (set-of ? [? unknown,
                       :when (seq (intersection negatives
                                                (clop (conj positives ?))))]))]))


;;; Counterexample REPL

(defmulti run-repl-command
  "Runs a command for the counterexample REPL."
  (fn [& args] (first args)))
(alter-meta! #'run-repl-command assoc :private true)

(defmulti help-repl-command
  "Returns the help string of the given command."
  (fn [& args] (first args)))
(alter-meta! #'help-repl-command assoc :private true)

(defn- suitable-repl-commands
  "Returns all known repl commands for query, which can be a symbol or
  a string."
  [query]
  (let [str-query (str query)]
    (filter #(.startsWith (str %) str-query)
            (remove #{:default} (keys (methods run-repl-command))))))

(def ^:private abortion-sentinal (Exception. "You should never see this"))

(defn- eval-command
  "Runs the given REPL command query with state, in the case the query uniquely
  determines a command.  If not, an error message is printed and state is
  returned."
  [query state]
  (if (= query 'abort)
    (throw abortion-sentinal)
    (let [suitable-methods (suitable-repl-commands query)]
      (cond
       (second suitable-methods)
       (do
         (println "Ambigious command, suitable methods are")
         (doseq [name suitable-methods]
           (println "  " name))
         state),
       (empty? suitable-methods)
       (do
         (println "Unknown command")
         state)
       :else
       (try
         (run-repl-command (first suitable-methods) state)
         (catch Throwable t
           (print "Encountered Error: ")
           (println t)
           state))))))

(defn- counterexample-from-expert [initial-state result-fns]
  (loop [state initial-state]
    (let [result (eval-command (ask "counterexample> "
                                    #(read-string (str (read-line)))
                                    (constantly true)
                                    "counterexample> ")
                               state)]
      (if (:done result)
        (vec (map (fn [f] (f state)) result-fns))
        (recur result)))))

(defn- expert-interaction [initial-state result-fns]
  "Implements expert interaction.  The initial state is given by
   «initial-state», from which on the expert is queried for commands that modify
   this state.  If eventually one command returns nil, a vector is returned
   which arises from applying all functions in the sequence «result-fns» to
   state, collecting the return values in the vector.  In other words,
   «result-fns» is a sequence of functions, which are applied to the final state
   in the sequence given, and their return values (collected in a vector)
   constitute the return value of the call to this function."
  (when-not (yes-or-no? (str "Does the implication "
                             (print-str (:implication initial-state))
                             " hold? "))
    (println "Then please provide a counterexample")
    (try
      (loop [counterexamples []]
        (let [counterexample  (counterexample-from-expert initial-state result-fns),
              counterexamples (conj counterexamples counterexample)]
          (if (yes-or-no? "Do you want to give another counterexample? ")
            (recur counterexamples)
            counterexamples)))
      (catch Exception e
        (if (identical? e abortion-sentinal)
          :abort
          (throw e))))))

(defn- counterexamples-via-repl
  "Starts a repl for counterexamples, which must be specified completely."
  [ctx knowledge impl]
  (expert-interaction {:context ctx,
                       :knowledge knowledge,
                       :implication impl
                       :complete-counterexamples? true}
                      [:object (comp set :positives)]))

(defn- incomplete-counterexamples-via-repl
  "Starts a repl for counterexamples, which may be incomplete."
  [possible-ctx certain-ctx knowledge impl]
  (expert-interaction {:possible-context possible-ctx,
                       :certain-context certain-ctx,
                       :knowledge knowledge,
                       :implication impl
                       :complete-counterexamples? false}
                      [:object (comp set :positives) (comp set :negatives)]))


;;; REPL commands

(defmacro- define-repl-fn [name doc & body]
  `(do
     (defmethod run-repl-command '~name
       ~'[_ state]
       (let [~'objects    (objects (or (:context ~'state)
                                       (:possible-context ~'state)))
             ~'attributes (attributes (or (:context ~'state)
                                          (:possible-context ~'state))),
             ~'impl       (:implication ~'state)]
         ~@body))
     (defmethod help-repl-command '~name
       ~'[_]
       ~doc)))

(define-repl-fn object
  "Enter a name for the new object."
  (assoc state :object
         (ask (str "Please enter new object: ")
              #(read-string (str (read-line)))
              #(not (contains? objects %))
              "This object is already present, please enter a new one: ")))

(define-repl-fn attributes
  "Enter all attributes the object has."
  (let [new-attributes (ask (str "Please enter the attributes the new object should have: ")
                            #(read-string (str "#{" (read-line) "}"))
                            #(subset? % attributes)
                            (str "The attributes have to be present in the given context.\n"
                                 "Please enter new attributes: "))]
    (-> state
        (assoc :positives new-attributes)
        (assoc :negatives (difference attributes new-attributes)))))

(define-repl-fn positives
  "Give some attributes the object definitively has."
  (let [positives (ask (str "Please enter attributes the new object definitively has: ")
                       #(read-string (str "#{" (read-line) "}"))
                       #(and (subset? % attributes)
                             (not (exists [m (:negatives state)]
                                    (contains? % m))))
                       (str "The attributes are invalid or already marked as negative.\n"
                            "Please enter new attributes: "))]
    (assoc state :positives positives)))

(define-repl-fn negatives
  "Give some attributes the object definitively has not."
  (let [negatives (ask (str "Please enter attributes the new object definitively has not: ")
                       #(read-string (str "#{" (read-line) "}"))
                       #(and (subset? % attributes)
                             (not (exists [m (:positives state)]
                                    (contains? % m))))
                       (str "The attributes are invalid or already marked as positive.\n"
                            "Please enter new attributes: "))]
    (assoc state :negatives negatives)))

(define-repl-fn complete-example
  "Adds all attributes, which are not positive, to the negatives."
  (let [non-pos (difference attributes (set (:positives state)))]
    (assoc state :negatives non-pos)))

(define-repl-fn saturate-partial-example
  "Saturates the given partial counterexample."
  (let [old-pos   (set (:positives state)),
        old-neg   (set (:negatives state)),
        [pos neg] (saturate-partial-example (:knowledge state)
                                            old-pos
                                            old-neg
                                            (difference attributes
                                                        old-pos
                                                        old-neg))]
    (-> state
        (assoc :positives pos)
        (assoc :negatives neg))))

(defn- valid-counterexample?
  "Checks the given example for being valid."
  [state attributes impl]
  (let [new-atts    (union (set (:positives state)) (set (:negatives state))),
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

(define-repl-fn check-counterexample
  "Checks the given counterexample for being valid."
  (valid-counterexample? state attributes impl)
  state)

(define-repl-fn quit
  "Quit the REPL, if the given counterexample is valid."
  (if (valid-counterexample? state attributes impl)
    (assoc state :done true)
    state))

(define-repl-fn done
  "Alias for «quit»"
  (run-repl-command 'quit state))

(define-repl-fn clear
  "Clears the current state."
  (-> state
      (dissoc :object)
      (dissoc :positives)
      (dissoc :negatives)))

(defn- print-when-there
  [state key string]
  (when (contains? state key)
    (println string (get state key))))

(define-repl-fn state
  "Prints the current state."
  (print-when-there state :object    "Object:    ")
  (print-when-there state :positives "Positives: ")
  (print-when-there state :negatives "Negatives: ")
  state)

(define-repl-fn context
  "Prints the current context."
  (if (:context state)
    (println (:context state))
    (do
      (println (:possible-context state))
      (println (:certain-context state))))
  state)

(define-repl-fn knowledge
  "Prints the currently known implications (including background knowledge)."
  (doseq [impl (:knowledge state)]
    (println impl))
  state)

(define-repl-fn implication
  "Prints the current implication."
  (println impl)
  state)

(define-repl-fn help
  "Prints help."
  (let [commands (suitable-repl-commands "")]
    (println "Type «abort» to abort exploration.")
    (println "Any other command can be abbreviated, as long as this is unambigious.")
    (doseq [cmd commands]
      (println (str "  " cmd))
      (println (str "    -> " (help-repl-command cmd))))
    state))


;;; Exploration Interface

(declare explore-attributes-with-complete-counterexamples
         explore-attributes-with-incomplete-counterexamples
         default-handler-for-complete-counterexamples
         default-handler-for-incomplete-counterexamples)

(defn explore-attributes
  "Performs attribute exploration on the given context(s).  Returns a hashmap of
  implications computed and the final context, stored with keys :implications
  and :context (in the case of complete counterexamples)
  or :possible-context/:certain-context (in the case of incomplete counterexamples),
  respectively.

  Arguments are passed as keyword arguments like so

    (explore-attributes
      :context ctx-1
      :handler my-handler)

    (explore-attributes
      :incomplete-counterexamples true
      :possible-context ctx-1
      :certain-context  ctx-2
      :handler          my-other-handler
      :background-knowledge #{})

  Either a value for :context or values for :possible-context and :certain-context must be
  given, but not both.  The second option is only possible if :incomplete-counterexamples
  is set to «true».

  Optional keyword arguments are:

  - :handler «fn»

    Interaction is accomplished via the given handler fn.  Depending on whether incomplete
    counterexamples are allowed or not, this handler is called with different arguments
    and is supposed to return different things:

    - if incomplete counterexamples are not allowed, «fn» is called (in this order) with
      the current working context, the set of known implications and the current
      implication to be asked to the expert.  It is supposed to return either «nil» (in
      which case the implication is accepted), or a sequence of counterexamples which are
      supposed to be of the form

        [g attributes]

      where «g» is the name of a new object and «attributes» is the set of attributes the
      new object should possess.

    - if incomplete counterexamples are allowed, «fn» is called with the context of
      possible incidences, the context of certain incidence, the set of known implications
      and the current implication to be asked to the expert.  It is supposed to return
      either «nil» (in which case the implication is accepted), or a sequence of
      counterexamples of the form

        [g positive-attributes negative-attributes],

      where «g» is a new object, «positive-attributes» is a sequence of attributes the new
      object has, and «negative-attributes» is a sequence of attributes the new object
      does not have.  Note that «positive-attributes» and «negative-attributes» must be
      disjoint.

    Note that it is the responsibility of the handler to ensure that the counterexample is
    correct.  If this is not the case, the exploration algorithm will just ask the same
    question in the next iteration again.

    It is possible to abort the exploration from within a handler.  For this, the handler
    just has to return :abort.  In this case, the current working context as well as the
    currently known implications are returned as if the exploration would have been
    finished in that iteration.

  - :background-knowledge «set of implications»

    background-knowledge denotes a set of implications used as background knowledge, which
    will be subtracted from the computed result.

  - :incomplete-counterexamples «true or false»

    Specifies whether incomplete counterexamples are allowed or not.  Default is false.
    Mandatory to be set to true if context is given via :possible-context
    and :certain-context.

  If you want to use automorphisms of the underlying context, you have to construct a
  special handler using the «make-handler» function. See the corresponding documentation
  of «make-handler»."
  [& {:keys [possible-context certain-context context
             background-knowledge handler incomplete-counterexamples]}]
  ;; first check for arguments
  (assert (or (and (nil? context)
                   (not (nil? possible-context))
                   (not (nil? certain-context)))
              (and (not (nil? context))
                   (nil? possible-context)
                   (nil? certain-context)))
          "Contexts can only be specified by either specifying only :context or by
          specifying only both :possible-context and :certain-context.")
  (assert (contains? #{true false nil} incomplete-counterexamples)
          "Value for :incomplete-counterexamples must be either «true» or «false».")
  (assert (or incomplete-counterexamples
              (not possible-context))
          "Possible/certain incidences can only be specified if incomplete counterexamples
          are allowed.")
  (let [[possible-context certain-context] (if context
                                             [context context]
                                             [possible-context certain-context]),
        background-knowledge               (or background-knowledge #{})]
    ;; additional checks
    (assert (and (context? possible-context)
                 (context? certain-context))
            "Arguments to :context or :possible-context/:certain-context must be contexts")
    (assert (set? background-knowledge)
            "Background knowledge must be given as set")
    (assert (= (attributes certain-context)
               (attributes possible-context))
            "Given contexts must coincide on the set of attributes")
    (assert (= (objects certain-context)
               (objects possible-context))
            "Given contexts must coincide on the set of objects")
    ;; dispatch
    (if incomplete-counterexamples
      (explore-attributes-with-incomplete-counterexamples
         possible-context
         certain-context
         background-knowledge
         (or handler
             default-handler-for-incomplete-counterexamples))
      (explore-attributes-with-complete-counterexamples
         possible-context ; same as certain-context
         background-knowledge
         (or handler
             default-handler-for-complete-counterexamples)))))

;;; Handler for Expert Interaction

(defn make-handler
  "Creates a handler for attribute exploration. Valid keys are

  - automorphisms: A sequence of automorphisms of the overall context,
    used to construct more examples from a given one.

    Currently, this has only an effect if the counterexamples are complete.

  - incomplete-counterexamples?: If true, allows for incomplete counterexamples.  In
    contrast to the case of complete counterexamples, the function returned takes four
    arguments (instead of 3), namely the context of the possible incidence, the context of
    the certain incidence, the known implications as well as the current implication to
    be asked to the expert."
  [& {:keys [automorphisms incomplete-counterexamples?]
      :or   {automorphisms #{},
             incomplete-counterexamples? false}}]
  (assert (or (set? automorphisms) (seq? automorphisms)))
  (assert (or (contains? #{true false} incomplete-counterexamples?)))
  (if incomplete-counterexamples?
    ;; case of incomplete counterexamples
    incomplete-counterexamples-via-repl
    ;; case of complete counterexamples
    (fn [ctx known impl]
      (let [counterexamples (counterexamples-via-repl ctx known impl)]
        (case counterexamples
          :abort :abort
          nil    nil
          (r/reduce into
                    []
                    (r/map #(examples-by-automorphisms ctx % automorphisms)
                           counterexamples)))))))

(let [dh (make-handler)]
  (defn default-handler-for-complete-counterexamples
    "Default handler for attribute exploration. Does it's interaction on the console."
    [ctx known impl]
    (dh ctx known impl)))

(let [dh (make-handler :incomplete-counterexamples? true)]
  (defn default-handler-for-incomplete-counterexamples
    "Default handler for attribute exploration with incomplete counterexamples. Does it's
    interaction on the console."
    [possible-ctx certain-ctx known impl]
    (dh possible-ctx certain-ctx known impl)))

;;; Attribute Exploration with Complete Counterexamples

(defn- explore-attributes-with-complete-counterexamples
  "Performs attribute exploration with complete background knowledge"
  [ctx background-knowledge handler]
  (loop [implications background-knowledge,
         last         (close-under-implications implications #{}),
         ctx          ctx]
    (if (not last)
      {:implications (difference implications background-knowledge),
       :context ctx}
      (let [conclusion-from-last (context-attribute-closure ctx last)]
        (if (= last conclusion-from-last)
          (recur implications
                 (next-closed-set (attributes ctx)
                                  (clop-by-implications implications)
                                  last)
                 ctx)
          (let [new-impl        (make-implication last conclusion-from-last),
                counterexamples (handler ctx implications new-impl)]
            (cond
             (= counterexamples :abort)
             (recur implications nil ctx) ; forget all other sets
             ;;
             counterexamples            ; add counterexample
             (let [new-objs (map first counterexamples)]
               ;; check that new names are not there already
               (when (exists [g new-objs] (contains? (objects ctx) g))
                 (illegal-argument "Got objects as «new objects» in exploration "
                                   "which are already present."))
               (recur implications
                      last
                      (make-context (into (objects ctx) new-objs)
                                    (attributes ctx)
                                    (union (incidence-relation ctx)
                                           (set-of [g m] [[g ms] counterexamples,
                                                          m ms])))))
             ;;
             true                       ; add implication
             (recur (conj implications new-impl)
                    (next-closed-set (attributes ctx)
                                     (clop-by-implications (conj implications new-impl))
                                     last)
                    ctx))))))))

;;; Attribute Exploration with Incomplete Counterexamples

(defn- explore-attributes-with-incomplete-counterexamples
  "Performs attribute exploration allowing for incomplete counterexamples"
  [possible-ctx certain-ctx background-knowledge handler]
  (loop [implications background-knowledge,
         last         (close-under-implications implications #{}),
         possible-ctx possible-ctx,
         certain-ctx  certain-ctx]
    (cond
     ;; consistency check
     (not (subset? (incidence-relation certain-ctx)
                   (incidence-relation possible-ctx)))
     (do
       (println "Inconsistent state reached: certain incidence is not a subset of the possible incidence")
       {:implications     (difference implications background-knowledge),
        :possible-context possible-ctx
        :certain-context  certain-ctx})
     ;; exploration finished
     (not last)
     {:implications     (difference implications background-knowledge),
      :possible-context possible-ctx
      :certain-context  certain-ctx}
     ;; exploration continues
     true
     (let [conclusion-from-last (oprime possible-ctx (aprime certain-ctx last))] ; ?
       (if (= last conclusion-from-last)
         (recur implications
                (next-closed-set (attributes possible-ctx)
                                 (clop-by-implications implications)
                                 last)
                possible-ctx
                certain-ctx)
         (let [new-impl        (make-implication last conclusion-from-last),
               counterexamples (handler possible-ctx certain-ctx implications new-impl)]
           (cond
            (= counterexamples :abort)  ; abort exploration
            (recur implications nil possible-ctx certain-ctx)
            ;;
            counterexamples             ; add counterexample
            (let [new-objs (map first counterexamples)]
              ;; check that new names are not there already
              (when (exists [g new-objs] (contains? (objects possible-ctx) g))
                (illegal-argument "Got objects as «new objects» in exploration "
                                  "which are already present."))
              (recur implications
                     last
                     ;; possible incidence, i.e. the one not excluded by the expert
                     (make-context (into (objects possible-ctx) new-objs)
                                   (attributes possible-ctx)
                                   (union (incidence-relation possible-ctx)
                                          (set-of [g m] [[g _ neg] counterexamples,
                                                         m (difference (attributes possible-ctx)
                                                                       (set neg))])))
                     ;; certain incidence, i.e. the one given by the expert
                     (make-context (into (objects certain-ctx) new-objs)
                                   (attributes certain-ctx)
                                   (union (incidence-relation certain-ctx)
                                          (set-of [g m] [[g pos _] counterexamples,
                                                         m pos])))))
            ;;
            true                        ; add implication
            (let [new-implications (conj implications new-impl),
                  new-clop         (clop-by-implications new-implications),
                  new-certain-ctx  (make-context (objects certain-ctx)
                                                 (attributes certain-ctx)
                                                 (set-of [g m] [g (objects certain-ctx),
                                                                m (new-clop (oprime certain-ctx #{g}))])),
                  new-possible-ctx (make-context (objects possible-ctx)
                                                 (attributes possible-ctx)
                                                 (filter (fn [[g m]]
                                                           (let [certain-atts (oprime new-certain-ctx #{g})]
                                                             (subset? (new-clop (conj certain-atts m))
                                                                      (oprime possible-ctx #{g}))))
                                                         (incidence-relation possible-ctx)))]
              (recur new-implications
                     (next-closed-set (attributes possible-ctx) new-clop last)
                     new-possible-ctx
                     new-certain-ctx)))))))))

;;;

true
