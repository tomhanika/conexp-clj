;; Copyright ⓒ the conexp-clj developers; all rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.fca.exploration.repl
  "A REPL for giving counterexamples during attribute exploration."
  (:use conexp.base
        conexp.fca.contexts
        conexp.fca.implications
        conexp.fca.exploration.util))

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
  "Runs the given REPL command query with state, in the case the query uniquely determines
  a command.  If not, an error message is printed and state is returned."
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

;;

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
  "Implements expert interaction.  The initial state is given by «initial-state», from
   which on the expert is queried for commands that modify this state.  If eventually one
   command returns nil, a vector is returned which arises from applying all functions in
   the sequence «result-fns» to state, collecting the return values in the vector.  In
   other words, «result-fns» is a sequence of functions, which are applied to the final
   state in the sequence given, and their return values (collected in a vector) constitute
   the return value of the call to this function."
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

(defn counterexamples-via-repl
  "Starts a repl for counterexamples, which must be specified completely."
  [ctx knowledge impl]
  (expert-interaction {:context ctx,
                       :knowledge knowledge,
                       :implication impl
                       :complete-counterexamples? true}
                      [:object (comp set :positives)]))

(defn incomplete-counterexamples-via-repl
  "Starts a repl for counterexamples, which may be incomplete."
  [possible-ctx certain-ctx knowledge impl]
  (expert-interaction {:possible-context possible-ctx,
                       :certain-context certain-ctx,
                       :knowledge knowledge,
                       :implication impl
                       :complete-counterexamples? false}
                      [:object (comp set :positives) (comp set :negatives)]))

;; REPL commands

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

;;;

nil
