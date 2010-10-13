;; Copyright (c) Daniel Borchmann. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.fca.exploration.repl
  (:use conexp.base
	conexp.fca.contexts
	conexp.fca.implications
        conexp.fca.exploration.util))

(ns-doc
 "A REPL for giving counterexamples during attribute exploration.")

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

(defn- eval-command
  "Runs the given REPL command query with state, context ctx and
  implication impl."
  [query state]
  (if (= query 'abort)
    (throw (Exception. "Abnormal abortion from attribute exploration"))
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

(defn counterexample-via-repl
  "Starts a repl for counterexamples."
  [ctx knowledge impl]
  (loop [state {:context ctx,
                :knowledge knowledge,
                :implication impl}]
    (let [query  (ask "counterexample> "
                      #(read-string (str (read-line)))
                      (constantly true)
                      "counterexample> "),
          result (eval-command query state)]
      (if result
        (recur result)
        [(:object state) (set (:positives state))]))))

(defmacro- define-repl-fn [name doc & body]
  `(do
     (defmethod run-repl-command '~name
       ~'[_ state]
       (let [~'ctx  (:context ~'state),
             ~'impl (:implication ~'state)]
         ~@body))
     (defmethod help-repl-command '~name
       ~'[_]
       ~doc)))

(define-repl-fn object
  "Enter a name for the new object."
  (assoc state :object
         (ask (str "Please enter new object: ")
              #(read-string (str (read-line)))
              #(not (contains? (objects ctx) %))
              "This object is already present, please enter a new one: ")))

(define-repl-fn attributes
  "Enter all attributes the object has."
  (let [new-attributes (ask (str "Please enter the attributes the new object should have: ")
                            #(read-string (str "#{" (read-line) "}"))
                            #(subset? % (attributes ctx))
                            (str "The attributes have to be present in the given context.\n"
                                 "Please enter new attributes: "))]
    (-> state
        (assoc :positives new-attributes)
        (assoc :negatives (difference (attributes ctx) new-attributes)))))

(define-repl-fn positives
  "Give some attributes the object definitively has."
  (let [positives (ask (str "Please enter attributes the new object definitively has: ")
                       #(read-string (str "#{" (read-line) "}"))
                       #(and (subset? % (attributes ctx))
                             (not (exists [m (:negatives state)]
                                    (contains? % m))))
                       (str "The attributes are invalid or already marked as negative.\n"
                            "Please enter new attributes: "))]
    (assoc state :positives positives)))

(define-repl-fn negatives
  "Give some attributes the object definitively has not."
  (let [negatives (ask (str "Please enter attributes the new object definitively has not: ")
                       #(read-string (str "#{" (read-line) "}"))
                       #(and (subset? % (attributes ctx))
                             (not (exists [m (:positives state)]
                                    (contains? % m))))
                       (str "The attributes are invalid or already marked as positive.\n"
                            "Please enter new attributes: "))]
    (assoc state :negatives negatives)))

(define-repl-fn saturate-partial-example
  "Saturates the given partial counterexample."
  (let [old-pos   (set (:positives state)),
        old-neg   (set (:negatives state)),
        [pos neg] (saturate-partial-example (:knowledge state)
                                            old-pos
                                            old-neg
                                            (difference (attributes ctx)
                                                        old-pos
                                                        old-neg))]
    (-> state
        (assoc :positives pos)
        (assoc :negatives neg))))

(defn- valid-counterexample?
  "Checks the given example for being valid."
  [state ctx impl]
  (let [new-atts    (union (set (:positives state)) (set (:negatives state))),
        object-set? (contains? state :object),
        all-atts?   (= (attributes ctx) new-atts),
        valid-cex?  (falsifies-implication? (set (:positives state)) impl)]
    (when-not object-set?
      (println "You need to set a name for the new object."))
    (when-not all-atts?
      (println "You have not specified a complete counterexample."))
    (when-not valid-cex?
      (println "Your example does not falsify the given implication."))
    (and object-set? all-atts? valid-cex?)))

(define-repl-fn check-counterexample
  "Checks the given counterexample for being valid."
  (valid-counterexample? state ctx impl)
  state)

(define-repl-fn quit
  "Quit the REPL, if the given counterexample is valid."
  (if (valid-counterexample? state ctx impl)
    nil
    state))

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
  (println ctx)
  state)

(define-repl-fn knowledge
  "Prints the currently known implications (including background knowledge)."
  (println knowledge)
  state)

(define-repl-fn implication
  "Prints the current implication."
  (println impl)
  state)

(define-repl-fn help
  "Prints help."
  (let [commands (suitable-repl-commands "")]
    (println "Any command can be abbreviated, as long as this is unambigious.")
    (doseq [cmd commands]
      (println (str "  " cmd))
      (println (str "    -> " (help-repl-command cmd))))
    state))

;;;

nil
