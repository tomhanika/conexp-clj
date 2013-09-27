;; Copyright (c) Daniel Borchmann. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.fca.exploration
  (:use conexp.base
        conexp.fca.contexts
        conexp.fca.implications
        conexp.fca.exploration.util
        conexp.fca.exploration.repl))

(ns-doc
 "Provides function for exploration and computing proper premises.")

;;; Handler for Expert Interaction

(defnk make-handler
  "Creates a handler for attribute exploration. Valid keys are

  - automorphisms: A sequence of automorphisms of the overall context,
    used to construct more examples from a given one.

    Currently, this has only an effect if the counterexamples are complete.

  - incomplete-counterexamples?: If true, allows for incomplete counterexamples.  In
    contrast to the case of complete counterexamples, the function returned takes four
    arguments (instead of 3), namely the context of the possible incidence, the context of
    the certain incidence, the known implications aas well as the current implication to
    be asked to the expert."
  [:automorphisms #{}, :incomplete-counterexamples? false]
  (assert (or (set? automorphisms) (seq? automorphisms)))
  (assert (or (contains? #{true false} incomplete-counterexamples?)))
  (if incomplete-counterexamples?
    ;; case of incomplete counterexamples
    (fn [possible-ctx certain-ctx known impl]
      (when-not (yes-or-no? (str "Does the implication " (print-str impl) " hold? "))
        (loop [counterexamples []]
          (let [counterexamples (conj counterexamples
                                      (incomplete-counterexample-via-repl possible-ctx
                                                                          certain-ctx
                                                                          known
                                                                          impl))]
            (if (yes-or-no? "Do you want to give another counterexample? ")
              (recur counterexamples)
              counterexamples)))))
    ;; case of complete counterexamples
    (fn [ctx known impl]
      (when-not (yes-or-no? (str "Does the implication " (print-str impl) " hold? "))
        (loop [counterexamples []]
          (let [counterexample (counterexample-via-repl ctx known impl),
                new-counters   (into counterexamples
                                     (examples-by-automorphism ctx counterexample automorphisms))]
            (if (yes-or-no? "Do you want to give another counterexample? ")
              (recur new-counters)
              new-counters)))))))

;;; Attribute Exploration with Complete Counterexamples

(let [dh (make-handler)]
  (defn default-handler
    "Default handler for attribute exploration. Does it's interaction on the console."
    [ctx known impl]
    (dh ctx known impl)))

(defnk explore-attributes
  "Performs attribute exploration in given context. Returns a hashmap
  of implications computed and the final context, stored with keys
  :implications and :context, respectively.

  The function takes a number of keyword arguments, which are as follows:

  - :handler «fn»

    Interaction is accomplished via the given handler fn, which is
    called with the current context, all implications known so far and
    a new implication. The handler has to return counterexamples for
    the new implication, if there are some. Otherwise it has to return
    nil. Counterexamples are given as a sequence of rows, every row
    being of the form [g ms], where «g» is a new object and «ms» is
    the set of its attributes.

  - :background-knowledge «set of implications»

    background-knowledge denotes a set of implications used as
    background knowledge, which will be subtracted from the computed
    result.

  If you want to use automorphisms of the underlying context, you have
  to construct a special handler using the «make-handler»
  function. See the corresponding documentation of «make-handler»"
  [ctx, :background-knowledge #{}, :handler default-handler]
  (assert (set? background-knowledge))
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
                counterexamples (try
                                  (handler ctx implications new-impl)
                                  (catch Throwable e
                                    :abort))]
            (cond
             (= counterexamples :abort)
             (recur implications nil ctx) ; forget all other sets
             ;;
             counterexamples
             (let [new-objs (map first counterexamples)]
               ;; check that new names are not there already
               (when (exists [g new-objs] (contains? (objects ctx) g))
                 (illegal-argument "Got objects as «new objects» in exploration "
                                   "which are already present."))
               (recur implications
                      last
                      (make-context (into (objects ctx) new-objs)
                                    (attributes ctx)
                                    (union (incidence ctx)
                                           (set-of [g m] [[g ms] counterexamples,
                                                          m ms])))))
             ;;
             true                       ; add counterexample
             (recur (conj implications new-impl)
                    (next-closed-set (attributes ctx)
                                     (clop-by-implications (conj implications new-impl))
                                     last)
                    ctx))))))))

;;; Attribute Exploration with Incomplete Counterexamples

(let [dh (make-handler :incomplete-counterexamples? true)]
  (defn default-handler-for-incomplete-counterexamples
    "Default handler for attribute exploration with incomplete counterexamples. Does it's
    interaction on the console."
    [possible-ctx certain-ctx known impl]
    (dh possible-ctx certain-ctx known impl)))

(defn explore-attributes-with-incomplete-counterexamples
  "EXPERIMENTAL and LARGLY UNTESTED — USE WITH CARE

  Performs attribute exploration in given contexts of possible («possible-ctx») and
  certain («certain-ctx») incidences, allowing the expert to provide incomplete
  counterexamples.  Returns a hashmap of implications computed and the final context,
  stored with keys :implications and :context, respectively.

  Arguments are passed as keyword arguments like so

    (explore-attributes-with-incomplete-counterexamples
      :possible-context ctx-1
      :certain-context  ctx-2
      :handler          my-handler
      :background-knowledge #{})

    (explore-attributes-with-incomplete-counterexamples
      :context ctx-1
      :handler my-other-handler)

  Either a value for :context or values for :possible-context and :certain-context must be
  given, but not both.  Optional keyword arguments are:

  - :handler «fn»

    Interaction is accomplished via the given handler fn, which is called
    with the current context of possible incidences, the current context of
    certain indcidences, all implications known so far and a new
    implication. The handler has to return counterexamples for the new
    implication, if there are some. Otherwise it has to return
    nil. Counterexamples are given as a sequence of rows, every row being
    of the form

       [g positive-attributes negative-attributes],

    where «g» is a new object, «positive-attributes» is a sequence of
    attributes the new object has, and «negative-attributes» is a sequence
    of attributes the new object does not have.  Note that
    «positive-attributes» and «negative-attributes» must be disjoint.

  - :background-knowledge «set of implications»

    background-knowledge denotes a set of implications used as
    background knowledge, which will be subtracted from the computed
    result.

  If you want to use automorphisms of the underlying context, you have
  to construct a special handler using the «make-handler»
  function. See the corresponding documentation of «make-handler»."
  [& {:keys [possible-context certain-context context background-knowledge handler]}]
  (assert (or (and (nil? context)
                   (not (nil? possible-context))
                   (not (nil? certain-context)))
              (and (not (nil? context))
                   (nil? possible-context)
                   (nil? certain-context)))
          "Contexts can only be specified by either specifying only :context or by
          specifying only both :possible-context and :certain-context.")
  (let [[possible-ctx certain-ctx] (if context
                                     [context context]
                                     [possible-context certain-context]),
        background-knowledge       (or background-knowledge #{}),
        handler                    (or handler default-handler-for-incomplete-counterexamples)]
    ;; check arguments
    (assert (and (context? possible-ctx)
                 (context? certain-ctx))
            "Arguments to :context or :possible-context/:certain-context must be contexts")
    (assert (set? background-knowledge)
            "Background knowledge must be given as set")
    (assert (= (attributes certain-ctx)
               (attributes possible-ctx))
            "Given contexts must coincide on the set of attributes")
    (assert (= (objects certain-ctx)
               (objects possible-ctx))
            "Given contexts must coincide on the set of objects")
    ;; actual exploration
    (loop [implications background-knowledge,
           last         (close-under-implications implications #{}),
           possible-ctx possible-ctx
           certain-ctx  certain-ctx]
      (if (not last)
        {:implications     (difference implications background-knowledge),
         :possible-context possible-ctx
         :certain-context  certain-ctx}
        (let [conclusion-from-last (oprime possible-ctx (aprime certain-ctx last))] ; ?
          (if (= last conclusion-from-last)
            (recur implications
                   (next-closed-set (attributes possible-ctx)
                                    (clop-by-implications implications)
                                    last)
                   possible-ctx
                   certain-ctx)
            (let [new-impl        (make-implication last conclusion-from-last),
                  counterexamples (try
                                    (handler possible-ctx certain-ctx implications new-impl)
                                    (catch Throwable e
                                      :abort))]

              (cond
               (= counterexamples :abort) ; abort exploration
               (recur implications nil possible-ctx certain-ctx)
               ;;
               counterexamples          ; add counterexample
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
                                      (union (incidence possible-ctx)
                                             (set-of [g m] [[g _ neg] counterexamples,
                                                            m (difference (attributes possible-ctx)
                                                                          (set neg))])))
                        ;; certain incidence, i.e. the one given by the expert
                        (make-context (into (objects certain-ctx) new-objs)
                                      (attributes certain-ctx)
                                      (union (incidence certain-ctx)
                                             (set-of [g m] [[g pos _] counterexamples,
                                                            m pos])))))
               ;;
               true                     ; add implication
               (let [new-implications (conj implications new-impl)]
                 (recur new-implications
                        (next-closed-set (attributes possible-ctx)
                                         (clop-by-implications new-implications)
                                         last)
                        possible-ctx
                        (make-context (objects certain-ctx)
                                      (attributes certain-ctx)
                                      (set-of [g m] [g (objects certain-ctx),
                                                     m (close-under-implications
                                                        new-implications
                                                        (oprime certain-ctx #{g}))]))))))))))))

;;;

nil
