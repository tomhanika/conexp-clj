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

;;; Attribute Exploration

(declare default-handler)

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
             (recur implications nil ctx) ;; forget all other sets
             ;;;
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
             ;;;
             true
             ;; add counterexample
             (recur (conj implications new-impl)
                    (next-closed-set (attributes ctx)
                                     (clop-by-implications (conj implications new-impl))
                                     last)
                    ctx))))))))

;;; Handler

(defnk make-handler
  "Creates a handler for attribute exploration. Valid keys are

  - automorphisms: A sequence of automorphisms of the overall context,
      used to construct more examples from a given one."
  [:automorphisms #{}]
  (assert (or (set? automorphisms) (seq? automorphisms)))
  (fn [ctx known impl]
    (when-not (yes-or-no? (str "Does the implication " (print-str impl) " hold? "))
      (loop [counterexamples []]
        (let [counterexample (counterexample-via-repl ctx known impl),
              new-counters   (into counterexamples
                                   (examples-by-automorphism ctx counterexample automorphisms))]
          (if (yes-or-no? "Do you want to give another counterexample? ")
            (recur new-counters)
            new-counters))))))

(let [dh (make-handler)]
  (defn default-handler
    "Default handler for attribute exploration. Does it's interaction on the console."
    [ctx known impl]
    (dh ctx known impl)))

;;;

nil
