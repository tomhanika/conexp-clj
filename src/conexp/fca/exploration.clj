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
	conexp.fca.implications))

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
    called with the current context, and a new implication. The
    handler has to return counterexamples for the new implication, if
    there are some. Otherwise it has to return nil. Counterexamples
    are given as a sequence of rows, every row being of the form [g
    ms], where «g» is a new object and «ms» is the set of its
    attributes.

  - background-knowledge «set of implications»

    background-knowledge denotes a set of implications used as
    background knowledge, which will be subtracted from the computed
    result."
  [ctx :background-knowledge #{} :handler default-handler]
  (loop [implications background-knowledge,
         last         #{},
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
                counterexamples (handler ctx new-impl)]
            (if counterexamples
              (let [new-objs (map first counterexamples)]
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
              (recur (conj implications new-impl)
                     (next-closed-set (attributes ctx)
                                      (clop-by-implications (conj implications new-impl))
                                      last)
                     ctx))))))))

;;; Exploration Helper

(defn falsifies-implication?
  "Returns true iff set of new attributes does not respect implication impl."
  [new-atts impl]
  (and (subset? (premise impl) new-atts)
       (not (subset? (conclusion impl) new-atts))))

(defn examples-by-automorphism
  "Generates for the given context ctx and a given example row [g
  atts] a sequence of new examples (as rows of the same form) by
  applying the context automorphism in auts. The context automorphisms
  are applied to the attributes in atts only, the corresponding object
  will be a newly generated."
  [ctx [g atts] auts]
  (map (fn [alpha]
         [(gensym (str g "-")), (set-of (alpha m) [m atts])])
       auts))

(defn saturate-example
  "For the given context, the new object new-obj and
  positives (i.e. attributes new-obj definitively has) and
  negatives (i.e. attributes new-obj definitively does not have)
  returns a the pair [saturated-positives saturated-negatives]."
  [ctx new-obj positives negatives]
  (unsupported-operation "Not yet implemented"))

;;; Default Handler

(defn default-handler
  "Default handler for attribute exploration. Does it's interaction on the console."
  [ctx impl]
  (when-not (yes-or-no? (str "Does the implication " (print-str impl) " hold? "))
    (println ctx)
    (loop [counterexamples []]
      (let [new-obj  (ask (str "Please enter new object: ")
                          #(read-string (str (read-line)))
                          (fn [new-obj] (not ((objects ctx) new-obj)))
                          "This object is already present, please enter a new one: "),
            new-atts (ask (str "Please enter the attributes the new object should have: ")
                          #(read-string (str "#{" (read-line) "}"))
                          (fn [new-atts]
                            (and (set? new-atts)
                                 (subset? new-atts (attributes ctx))
                                 (falsifies-implication? new-atts impl)))
                          (str "These attributes are not valid or do not falsify the implication.\n"
                               "Please enter new attributes: ")),
            new-ctrs (conj counterexamples [new-obj new-atts])]
        (if (yes-or-no? "Do you want to give another counterexample? ")
          (recur new-ctrs)
          new-ctrs)))))

;;;

nil
