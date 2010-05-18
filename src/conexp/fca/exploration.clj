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

(update-ns-meta! conexp.fca.exploration
  :doc "Provides function for exploration and computing proper premises.")

;;; Attribute Exploration

(declare default-handler)

(defn explore-attributes
  "Performs attribute exploration in given context. Interaction is
  accomplished via the given handler, which is called with the current
  context and a new implication. It has to return a pair [status
  new-row], where status is a boolean, indicating whether this
  implication is to be accepted or not, and a new-row, which, in the
  case of not accepting an implication, has to be a valid
  counterexample. background-implications denotes a set of
  implications used as background knowledge, which will be subtracted
  from the computed result."
  ([ctx]
     (explore-attributes ctx #{}))
  ([ctx background-implications]
     (explore-attributes ctx background-implications default-handler))
  ([ctx background-implications handler]
     (loop [implications background-implications,
            last         #{},
            ctx          ctx]
       (if (not last)
         (difference implications background-implications)
         (let [conclusion-from-last (context-attribute-closure ctx last)]
           (if (= last conclusion-from-last)
             (recur implications
                    (next-closed-set (attributes ctx)
                                     (clop-by-implications* implications)
                                     last)
                    ctx)
             (let [new-impl (make-implication last conclusion-from-last),
                   [ok new-row] (handler ctx new-impl)]
               (if ok
                 (let [new-implications (conj implications new-impl)]
                   (recur new-implications
                          (next-closed-set (attributes ctx)
                                           (clop-by-implications* new-implications)
                                           last)
                          ctx))
                 (let [new-ctx (make-context (conj (objects ctx) (new-row 0))
                                             (attributes ctx)
                                             (union (incidence ctx) (new-row 1)))]
                   (recur implications
                          (next-closed-set (attributes new-ctx)
                                           (clop-by-implications* implications)
                                           last)
                          new-ctx))))))))))

(defn falsifies-implication?
  "Returns true iff set of new attributes does not respect implication impl."
  [new-atts impl]
  (and (subset? (premise impl) new-atts)
       (not (subset? (conclusion impl) new-atts))))

;;

(defn ask
  "Performs simple quering. prompt is printed first and then the user
  is asked for an answer (via read). If this answer does not satisfy
  pred, fail-message is printed and the user is asked again, until the
  given answer fulfills pred."
  [prompt pred fail-message]
  (do
    (print prompt)
    (flush)
    (loop [answer (read)]
      (if (pred answer)
        answer
        (do
          (print fail-message)
          (flush)
          (recur (read)))))))

(defn default-handler
  "Default handler for attribute exploration. Does it's interaction on the console."
  [ctx impl]
  (do
    (let [answer (ask (str "Does the implication " (print-str impl) " hold? ")
                      #{'yes 'no}
		      "Please answer 'yes' or 'no': ")]
      (if (= answer 'yes)
	[true []]
        (do
          (println ctx)
          (let [new-obj (ask (str "Please enter new object: ")
                             (fn [new-obj] (not ((objects ctx) new-obj)))
                             "This object is already present, please enter a new one: "),
                new-att (ask (str "Please enter the attributes the new object should have: ")
                             (fn [new-atts]
                               (and (subset? new-atts (attributes ctx))
                                    (falsifies-implication? new-atts impl)))
                             (str "These attributes are not valid or do not falsify the implication.\n"
                                  "Please enter a new set (in the form #{... atts ...}): "))]
            [false [new-obj (set (map (fn [att] [new-obj att]) new-att))]]))))))

;;;

nil
