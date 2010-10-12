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

(defn explore-attributes
  "Performs attribute exploration in given context. Returns a hashmap
  of implications computed and the final context, stored with keys
  :implications and :context, respectively.

  Interaction is accomplished via the given handler, which is called
  with the current context and a new implication. If the implication
  is accepted, nil has to be returned. Otherwise a pair [new-obj
  new-obj-attributes] of a new object and its attributes has to be
  returned. That is, the handler has to return a (valid)
  counterexample, if there is one, and nil otherwise.

  background-implications denotes a set of implications used as
  background knowledge, which will be subtracted from the computed
  result."
  ([ctx]
     (explore-attributes ctx #{}))
  ([ctx background-implications]
     (explore-attributes ctx background-implications default-handler))
  ([ctx background-implications handler]
     (loop [implications background-implications,
            last         #{},
            ctx          ctx]
       (if (not last)
         {:implications (difference implications background-implications),
          :context ctx}
         (let [conclusion-from-last (context-attribute-closure ctx last)]
           (if (= last conclusion-from-last)
             (recur implications
                    (next-closed-set (attributes ctx)
                                     (clop-by-implications implications)
                                     last)
                    ctx)
             (let [new-impl (make-implication last conclusion-from-last)]
               (if-let [[new-obj new-atts] (handler ctx new-impl)]
                 (recur implications
                        last
                        (make-context (conj (objects ctx) new-obj)
                                      (attributes ctx)
                                      (union (incidence ctx)
                                             (set-of [new-obj m]
                                                     [m new-atts]))))
                 (recur (conj implications new-impl)
                        (next-closed-set (attributes ctx)
                                         (clop-by-implications (conj implications new-impl))
                                         last)
                        ctx)))))))))

(defn falsifies-implication?
  "Returns true iff set of new attributes does not respect implication impl."
  [new-atts impl]
  (and (subset? (premise impl) new-atts)
       (not (subset? (conclusion impl) new-atts))))

;;

(defn default-handler
  "Default handler for attribute exploration. Does it's interaction on the console."
  [ctx impl]
  (let [answer (ask (str "Does the implication " (print-str impl) " hold? ")
                    #(read-string (str (read-line)))
                    #{'yes 'no}
                    "Please answer 'yes' or 'no': ")]
    (when (= answer 'no)
      (println ctx)
      (let [new-obj (ask (str "Please enter new object: ")
                         #(read-string (str (read-line)))
                         (fn [new-obj] (not ((objects ctx) new-obj)))
                         "This object is already present, please enter a new one: "),
            new-att (ask (str "Please enter the attributes the new object should have: ")
                         #(read-string (str "#{" (read-line) "}"))
                         (fn [new-atts]
                           (and (set? new-atts)
                                (subset? new-atts (attributes ctx))
                                (falsifies-implication? new-atts impl)))
                         (str "These attributes are not valid or do not falsify the implication.\n"
                              "Please enter new attributes: "))]
        [new-obj (set new-att)]))))

;;;

nil
