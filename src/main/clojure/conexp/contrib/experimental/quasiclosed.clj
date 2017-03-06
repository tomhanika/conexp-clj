;; Copyright ⓒ the conexp-clj developers; all rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.contrib.experimental.quasiclosed
  "Some experiments with the quasiclosed sets of a formal context"
  (:use conexp.base
        conexp.fca.contexts
        conexp.fca.implications))

;;;

(defn- direct-quasigenerators
  "Returns all maximal proper subsets of set which have a different
  closure than set has."
  [ctx initial-set]
  (let [elements (vec initial-set),
        intent   (context-attribute-closure ctx initial-set),
        maximals (atom []),
        search   (fn search [current i]
                   ;; invariant: current is always a set with the same
                   ;; intent as initial-set
                   (if (>= i (count elements))
                     nil
                     (let [next (disj current (elements i))]
                       (if (not= (context-attribute-closure ctx next)
                                 intent)
                         (when (forall [x elements]
                                 (=> (not (contains? next x))
                                     (= intent
                                        (context-attribute-closure ctx (conj next x)))))
                           (swap! maximals conj next))
                         (search next (inc i)))
                       (search current (inc i)))))]
    (search initial-set 0)
    @maximals))

(defn context-attribute-quasiclosure
  "Computes the quasiclosure of set in context ctx."
  [ctx set]
  ;; Q: is the iteration needed?
  (let [quasiclosure (reduce (fn [closure generator]
                               (into closure (context-attribute-closure ctx generator)))
                             set
                             (direct-quasigenerators ctx set))]
    (if (= quasiclosure set)
      set
      (recur ctx quasiclosure))))

(defn quasi-intents
  "Returns the quasi-closed sets of attributes of the given context
  ctx."
  [ctx]
  (all-closed-sets (attributes ctx)
                   #(context-attribute-quasiclosure ctx %)))

(defn quasi-stem-base
  "Returns the base of implications with quasiclosed premise."
  [ctx]
  (reduce! (fn [impls set]
             (if (= set (context-attribute-closure ctx set))
               impls
               (conj! impls
                      (make-implication set
                                        (context-attribute-closure ctx set)))))
           #{}
           (quasi-intents ctx)))

;;;

nil
