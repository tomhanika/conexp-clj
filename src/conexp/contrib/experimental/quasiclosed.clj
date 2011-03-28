;; Copyright (c) Daniel Borchmann. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.contrib.experimental.quasiclosed
  (:use conexp.base
        conexp.fca.contexts
        conexp.fca.implications))

(ns-doc "Some experiments with the quasiclosed sets of a formal
context")

;;;

(defn- direct-quasigenerators
  "Returns all maximal proper subsets of set which have a different
  closure than set has."
  [ctx initial-set]
  (let [elements (vec initial-set),
        intent   (context-attribute-closure ctx initial-set),
        result   (atom []),
        search   (fn search [current i] ;this yields more than the direct generators
                   ;; invariant: current is always a set with the same
                   ;; intent as initial-set
                   (if (>= i (count elements))
                     nil
                     (let [next (disj current (elements i))]
                       (if (not= (context-attribute-closure ctx next)
                                 intent)
                         (swap! result conj next)
                         (search next (inc i)))
                       (search current (inc i)))))]
    (search initial-set 0)
    @result))

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
           (all-closed-sets (attributes ctx)
                            #(context-attribute-quasiclosure ctx %))))

;;;

;; (defn- context-attribute-pseudoclosure  ;this is wrong
;;   "Returns the pseudoclosure of initial-set in context ctx."
;;   [ctx initial-set]
;;   (let [elements (vec initial-set),
;;         intent   (context-attribute-closure ctx initial-set),
;;         minimal-nongens (atom []),
;;         search   (fn search [current i]
;;                    (if (not= (context-attribute-closure ctx current)
;;                              intent)
;;                      nil
;;                      (let [old @minimal-nongens]
;;                        (doseq [j (range 0 (count elements))]
;;                          (let [next (disj current (elements j))]
;;                            (when (not= next current)
;;                              (search next (inc j)))))
;;                        (when (= old @minimal-nongens)
;;                          (swap! minimal-nongens conj current)))))]
;;     (search initial-set 0)
;;     (println @minimal-nongens)
;;     (if (empty? @minimal-nongens)
;;       initial-set
;;       (let [quasiclosed (map #(context-attribute-quasiclosure ctx %)
;;                              @minimal-nongens)]
;;         (println quasiclosed)
;;         (if (not (exists [set quasiclosed] (proper-subset? set initial-set)))
;;           (context-attribute-quasiclosure ctx initial-set)
;;           intent)))))

;; (defn pseudo-stem-base
;;   "Returns the base of implications with pseudoclosed premise."
;;   [ctx]
;;   (reduce! (fn [impls set]
;;              (if (= set (context-attribute-closure ctx set))
;;                impls
;;                (conj! impls
;;                       (make-implication set
;;                                         (context-attribute-closure ctx set)))))
;;            #{}
;;            (all-closed-sets (attributes ctx)
;;                             #(context-attribute-pseudoclosure ctx %))))

;;;

nil
