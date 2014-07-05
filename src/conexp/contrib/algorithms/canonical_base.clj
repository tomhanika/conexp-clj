;; Copyright (c) Daniel Borchmann. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(in-ns 'conexp.contrib.algorithms)

(use 'conexp.contrib.algorithms.bitwise
     '[conexp.fca.implications :only (premise conclusion implication? make-implication)]
     '[conexp.base :only (next-closed-set-in-family subset? forall)]
     '[conexp.fca.contexts :only (attributes context-attribute-closure)])

;;; Computing Implicational Closures

(defn- add-immediate-elements
  [implications initial-set subset-test]
  (loop [conclusions  (transient initial-set),
         impls        implications,
         unused-impls (transient [])]
    (if-let [impl (first impls)]
      (if (subset-test (premise impl) initial-set)
        (recur (reduce conj! conclusions (conclusion impl))
               (rest impls)
               unused-impls)
        (recur conclusions
               (rest impls)
               (conj! unused-impls impl)))
      [(persistent! conclusions)
       (persistent! unused-impls)])))

(defn close-under-implications
  "Computes smallest superset of set being closed under given implications."
  [implications set]
  (assert (set? set))
  (loop [set   set,
         impls implications]
    (let [[new impls] (add-immediate-elements impls set subset?)]
      (if (= new set)
        new
        (recur new impls)))))

(defn clop-by-implications
  "Returns closure operator given by implications."
  [implications]
  (partial close-under-implications implications))


;;; Canonical Base

(defn canonical-base-from-clop
  ([clop base]
     (canonical-base-from-clop clop base #{} (constantly true)))
  ([clop base background-knowledge]
     (canonical-base-from-clop clop base background-knowledge (constantly true)))
  ([clop base background-knowledge predicate]
     (assert (fn? clop)
             "Given closure operator must be a function")
     (assert (coll? base)
             "Base must be a collection")
     (assert (fn? predicate)
             "Predicate must be a function")
     (assert (and (set? background-knowledge)
                  (forall [x background-knowledge]
                    (implication? x)))
             "Background knowledge must be a set of implications")
     (let [next-closure (fn [implications last]
                          (next-closed-set-in-family predicate
                                                     base
                                                     (clop-by-implications implications)
                                                     last)),
           runner       (fn runner [implications candidate]
                          (when candidate
                            (let [conclusions (clop candidate)]
                              (if (not= candidate conclusions)
                                (let [impl  (make-implication candidate conclusions),
                                      impls (conj implications impl)]
                                  (cons impl
                                        (lazy-seq (runner impls (next-closure impls candidate)))))
                                (recur implications (next-closure implications candidate))))))]
       (lazy-seq (runner background-knowledge
                         (close-under-implications background-knowledge #{}))))))

(defn canonical-base
  ([ctx]
     (canonical-base ctx #{} (constantly true)))
  ([ctx background-knowledge]
     (canonical-base ctx background-knowledge (constantly true)))
  ([ctx background-knowledge predicate]
     (assert (context? ctx)
             "First argument must be a formal context")
     (canonical-base-from-clop #(context-attribute-closure ctx %)
                               (attributes ctx)
                               background-knowledge
                               predicate)))

;;;

nil
