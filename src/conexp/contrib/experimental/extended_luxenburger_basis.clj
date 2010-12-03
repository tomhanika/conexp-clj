;; Copyright (c) Daniel Borchmann. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.contrib.experimental.extended-luxenburger-basis
  (:use conexp.main))

(ns-doc "Experimental implementation of extended Luxenburger basis
computation.")

;;;

(defn extended-iceberg-intent-seq
  "Computes for the given closure operator for given minimal support minsupp the
  corresponding extended iceberg intent seq, which is the iceberg
  lattice of all intents and pseudo-intents."
  [context minsupp]
  (let [mincount (ceil (* minsupp (count (objects context))))]
    (all-closed-sets-in-family (fn [intent]
                                 (>= (count (attribute-derivation context intent))
                                     mincount))
                               (attributes context)
                               (pseudo-clop-by-implications (stem-base context)))))

(defn extended-luxenburger-basis
  "Computes a modified luxenburger basis, which is obtained from the
  original one by adding all implication of the stem-base of context
  which have support greater or equal to minsupp, and additionally
  some trivial implications of the form C ==> C."
  [context minsupp minconf]
  (let [closures (set (extended-iceberg-intent-seq context minsupp)),
        LB+DG    (set-of ar
                         [B_1 closures,
                          B_2 closures,
                          :when (and (proper-subset? B_1 B_2)
                                     (not (exists [C closures]
                                                  (and (proper-subset? B_1 C)
                                                       (proper-subset? C B_2)))))
                          :let [ar (make-association-rule context
                                                          B_1
                                                          (context-attribute-closure context B_2))]
                          :when (>= (confidence ar) minconf)]),
        X        (set-of (make-association-rule context C-pp C-pp)
                         [C (difference closures
                                        (set-of (union (premise impl)
                                                       (conclusion impl))
                                                [impl LB+DG]))
                          :let [C-pp (context-attribute-closure context C)]])]
    (union LB+DG X)))

;;;

nil
