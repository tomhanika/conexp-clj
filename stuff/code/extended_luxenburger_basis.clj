;; Copyright (c) Daniel Borchmann. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(in-ns 'user)

(use 'conexp.main)

;;;

(defn add-immediate-elements
  "Adds all elements which follow from implications with premises in
  initial-set."
  [implications initial-set]
  (loop [conclusions  [],
         impls        implications,
         unused-impls []]
    (if (empty? impls)
      [(apply union initial-set conclusions) unused-impls]
      (let [impl (first impls)]
        (if (and (proper-subset? (premise impl) initial-set)
                 (not (subset? (conclusion impl) initial-set)))
          (recur (conj conclusions (conclusion impl))
                 (rest impls)
                 unused-impls)
          (recur conclusions
                 (rest impls)
                 (conj unused-impls impl)))))))

(defn pseudo-clop-by-implications
  "Returns for a given set of implications the corresponding closure
  operator whose closures are all closed and pseudo-closed sets."
  [implications]
  (fn [set]
    (loop [set   set,
           impls implications]
      (let [[new impls] (add-immediate-elements impls set)]
        (if (= new set)
          new
          (recur new impls))))))

(defn extended-iceberg-intent-set
  "Computes for the given closure operator for given minimal support minsupp the
  corresponding extended iceberg intent set, which is the iceberg
  lattice of all intents and pseudo-intents."
  [context minsupp]
  (let [mincount (round (ceil (* minsupp (count (objects context)))))]
    (all-closed-sets-in-family (fn [intent]
                                 (>= (count (attribute-derivation context intent))
                                     mincount))
                               (attributes context)
                               (pseudo-clop-by-implications (stem-base context)))))

(defn extended-luxenburger-basis
  "Computes a modified luxenburger basis, which is obtained from the
  original one by adding all implication of the stem-base of context
  which have support greater or equal to minsupp."
  [context minsupp minconf]
  (let [closures (extended-iceberg-intent-set context minsupp)]
    (for [B_1 closures,
          B_2 closures,
          :when (and (proper-subset? B_1 B_2)
                     (not (exists [C closures]
                            (and (proper-subset? B_1 C)
                                 (proper-subset? C B_2)))))
          :let [ar (make-association-rule context B_1 B_2)]
          :when (>= (confidence ar) minconf)]
      ar)))

;;;

nil
