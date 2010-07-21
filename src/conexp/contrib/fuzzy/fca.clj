;; Copyright (c) Daniel Borchmann. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.contrib.fuzzy.fca
  (:use conexp.main
        [conexp.contrib.fuzzy sets logics]))

(ns-doc "Basic definitions for Fuzzy FCA")

;;;

(defn fuzzy-object-derivation
  "Computes the fuzzy derivation of the fuzzy set C of objects in
  the given context."
  [context C]
  (let [inz (incidence context)]
    (make-fuzzy-set (map-by-fn (fn [m]
                                 (reduce (fn [a g]
                                           (f-and a (f-impl (C g) (inz [g m]))))
                                         1
                                         (objects context)))
                               (attributes context)))))

(defn fuzzy-attribute-derivation
  "Computes the fuzzy derivation of the fuzzy set D of attributes in
  the given context."
  [context D]
  (let [inz (incidence context)]
    (make-fuzzy-set (map-by-fn (fn [g]
                                 (reduce (fn [a m]
                                           (f-and a (f-impl (D m) (inz [g m]))))
                                         1
                                         (attributes context)))
                               (objects context)))))

;;;

nil
