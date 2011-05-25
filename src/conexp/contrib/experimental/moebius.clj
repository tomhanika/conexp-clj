;; Copyright (c) Daniel Borchmann. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.contrib.experimental.moebius
  (:use conexp.main))

(ns-doc "Computing the Möbius function of concept lattices")

;;;

(defn moebius-function
  "Returns the Möbius function of the concept lattice of the given
  formal context ctx."
  [ctx]
  (let [objects   (objects ctx),
        dprime    (fn [X]
                    (context-object-closure ctx X)),
        mf-helper (fn mf-helper [A B]
                    (if (= A B)
                      1
                      (reduce (fn [sum X]
                                (if (proper-subset? X B)
                                  (+ sum (mf-helper A X))
                                  sum))
                              0
                              (all-closed-sets-in-family (fn [X]
                                                           (lectic-< objects X B))
                                                         objects
                                                         dprime
                                                         A))))]
    (fn [[A _] [B _]]
      (mf-helper A B))))

;;;

nil
