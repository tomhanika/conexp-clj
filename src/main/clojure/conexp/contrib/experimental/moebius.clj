;; Copyright ⓒ the conexp-clj developers; all rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.contrib.experimental.moebius
  "Computing the Möbius function of concept lattices"
  (:use conexp.base
        conexp.fca.contexts))

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

(defn egen-unit-element
  [ctx]
  (let [ctx     (make-context (objects ctx)
                              (difference (attributes ctx)
                                          (set-of m | m (attributes ctx)
                                                      :when (< 1 (count (context-attribute-closure ctx #{m}))))
                                          (context-attribute-closure ctx #{}))
                              (incidence ctx)),
        intents (intents ctx)]
    (loop [µ       {(first intents) 1},
           intents (rest intents)]
      (if-let [A (first intents)]
        (let [µ (assoc µ A 0)]
          (recur (reduce (fn [µ X]
                           (assoc µ A (- (µ A) (µ X))))
                         µ
                         (all-closed-sets-in-family #(proper-subset? % A)
                                                    (attributes ctx)
                                                    #(context-attribute-closure ctx %)))
                 (rest intents)))
        (reduce (fn [sum [A µ-A]]
                  (+ sum (* µ-A (expt 2 (count (aprime ctx A))))))
                0
                µ)))))

;;;

nil
