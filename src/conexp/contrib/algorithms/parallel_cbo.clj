;; Copyright (c) Daniel Borchmann. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.contrib.algorithms.parallel-cbo
  (:use conexp.main
        conexp.contrib.exec)
  (:use [clojure.string :only (split-lines)]))

(ns-doc "Defines an external binding to the program «pcbo» by Petr
Krajca, Jan Outrata, and Vilem Vychodil, which can compute context
intents in parallel.")

;;;

(define-external-program pcbo
  pcbo :threads :depth :min-support :input-file :fcalgs)
(alter-meta! (var pcbo) assoc :private true)

(defn- to-fcalgs-context
  "Transforms ctx to a context suitable for the :fcalgs context
  format, returns a vector [context object-vector attribute-vector]."
  [ctx]
  [ctx [] []])

(defn concept-intents-by-parallel-cbo
  "Computes the intents of context using parallel close-by-one (CbO)."
  [threads depth min-support context]
  (let [[ctx obj-vec att-vec] (to-fcalgs-context context)]
    (map #(read-string (str "#{" % "}"))
         (split-lines (pcbo (str "-P" threads) (str "-L" depth) (str "-S" min-support) ctx)))))

;;;

nil
