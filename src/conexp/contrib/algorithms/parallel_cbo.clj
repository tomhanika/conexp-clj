;; Copyright (c) Daniel Borchmann. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.contrib.algorithms.parallel-cbo
  (:use [conexp.main :exclude (context-intents)]
        conexp.contrib.exec
        [conexp.io.util :only (tmpfile)])
  (:use [clojure.string :only (split-lines split)]
        [clojure.java.io :only (reader)]))

(ns-doc "Defines an external binding to the program «pcbo» by Petr
Krajca, Jan Outrata, and Vilem Vychodil, which can compute context
intents in parallel.")

;;;

(define-external-program pcbo-external
  pcbo :threads :depth :min-support :input-file :fcalgs :output-file)
(alter-meta! (var pcbo-external) assoc :private true)

(defn- pcbo
  "Runs pcbo and returns the name of the file where the concepts have
  been written to."
  [threads depth min-support context]
  (let [output-file (.getAbsolutePath ^java.io.File (tmpfile))]
    (pcbo-external (str "-P" threads) (str "-L" depth) (str "-S" (float (* min-support 100)))
                   context output-file)
    output-file))

(defn- to-fcalgs-context
  "Transforms ctx to a context suitable for the :fcalgs context
  format, returns a vector [context object-vector attribute-vector]."
  [ctx]
  (let [object-vector    (vec (objects ctx)),
        attribute-vector (vec (attributes ctx)),

        new-objects      (range (count object-vector)),
        new-attributes   (range (count attribute-vector)),
        new-incidence    (set-of [g m] [g new-objects,
                                        m new-attributes,
                                        :when (contains? (incidence ctx)
                                                         [(nth object-vector g)
                                                          (nth attribute-vector m)])])]
    [(make-context new-objects new-attributes new-incidence)
     object-vector
     attribute-vector]))

(defn- string-to-int [str]
  (reduce #(+ (* 10 %1) (Character/digit ^Character %2 10)) 0 str))

(defn context-intents
  "Computes the intents of context using parallel close-by-one (CbO)."
  [threads depth min-support context]
  (let [[ctx _ att-vec] (to-fcalgs-context context),
        output-file     (pcbo threads depth min-support ctx),
        intents         (line-seq (reader output-file)),
        transform-back  #(set-of (nth att-vec (string-to-int x))
                                 [x (split % #"\s+")])]
    (if (= "" (first intents))
      (cons #{} (map transform-back (rest intents)))
      (map transform-back intents))))

(defn count-context-intents
  "Counts the intents of context using parallel close-by-one (CbO)."
  [threads depth min-support context]
  (count (line-seq (reader (pcbo threads depth min-support (first (to-fcalgs-context context)))))))

;;;

nil
