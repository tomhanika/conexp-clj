;; Copyright ⓒ the conexp-clj developers; all rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.contrib.algorithms.close-by-one
  "Defines an external binding to the program «pcbo» by Petr Krajca, Jan Outrata, and Vilem
Vychodil, which can compute context intents in parallel."
  (:use [conexp.base :only (set-of)]
        [conexp.fca.contexts :only (objects attributes incidence make-context)]
        conexp.contrib.exec
        [conexp.io.util :only (tmpfile)]
        [conexp.contrib.algorithms.util :only (string-to-ints)])
  (:use [clojure.string :only (split-lines split)]
        [clojure.java.io :only (reader)]))

;;;

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
    [(make-context new-objects new-attributes new-incidence),
     object-vector,
     attribute-vector]))

;;;

(define-external-program pcbo-external
  pcbo :threads :depth :min-support :verbosity :input-file :fcalgs :output-file)
(alter-meta! (var pcbo-external) assoc :private true)

(defn- pcbo
  "Runs pcbo and returns the file where the concepts have been written
  to."
  [threads depth min-support verbosity context]
  (let [^java.io.File output-file (tmpfile)]
    (pcbo-external (str "-P" threads)
                   (str "-L" depth)
                   (str "-S" (float (* min-support 100)))
                   (str "-V" verbosity)
                   context (.getAbsolutePath output-file))
    output-file))

(defn parallel-intents
  "Computes the intents of context using parallel close-by-one (PCbO)."
  [threads depth min-support context]
  (let [[ctx _ att-vec] (to-fcalgs-context context),
        output-file     (pcbo threads depth min-support 1 ctx),
        intents         (line-seq (reader output-file)),
        transform-back  #(string-to-ints att-vec %)]
    (if (= "" (first intents))
      (cons #{} (map transform-back (rest intents)))
      (map transform-back intents))))

(defn parallel-count-intents
  "Counts the intents of context using parallel close-by-one (PCbO)."
  [threads depth min-support context]
  (-> (pcbo threads depth min-support 1 (first (to-fcalgs-context context)))
      reader
      line-seq
      count))

;;;

(define-external-program fcbo-external
  pcbo :min-support :verbosity :input-file :fcalgs :output-file)
(alter-meta! (var fcbo-external) assoc :private true)

(defn- fcbo
  "Runs fcbo and returns the file where the concepts have been written
  to."
  [min-support verbosity context]
  (let [^java.io.File output-file (tmpfile)]
    (fcbo-external (str "-S" (float (* min-support 100)))
                   (str "-V" verbosity)
                   context (.getAbsolutePath output-file))
    output-file))

(defn fast-intents
  "Computes the intents of context using fast close-by-one (FCbO)."
  [min-support context]
  (let [[ctx _ att-vec] (to-fcalgs-context context),
        output-file     (fcbo min-support 1 ctx),
        intents         (line-seq (reader output-file)),
        transform-back  #(string-to-ints att-vec %)]
    (if (= "" (first intents))
      (cons #{} (map transform-back (rest intents)))
      (map transform-back intents))))

(defn fast-count-intents
  "Counts the intents of context using parallel close-by-one (CbO)."
  [min-support context]
  (-> (fcbo min-support 1 (first (to-fcalgs-context context)))
      reader
      line-seq
      count))

;;;

nil
