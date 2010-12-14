;; Copyright (c) Daniel Borchmann. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.contrib.dl.framework.reasoning
  (:use conexp.main
        conexp.contrib.dl.framework.syntax))

;;;

(defn- expressions-language
  "Returns the name of the common DL of given dl-expression C and D,
  if existent. Otherwise raises an error."
  [C D]
  (when (or (not (dl-expression? C))
            (not (dl-expression? D)))
    (illegal-argument "Arguments to subsumption must be dl-expressions."))
  (let [C-language (expression-language C),
        D-language (expression-language D)]
    (when (not= C-language D-language)
      (illegal-argument "For subsumption the expression must be formulated in the same language."))
    (language-name C-language)))

(defmulti compute-subsumption
  "Defines subsumption algorithms for specific languages."
  expressions-language)

(defmethod compute-subsumption :default [C D]
  (illegal-argument "There is no algorithm defined for subsumption in language " (print-str (expression-language C)) "."))

(defmacro define-subsumption
  "Define subsumption algorithm for given language."
  [language [C D] & body]
  `(defmethod compute-subsumption (language-name ~language) [~C ~D]
     ~@body))

(defn subsumed-by?
  "Returns true iff C is subsumed by D in the given language."
  [C D]
  (compute-subsumption C D))

;;;

(defmulti equivalent?
  "Returns true iff C and D are equivalent."
  expressions-language)

(defmethod equivalent? :default [C D]
  (and (subsumed-by? C D)
       (subsumed-by? D C)))

(defmacro define-equivalence
  "Define equivalence algorithm for given language."
  [language [C D] & body]
  `(defmethod equivalent? (language-name ~language) [~C ~D]
     ~@body))


;;;

nil
