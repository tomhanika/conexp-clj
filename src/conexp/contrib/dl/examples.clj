;; Copyright (c) Daniel Borchmann. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.contrib.dl.examples
  (:use conexp.contrib.dl.framework.syntax
	conexp.contrib.dl.framework.models
	conexp.contrib.dl.framework.boxes))

;;;

(define-dl SimpleDL [Father Mother Male Female] [child] [exists and])

(def dl-exp (make-dl-expression SimpleDL '(exists child Male)))

(define-model some-model SimpleDL
  #{John Marry Peter Jana}
  {Mother #{Marry},
   Father #{John, Peter},
   Male   #{John, Peter},
   Female #{Marry, Jana},
   child  #{[John Peter], [Marry Peter], [Peter Jana]}})

(define-tbox some-tbox SimpleDL
  Grandfather (and Male (exists child (exists child (and)))))

(define-base-semantics SimpleDL
  [model dl-expression]
  ;; note: dl-expression is neither compound nor primitive (i.e. not a
  ;; concept name and not a role name)
  (let [[tbox, target] (expression dl-expression),
	exp    (first (filter #(= (definition-target %) target)
			      (tbox-definitions tbox)))]
    (if exp
      (interpret model (definition-expression exp))
      (throw (IllegalArgumentException. (str "Not a valid expression: " (print-str exp)))))))

(def ext-dl-exp (make-dl-expression SimpleDL [some-tbox, 'Grandfather]))
(def ext-dl-exp-2 (make-dl-expression SimpleDL (list 'and [some-tbox, 'Grandfather])))

;;;

;;;

nil
