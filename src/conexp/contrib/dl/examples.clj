;; Copyright (c) Daniel Borchmann. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.contrib.dl.examples
  (:use conexp
	conexp.contrib.dl.framework.syntax
	conexp.contrib.dl.framework.models
	conexp.contrib.dl.framework.boxes
	conexp.contrib.dl.languages.description-graphs))

;;;

(define-dl SimpleDL [Father Mother Male Female] [Child] [exists and])

(def dl-exp (dl-expression SimpleDL (exists Child Male)))

(define-model some-model SimpleDL
  #{John Marry Peter Jana}
  Mother #{Marry},
  Father #{John, Peter},
  Male   #{John, Peter},
  Female #{Marry, Jana},
  Child  #{[John Peter], [Marry Peter], [Peter Jana]})

(define-tbox some-tbox SimpleDL
  Grandfather (and Male (exists Child (exists Child (and))))
  Grandmother (and Female (exists Child (exists Child (and)))))

(define-tbox some-normal-tbox SimpleDL
  A (and Male Father (exists Child B)),
  B (and Female (exists Child T)),
  T (and))

(define-base-semantics SimpleDL
  [model dl-expression]
  (let [; note: dl-expression is neither compound nor primitive (i.e. not a
	; concept name and not a role name), therefore it must be a pair of
	; a TBox and a target symbol
	[tbox, target] (expression dl-expression),
	exp            (find-definition tbox target)]
    (if exp
      (interpret model (definition-expression exp))
      (illegal-argument "Not a valid expression: " (print-str exp)))))

(def ext-dl-exp (dl-expression SimpleDL [some-tbox, Grandfather]))
(def ext-dl-exp-2 (dl-expression SimpleDL (and [some-tbox, Grandfather])))

;;;

;;;

nil
