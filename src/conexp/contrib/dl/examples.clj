;; Copyright (c) Daniel Borchmann. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.contrib.dl.examples
  (:use conexp
	conexp.contrib.dl.base))

(update-ns-meta! conexp.contrib.dl.examples
  :doc "Examples of some description logics.")

;;;

(define-dl Simple-DL [Mother Father Male Female] [hasChild] [and exists]
  and (fn [model C D]
	(intersection (interpret model C)
		      (interpret model D))),
  exists (fn [model r C]
	   (set-of x [x (model-base-set model),
		      :when (exists [y (interpret model C)]
			      (contains? (interpret model r) [x y]))])))

(def some-model (make-model Simple-DL
			    '#{John Marry Peter}
			    '{Mother   #{Marry},
			      Father   #{John, Peter},
			      Male     #{John, Peter},
			      Female   #{Marry, Jana},
			      hasChild #{[John Peter], [Marry Peter],
					 [Peter Jana]}}))

(def dl-exp-1 (make-dl-expression Simple-DL '(exists hasChild Male)))
(def dl-exp-2 (make-dl-expression Simple-DL 'Father))

;;;

nil
