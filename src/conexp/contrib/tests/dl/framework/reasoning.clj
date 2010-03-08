;; Copyright (c) Daniel Borchmann. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.contrib.tests.dl.framework.reasoning
  (:use conexp
	conexp.contrib.dl.framework.syntax
	conexp.contrib.dl.framework.boxes
	conexp.contrib.dl.framework.reasoning
	conexp.contrib.dl.languages.EL-gfp)
  (:use clojure.test))

;;;

(define-dl FamilyDL [Mother, Female, Father, Male] [MarriedTo, HasChild] []
  :extends EL-gfp)

(def parent* (make-tbox FamilyDL #{(make-dl-definition 'Child (dl-expression FamilyDL (and))),
				   (make-dl-definition 'Partner (dl-expression FamilyDL
									       (and (exists HasChild Child)
										    (exists MarriedTo Self))))
				   (make-dl-definition 'Self (dl-expression FamilyDL
									    (and (exists HasChild Child)
										 (exists MarriedTo Partner))))}))
(deftest test-subsumed-by?
  (are [dl exp] (subsumed-by? (dl-expression dl exp) (dl-expression dl exp))
       FamilyDL (exists HasChild (and Mother Female (exists HasChild (and))))
       FamilyDL Mother)
  (are [dl exp-1 exp-2] (subsumed-by? (dl-expression dl exp-1) (dl-expression dl exp-2))
       FamilyDL (exists HasChild Female) (exists HasChild (and)),
       FamilyDL (and Female Mother) (and Female)
       FamilyDL [parent* Self] (exists MarriedTo [parent* Self])))
;;;

nil
