;; Copyright (c) Daniel Borchmann. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.contrib.tests.dl.examples
  (:use conexp
	conexp.contrib.dl.framework.syntax
	conexp.contrib.dl.framework.models
	conexp.contrib.dl.framework.boxes
	conexp.contrib.dl.languages.exploration
	conexp.contrib.dl.languages.interaction
	conexp.contrib.dl.languages.EL-gfp))

;;; Initial Example

(define-dl SimpleDL [Father Mother Male Female] [Child] []
  :extends EL-gfp)

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

(def ext-dl-exp (dl-expression SimpleDL [some-tbox, Grandfather]))
(def ext-dl-exp-2 (dl-expression SimpleDL (and [some-tbox, Grandfather])))

(define-model paper-model SimpleDL
  [John Michelle Mackenzie Paul Linda James]
  Male   #{John Paul James}
  Female #{Michelle Mackenzie Linda}
  Father #{John Paul}
  Mother #{Michelle Linda}
  Child  #{[John Mackenzie] [Michelle Mackenzie]
	   [Paul James] [Linda James]})

(define-model small-model SimpleDL
  [John Michelle Mackenzie]
  Male   #{John}
  Female #{Michelle Mackenzie}
  Mother #{Michelle}
  Father #{John}
  Child  #{[John Mackenzie] [Michelle Mackenzie]})

;;; Fahrräder

(define-dl RidingDL [Fahrzeug, Fahrrad, Rad, Auto] [HatKomponente] []
  :extends EL-gfp)

(define-model riding-model RidingDL
  [MeinFahrrad, Hinterrad, Vorderrad, FranzSeinAuto, LinkesHinterrad, RechtesHinterrad, LinkesVorderrad, RechtesVorderrad]
  Fahrzeug #{MeinFahrrad, FranzSeinAuto},
  Fahrrad  #{MeinFahrrad},
  Auto     #{FranzSeinAuto},
  Rad      #{Hinterrad, Vorderrad, LinkesHinterrad, LinkesVorderrad, RechtesHinterrad, RechtesVorderrad},
  HatKomponente #{[MeinFahrrad Hinterrad] [MeinFahrrad Vorderrad]
		  [FranzSeinAuto LinkesVorderrad] [FranzSeinAuto LinkesHinterrad]
		  [FranzSeinAuto RechtesVorderrad] [FranzSeinAuto RechtesHinterrad]})

;;; Cyclic Example

(define-dl FamilyDL [Mother, Female, Father, Male] [MarriedTo, HasChild] []
  :extends EL-gfp)

(define-model family-model FamilyDL
  [John Michelle Mackenzie Paul Linda James]
  Male   #{John Paul James}
  Female #{Michelle Mackenzie Linda}
  Father #{John Paul}
  Mother #{Michelle Linda}
  HasChild  #{[John Mackenzie] [Michelle Mackenzie]
	      [Paul James] [Linda James]}
  MarriedTo #{[Paul Linda] [Linda Paul]
	      [John Michelle] [Michelle John]})

;;;

nil
