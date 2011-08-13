;; Copyright (c) Daniel Borchmann. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.contrib.tests.dl.examples
  (:use conexp.main
        conexp.contrib.dl.framework.syntax
        conexp.contrib.dl.framework.boxes
        conexp.contrib.dl.framework.semantics
        conexp.contrib.dl.languages.description-graphs
        conexp.contrib.dl.languages.EL-gfp
        conexp.contrib.dl.languages.EL-gfp-exploration
        conexp.contrib.dl.languages.interaction)
  (:use clojure.test))

(ns-doc
 "Defines example data for testing and show how to use
  conexp.contrib.dl. Don't change anything here since the tests rely
  on this.")

;;; Initial Example

(define-dl SimpleDL [Father Mother Male Female] [HasChild] []
  :extends EL-gfp)

(def dl-exp (dl-expression SimpleDL (exists HasChild Male)))

(def dl-exp-2 (dl-expression SimpleDL (nominal John Peter)))

(with-dl SimpleDL

  (def some-model (interpretation #{John Marry Peter Jana}
                                  Mother #{Marry},
                                  Father #{John, Peter},
                                  Male   #{John, Peter},
                                  Female #{Marry, Jana},
                                  HasChild #{[John Peter], [Marry Peter], [Peter Jana]}))

  (def empty-model (interpretation #{}
                                   Mother #{}
                                   Father #{}
                                   Male   #{}
                                   Female #{}
                                   HasChild #{}))

  (def some-tbox (tbox Grandfather (and Male (exists HasChild (exists HasChild (and))))
                       Grandmother (and Female (exists HasChild (exists HasChild (and))))))

  (def some-normal-tbox (tbox A (and Male Father (exists HasChild B)),
                              B (and Female (exists HasChild T)),
                              T (and)))

  (def all-tbox (tbox All (and Male Female Mother Father (exists HasChild All))))

  (def all-cpt (dl-expression [all-tbox All]))

  (def ext-dl-exp (dl-expression [some-tbox, Grandfather]))
  (def ext-dl-exp-2 (dl-expression (and [some-tbox, Grandfather])))

  (def paper-model (interpretation [John Michelle Mackenzie Paul Linda James]
                                   Male   #{John Paul James}
                                   Female #{Michelle Mackenzie Linda}
                                   Father #{John Paul}
                                   Mother #{Michelle Linda}
                                   HasChild #{[John Mackenzie] [Michelle Mackenzie]
                                              [Paul James] [Linda James]}))

  (def small-model (interpretation [John Michelle Mackenzie]
                                   Male   #{John}
                                   Female #{Michelle Mackenzie}
                                   Mother #{Michelle}
                                   Father #{John}
                                   HasChild #{[John Mackenzie] [Michelle Mackenzie]}))

  nil)

;;; Fahrräder

(define-dl RidingDL [Fahrzeug, Fahrrad, Rad, Auto] [HatKomponente] []
  :extends EL-gfp)

(def riding-model (interpretation RidingDL
                                  [MeinFahrrad, Hinterrad, Vorderrad, FranzSeinAuto,
                                   LinkesHinterrad, RechtesHinterrad, LinkesVorderrad, RechtesVorderrad]
                                  Fahrzeug #{MeinFahrrad, FranzSeinAuto},
                                  Fahrrad  #{MeinFahrrad},
                                  Auto     #{FranzSeinAuto},
                                  Rad      #{Hinterrad, Vorderrad, LinkesHinterrad, LinkesVorderrad, RechtesHinterrad, RechtesVorderrad},
                                  HatKomponente #{[MeinFahrrad Hinterrad] [MeinFahrrad Vorderrad]
                                                  [FranzSeinAuto LinkesVorderrad] [FranzSeinAuto LinkesHinterrad]
                                                  [FranzSeinAuto RechtesVorderrad] [FranzSeinAuto RechtesHinterrad]}))

;;; Cyclic Example

(define-dl FamilyDL [Mother, Female, Father, Male] [MarriedTo, HasChild] []
  :extends EL-gfp)

(def family-all-cpt (dl-expression FamilyDL
                                   [(tbox FamilyDL
                                          All (and Father Male Mother Female (exists HasChild All))),
                                    All]))

(def parent (tbox FamilyDL
                  Child (and),
                  Partner (and (exists HasChild Child) (exists MarriedTo Self)),
                  Self (and (exists HasChild Child) (exists MarriedTo Partner))))

(def family-model (interpretation FamilyDL
                                  [John Michelle Mackenzie Paul Linda James]
                                  Male   #{John Paul James}
                                  Female #{Michelle Mackenzie Linda}
                                  Father #{John Paul}
                                  Mother #{Michelle Linda}
                                  HasChild  #{[John Mackenzie] [Michelle Mackenzie]
                                              [Paul James] [Linda James]}
                                  MarriedTo #{[Paul Linda] [Linda Paul]
                                              [John Michelle] [Michelle John]}))

(def more-family-model (interpretation FamilyDL
                                       [Jana Paul Tom Martin Kathrin Thomas Clarissa Jean Elisabeth Andrea]
                                       Female #{Jana Kathrin Clarissa Elisabeth Andrea}
                                       Male   #{Paul Tom Martin Thomas Jean}
                                       Mother #{Jana Kathrin Elisabeth}
                                       Father #{Paul Martin Jean}
                                       HasChild  #{[Jana Tom]       [Paul Tom]
                                                   [Martin Thomas]  [Martin Clarissa]
                                                   [Kathrin Thomas] [Kathrin Clarissa]
                                                   [Jean Andrea]    [Elisabeth Andrea]}
                                       MarriedTo #{[Jana Paul]      [Paul Jana]
                                                   [Martin Kathrin] [Kathrin Martin]
                                                   [Jean Elisabeth] [Elisabeth Jean]}))

(def grandparent-model (interpretation FamilyDL
                                       [A B C D E F G H]
                                       Female #{A C E G}
                                       Male   #{B D F H}
                                       Mother #{A C E}
                                       Father #{B D F}
                                       HasChild  #{[A E] [B E]
                                                   [C F] [D F]
                                                   [E G] [F G]
                                                   [E H] [F H]}
                                       MarriedTo #{[A B] [B A]
                                                   [C D] [D C]
                                                   [E F] [F E]}))

;;;

nil
