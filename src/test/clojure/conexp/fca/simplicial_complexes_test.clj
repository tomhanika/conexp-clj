;; Copyright ⓒ the conexp-clj developers; all rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.fca.simplicial-complexes-test
  (:use clojure.test)
  (:require [conexp.base :refer [with-testing-data]]
            [conexp.fca.contexts :refer [attributes
                                         incidence
                                         make-context
                                         make-context-from-matrix
                                         objects
                                         random-contexts
                                         object-concept
                                         attribute-concept]]
            [conexp.fca.simplicial-complexes :refer :all]
            [conexp.fca.lattices :refer [concept-lattice]]
            [conexp.fca.ordinal-motifs :refer [generate-scale]]
            [conexp.fca.smeasure :refer [make-smeasure-nc
                                         smeasure?]]
            [conexp.io.contexts :refer [read-context]])
    (:import conexp.fca.simplicial_complexes.FullSimplicialComplex))

(deftest test-FullSimplicialComplex-equals
  (is (= (FullSimplicialComplex. #{} #{})
         (FullSimplicialComplex. #{} #{})))
  (is (= (FullSimplicialComplex. #{1 2 3} #{#{} #{1} #{2} #{3}})
         (FullSimplicialComplex. #{1 2 3} #{#{} #{1} #{2} #{3}})))
  (is (= (FullSimplicialComplex. #{1 2 3} #{#{} #{1} #{2} #{3}})
         (FullSimplicialComplex. #{1 2 3} #{#{} #{3} #{2} #{1}})))
  (is (= (FullSimplicialComplex. #{1 2 3} #{#{} #{1} #{2} #{3}})
         (FullSimplicialComplex. #{1 2 3} '(#{} #{3} #{2} #{1}))))
  (is (= (FullSimplicialComplex. #{1 2 3} #{#{} #{1} #{2} #{3}})
         (FullSimplicialComplex. #{1 2 3} [#{} #{3} #{2} #{1}])))
  (is (not= (FullSimplicialComplex. #{1 2 3} #{#{} #{1} #{2} #{3}})
            (FullSimplicialComplex. #{1 2 3} #{#{} #{1} #{2} #{3} #{1 2}})))
  (is (not= (FullSimplicialComplex. #{1 2 3} #{#{} #{1} #{2} #{3}})
            (FullSimplicialComplex. #{1 2 3 4} #{#{} #{1} #{2} #{3}})))
  (is (not= (FullSimplicialComplex. #{} #{})
            (Object.))))

(deftest test-FullSimplicialComplex-hashCode
  (let [simplicial-complex-1 (FullSimplicialComplex. #{1 2} #{#{} #{1} #{2} #{1 2}})
        simplicial-complex-2 (FullSimplicialComplex. #{1 2} #{#{} #{1} #{2} #{1 2}})
        simplicial-complex-3 (FullSimplicialComplex. #{1 2} #{#{} #{1} #{2}})]
    (is (= (hash simplicial-complex-1) (hash simplicial-complex-1)))
    (is (= (hash simplicial-complex-1) (hash simplicial-complex-2)))
    (is (not= (hash simplicial-complex-1) (hash simplicial-complex-3)))))

(deftest test-FullSimplicialComplex-toString
  (is (= (str (FullSimplicialComplex. #{1} #{#{1}}))
         "#{#{1}}"))
  (is (= (str (FullSimplicialComplex. #{1} [#{1}]))
         "#{#{1}}")))

(deftest test-make-full-simplicial-complex-nc
  (is (= (make-full-simplicial-complex-nc #{1 2 3} [#{} #{1} #{2} #{3}])
         (FullSimplicialComplex. #{1 2 3} #{#{} #{1} #{2} #{3}})))
  (is (= (make-full-simplicial-complex-nc [#{} #{1} #{2} #{3}])
         (FullSimplicialComplex. #{1 2 3} #{#{} #{1} #{2} #{3}})))
  (is (= (make-full-simplicial-complex-nc  [#{} #{1} #{2} #{3} #{1 2} #{1 3}])
         (FullSimplicialComplex. #{1 2 3} #{#{} #{1} #{2} #{3} #{1 2} #{1 3}})))
  (is (thrown? IllegalArgumentException (make-full-simplicial-complex-nc 0))))

(deftest test-is-simplicial-complex
  (is (is-simplicial-complex? 
       (make-full-simplicial-complex-nc #{#{} #{1} #{2} #{3} #{1 2} #{1 3}})))
  (is (is-simplicial-complex? 
       (make-full-simplicial-complex-nc #{1 2 3} [#{} #{1} #{2} #{3}])))
  (is (not (is-simplicial-complex? 
            (make-full-simplicial-complex-nc #{#{} #{2} #{3} #{1 2} #{1 3}}))))
  (is (not (is-simplicial-complex? 
            (make-full-simplicial-complex-nc #{#{1} #{2} #{3} #{1 2} #{1 3}}))))
  (is (is-simplicial-complex? 
       (make-full-simplicial-complex-nc #{1 2 3} #{#{} #{1} #{2} #{3} #{1 2} #{1 3}})))
  (is (is-simplicial-complex? 
       (make-full-simplicial-complex-nc #{1 2 3 4} #{#{} #{1} #{2} #{3} #{1 2} #{1 3}})))
  (is (not (is-simplicial-complex? 
            (make-full-simplicial-complex-nc #{1 2} #{#{} #{1} #{2} #{3} #{1 2} #{1 3}})))))

(deftest test-make-full-simplicial-complex
  (is (= (make-full-simplicial-complex #{1 2 3} [#{} #{1} #{2} #{3}])
         (FullSimplicialComplex. #{1 2 3} #{#{} #{1} #{2} #{3}})))
  (is (= (make-full-simplicial-complex [#{} #{1} #{2} #{3}])
         (FullSimplicialComplex. #{1 2 3} #{#{} #{1} #{2} #{3}})))
  (is (= (make-full-simplicial-complex [#{} #{1} #{2} #{3} #{1 2} #{1 3}])
         (FullSimplicialComplex. #{1 2 3} #{#{} #{1} #{2} #{3} #{1 2} #{1 3}})))
  (is (thrown? IllegalArgumentException (make-full-simplicial-complex [#{} #{1} #{2} #{1 2} #{1 3}])))
  (is (thrown? IllegalArgumentException (make-full-simplicial-complex #{1 2} [#{} #{1} #{2} #{3} #{1 2} #{1 3}])))
  (is (thrown? IllegalArgumentException (make-full-simplicial-complex-nc 0))))

;; FCA

(def ctx (make-context-from-matrix [0 1 2 3] ['a 'b 'c 'd] 
                                   [1 1 1 1 1 0 0 0 0 1 0 1 1 0 1 1]))
(def ctx2 (make-context-from-matrix [0 1 2 3 4] ['a 'b 'c 'd] 
                                   [1 1 1 1 1 0 0 0 0 1 0 1 1 0 1 1 1 1 0 0 ]))

(deftest test-t-simplex-next-closure
  (is (= (t-simplex-next-closure ctx [#{0 3} #{'a 'c 'd}])
         (FullSimplicialComplex. #{[#{0} #{'a 'b 'c 'd}] [#{0 2} #{'b 'd}]
                                   [#{0 3} #{'a 'c 'd}] [#{0 2 3} #{'d}]
                                   [#{0 1 3} #{'a}] [#{0 1 2 3} #{}]}
                                 #{#{} #{[#{0} #{'a 'b 'c 'd}]} #{[#{0 2} #{'b 'd}]}
                                   #{[#{0} #{'a 'b 'c 'd}] [#{0 2} #{'b 'd}]}})))
  (is (= (t-simplex-next-closure ctx [#{0 1 3} #{'a}])
         (FullSimplicialComplex. #{[#{0} #{'a 'b 'c 'd}] [#{0 2} #{'b 'd}]
                                   [#{0 3} #{'a 'c 'd}] [#{0 2 3} #{'d}]
                                   [#{0 1 3} #{'a}] [#{0 1 2 3} #{}]}
                                 #{#{} #{[#{0} #{'a 'b 'c 'd}]} #{[#{0 2} #{'b 'd}]}
                                   #{[#{0 3} #{'a 'c 'd}]} #{[#{0 2 3} #{'d}]}
                                   #{[#{0} #{'a 'b 'c 'd}] [#{0 2} #{'b 'd}]}
                                   #{[#{0} #{'a 'b 'c 'd}] [#{0 3} #{'a 'c 'd}]}
                                   #{[#{0} #{'a 'b 'c 'd}] [#{0 2 3} #{'d}]}
                                   #{[#{0 2} #{'b 'd}] [#{0 2 3} #{'d}]}
                                   #{[#{0 3} #{'a 'c 'd}] [#{0 2 3} #{'d}]}
                                   #{[#{0 2} #{'b 'd}] [#{0 3} #{'a 'c 'd}]}
                                   #{[#{0} #{'a 'b 'c 'd}] [#{0 2} #{'b 'd}] [#{0 2 3} #{'d}]}
                                   #{[#{0} #{'a 'b 'c 'd}] [#{0 3} #{'a 'c 'd}] [#{0 2 3} #{'d}]}
                                   #{[#{0 2} #{'b 'd}] [#{0 2 3} #{'d}] [#{0 3} #{'a 'c 'd}]}
                                   #{[#{0} #{'a 'b 'c 'd}] [#{0 2} #{'b 'd}] [#{0 3} #{'a 'c 'd}]}
                                   #{[#{0} #{'a 'b 'c 'd}] [#{0 2} #{'b 'd}] [#{0 2 3} #{'d}] [#{0 3} #{'a 'c 'd}]}})))
  (is (thrown? IllegalArgumentException (t-simplex-next-closure [#{0 3} #{'a 'c 'd}] [#{0 3} #{'a 'c 'd}]))))

(deftest test-ordinal-motif-sample-cases
  (is (= (ordinal-motif-next-closure ctx :ordinal)
         (FullSimplicialComplex. #{0 1 2 3}
                                 #{#{} #{0} #{1} #{2} #{3}
                                   #{0 1} #{0 2} #{0 3} #{1 3} #{0 1 3}})))
  (let [ctx1 (make-context-from-matrix [0 1 2 3] ['a 'b 'c 'd]
                                       [1 1 0 0 1 0 1 0 0 1 0 1 1 1 0 1])
        ctx2 (make-context-from-matrix [0 1 2 3] [0 1 2 3] [1 1 1 1 0 1 1 1 0 0 1 1 0 0 0 1])]
    (is (= (ordinal-motif-next-closure ctx1 :ordinal)
           (FullSimplicialComplex. #{0 1 2 3}
                                   #{#{} #{0} #{1} #{2} #{3} #{0 3} #{2 3}})))
    (is (= (ordinal-motif-next-closure ctx2 :ordinal)
           (FullSimplicialComplex. #{0 1 2 3}
                                   #{#{} #{0} #{1} #{2} #{3} 
                                     #{0 1} #{0 2} #{0 3} #{1 2} #{1 3} #{2 3}
                                     #{0 1 2} #{0 1 3} #{0 2 3} #{1 2 3} #{0 1 2 3}}))))
  (is (= (ordinal-motif-next-closure ctx :interordinal)
         (FullSimplicialComplex. #{0 1 2 3}
                                 #{#{} #{0} #{1} #{2} #{3}
                                   #{1 2} #{2 3}})))
  ;; Test case similar to ordinal-motifs branch.
  (let [ctx (make-context-from-matrix [0 1 2 3] 
                                      ['a 'b 'c 'd]
                                      [1 1 0 0 1 0 1 0 0 1 0 1 1 1 0 1])]
    (are [type simplices]
        (= (ordinal-motif-next-closure ctx type) 
           (FullSimplicialComplex. #{0 1 2 3}
                                   simplices))
      :ordinal #{#{} #{0} #{1} #{2} #{3} #{0 3} #{2 3}}
      :interordinal #{#{} #{0} #{1} #{2} #{3} #{0 1} #{0 2} #{1 2} #{1 3} #{0 1 2}}
      :nominal #{#{} #{0} #{1} #{2} #{3} #{1 3} #{1 2} #{0 2} #{0 1}}
      :contranominal #{#{} #{0} #{1} #{2} #{3} #{0 1} #{0 2} #{1 2} #{1 3}})))

(defn- has-smeasure?
  ;; Check if given context has a scale measure of given scale type.
  [ctx scale-type]
  (let [smeasures (map 
                   #(make-smeasure-nc 
                     ctx 
                     (generate-scale scale-type %)
                     (zipmap (objects ctx) (range 1 (inc %))))
                   (range (inc (count (objects ctx)))))]
    (some #(smeasure? %) smeasures)))

(deftest test-ordinal-motif-next-closure
  "Test the ordinal-motif-next-closure method by testing if for all
  subcontexts that contain simplices as objects there is a (local)
  scale-measure to the given scale-type."
  (let [contexts (random-contexts 10 10)]
    (with-testing-data [ctx contexts,
                        scale-type [:nominal :contranominal :ordinal :interordinal]]
      (let [ordinal-motifs (ordinal-motif-next-closure ctx scale-type)
            subcontexts (map #(make-context % (attributes ctx) (incidence ctx)) 
                             (simplices ordinal-motifs))
            smeasures (map 
                       #(make-smeasure-nc 
                         % 
                         (generate-scale scale-type (count (objects %)))
                         (zipmap (objects %) (range 1 (inc (count (objects %))))))
                       subcontexts)]
        (every? #(has-smeasure? % scale-type) subcontexts))))
  (is (thrown? IllegalArgumentException (ordinal-motif-next-closure ctx :other))))

;;; Tests for simplicial complex analytics

(deftest test-face-dimension
  (let [face1  #{[#{0} #{'a 'b 'c 'd}]}
        face2  #{[#{0} #{'a 'b 'c 'd}] [#{0 2} #{'b 'd}]}]
    (is (= (face-dimension face1) 0))
    (is (= (face-dimension face2) 1))))

(deftest test-complex-dimension
  (let [ctx   (read-context "testing-data/bodiesofwater.cxt")
        sc1 (t-simplex-next-closure ctx (object-concept ctx "puddle"))
        sc2 (t-simplex-next-closure ctx (object-concept ctx "reservoir"))
        sc3 (t-simplex-next-closure ctx (object-concept ctx "lagoon"))]
    (is (= (complex-dimension sc1) 7))
    (is (= (complex-dimension sc2) 6))
    (is (= (complex-dimension sc3) 2))))

(deftest test-sc-matrix-rep
  (let [sc1 (t-simplex-next-closure ctx (object-concept ctx 1))
        sc2 (t-simplex-next-closure ctx (object-concept ctx 2))
        sc3 (t-simplex-next-closure ctx (attribute-concept ctx 'd))]
    (is (= (sc-matrix-rep sc1 1)
           [[1 1 0 1 0 0] [1 0 1 0 1 0] [0 1 1 0 0 1] [0 0 0 1 1 1]]))
    (is (= (sc-matrix-rep sc1 2)
           [[1 1 0 0] [1 0 1 0] [1 0 0 1] [0 1 1 0] [0 1 0 1] [0 0 1 1]]))
    (is (= (sc-matrix-rep sc1 3)
           [[1] [1] [1] [1]]))
    (is (= (sc-matrix-rep sc1 4)
           [[]]))
    (is (= (sc-matrix-rep sc2 1)
           [[1 1 0] [1 0 1] [0 1 1]]))
    (is (= (sc-matrix-rep sc2 2)
           [[1] [1] [1]]))
    (is (= (sc-matrix-rep sc2 0)
           [[1 1 1]]))
    (is (= (sc-matrix-rep sc3 2)
           [[0] [1] [1] [1]]))
    (is (= (sc-matrix-rep sc3 1)
          [[1 0 0 0] [1 1 1 0] [0 1 0 1] [0 0 1 1]]))))

(deftest test-sc-chain-complex
  (let [chain [(object-concept ctx 3) (attribute-concept ctx 'd)]
        sc1 (t-simplex-next-closure ctx (nth chain 0))
        sc2 (t-simplex-next-closure ctx (nth chain 1))
        chain2 [(object-concept ctx2 2) (attribute-concept ctx 'd)] ]
    (is (= (sc-chain-complex (concept-lattice ctx) chain 2)
           [[[#{0 3} #{'a 'c 'd}] [] [[]]] [[#{0 3 2} #{'d}] [[]] [[0] [1] [1] [1]]]]))
    (is (= (sc-chain-complex (concept-lattice ctx2) chain2 2)
           [[[#{0 2} #{'b 'd}] [[1] [1] [1] [1]] [[1 1 0 0] [1 0 1 0] [1 0 0 1] [0 1 1 0] [0 1 0 1] [0 0 1 1]]] [[#{0 3 2} #{'d}] [[0 1] [1 0] [1 0] [1 0] [1 0] [0 1] [0 1] [0 1]] [[1 0 0 0 0 1 0 0] [1 0 0 0 0 0 1 0] [1 1 1 0 0 0 0 1] [0 1 0 1 0 0 0 0] [0 1 0 0 1 0 0 0] [0 0 1 1 0 0 0 0] [0 0 1 0 1 0 0 0] [0 0 0 1 1 0 0 0] [0 0 0 0 0 1 1 0] [0 0 0 0 0 1 0 1] [0 0 0 0 0 0 1 1]]]]))))
