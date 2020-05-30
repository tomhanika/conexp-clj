(ns lattices-as-graphs
  (:require [ubergraph.core :as uber])
  (:use conexp.fca.contexts)
  (:use conexp.fca.lattices)
  (:use conexp.util.graph)
  (:use conexp.fca.graph))

(defn show
  [g]
  (uber/pprint g)
  ;(uber/viz-graph g) ; needs graphViz, i.e.: `apt install graphviz`
  )

(println "### lattice->graph ###")
(println "make a context")
(def ctx-1 (make-context [1 2 3] [1 2 3] <=))
(println ctx-1)

(println "generate a lattice from it")
(def lat-1 (concept-lattice ctx-1))
(println lat-1 "\n")

(println "convert the lattice to a directed graph")
(def gph-1 (lattice->graph lat-1))
(show gph-1)


(println "\n### graph->lattice ###")
(println "make a digraph:")
(def gph-2 (uber/digraph [1 2] [2 3] [1 4] [3 5] [4 5]))
(show gph-2)

(println "make it transitive and add loops:")
(def gph-3 (add-loops (transitive-closure gph-2)))
(show gph-3)

(println "convert to lattice: (graph->lattice checks if the lattice is valid,
  use graph->lattice-nc if performance is important)")
(def lat-3 (graph->lattice gph-3))
(println lat-3)

(println "standard context:")
(def ctx-3 (standard-context lat-3))
(println ctx-3)

(println "bijection:")
(println (-> lat-1 lattice->graph graph->lattice))
(show (-> gph-1 graph->lattice lattice->graph))

(println "### comparability ###")
(def gph-comp-3 (comparability lat-3))
(show gph-comp-3)
(def gph-co-comp-3 (co-comparability lat-3))
(show gph-co-comp-3)

(def strings #{"a" "ab" "abcd" "cde" "cd" "defa"})
(def substr #(.contains %2 %1))
(def strict-substr (strict substr))
(def gph-4 (comparability strings strict-substr))
(println "comparable:")
(show gph-4)

(def gph-5 (co-comparability strings strict-substr))
(println "non-comparable:")
(show gph-5)


nil
