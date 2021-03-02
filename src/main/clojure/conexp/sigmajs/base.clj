(ns conexp.sigmajs.base
  (:require [loom.graph :as lg]
            [conexp.fca.lattices :as lat]
            [conexp.util.graph :exclude [transitive-closure] :refer :all])
  (:use conexp.base))


;;;

(defn graph->json
  "Given a graph g (loom), returns the json representation of the
  graph as required by https://sigmajs.org/."
  ([g]
   (graph->json g (fn [_] [(clojure.core/rand) (clojure.core/rand)])))
  ([g node-positions]
   (let [nodes (lg/nodes g)
         edges (lg/edges g)
         node->str (fn [n] (let [pos (node-positions n)]
                             (str "    {
      \"id\": \"" n "\",
      \"label\": \"" n "\",
      \"x\": " (first pos) ",
      \"y\": " (last pos) "
    }")))
         edge->str (fn [e id] (str "    {
      \"id\": \"" id "\",
      \"source\": \"" (lg/src e) "\",
      \"target\": \"" (lg/dest e) "\"
    }"))]
     (str
       "{\n  \"nodes\": [\n"
       (if nodes (node->str (first nodes)))
       (apply str (map #(str ",\n" (node->str %)) (drop 1 nodes)))
       "\n  ],\n  \"edges\": [\n"
       (if edges (edge->str (first edges) 0))
       (apply str (map (fn [e id] (str ",\n" (edge->str e id))) 
                       (drop 1 edges)
                       (drop 1(range (count edges)))))
       "\n  ]\n}"
       ))))


;;;

nil
