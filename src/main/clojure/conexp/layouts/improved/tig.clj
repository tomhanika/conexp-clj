(ns conexp.layouts.improved.tig
  (:require [loom.graph :as lg]
            [conexp.fca.graph :refer :all]
            [conexp.util.graph :refer :all])
  (:import [org.dimdraw.improved Tig]))

(defn- nodes-to-int-map [graph]
  (zipmap (lg/nodes graph) (range)))

(defn- adjacency-bitsets [graph node-map n]
  (let [arr (make-array java.util.BitSet n)]
    (doseq [[u v] (lg/edges graph)]
      (let [ui (get node-map u)
            vi (get node-map v)]
        (when (nil? (aget arr ui))
          (aset arr ui (java.util.BitSet. n)))
        (.set ^java.util.BitSet (aget arr ui) vi)))
    arr))

(defn tig-improved
  [graph]
  (let [nodes (vec (lg/nodes graph))
        n (count nodes)
        node-map (zipmap nodes (range n))
        adj-arr (adjacency-bitsets graph node-map n)

        result (Tig/computeTigEdges adj-arr n)

        pairs-java ^"[[I" (aget result 0)
        edges-java ^"[[I" (aget result 1)

        num-pairs (alength pairs-java)

        clojure-pairs
        (mapv (fn [i]
                (let [p (aget pairs-java i)
                      u (nth nodes (aget p 0))
                      v (nth nodes (aget p 1))]
                  [u v]))
              (range num-pairs))

        tig-edges
        (mapcat (fn [edge-arr]
                  (let [idx1 (aget edge-arr 0)
                        idx2 (aget edge-arr 1)
                        p1 (nth clojure-pairs idx1)
                        p2 (nth clojure-pairs idx2)]
                    [[p1 p2] [p2 p1]])) ;; Symmetric
                edges-java)]

    (if (zero? num-pairs)
      (lg/graph)
      (apply lg/graph tig-edges))))
