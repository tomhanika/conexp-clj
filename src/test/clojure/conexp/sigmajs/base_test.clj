;; Copyright ⓒ the conexp-clj developers; all rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.sigmajs.base-test
  "Tests for the sigma.js serialization.

  The output is checked by parsing it rather than by matching it against a
  regular expression.  The old regular expression pinned the node order and
  required every coordinate to look like `[0-9.]+`, but `graph->json` places
  nodes at `(rand)` by default, and Java prints a double below 0.001 in
  scientific notation, as in `9.25937777300545E-4`.  That made the test fail on
  0.6% of runs, measured over 20000 builds."
  (:require [clojure.data.json :as json]
            [loom.graph :as lg])
  (:use clojure.test
        conexp.sigmajs.base))

(def ^:private test-graph (lg/digraph [:a :b] [:b :c]))

(deftest test-graph->json-is-valid-json
  (let [parsed (json/read-str (graph->json test-graph))]
    (is (= #{"nodes" "edges"} (set (keys parsed))))))

(deftest test-graph->json-nodes
  (let [nodes (get (json/read-str (graph->json test-graph)) "nodes")]
    (testing "one entry per node of the graph, labelled by itself"
      (is (= #{":a" ":b" ":c"} (set (map #(get % "id") nodes))))
      (is (every? #(= (get % "id") (get % "label")) nodes)))
    (testing "every node is placed somewhere"
      (is (every? #(and (number? (get % "x")) (number? (get % "y"))) nodes)))))

(deftest test-graph->json-edges
  (let [edges (get (json/read-str (graph->json test-graph)) "edges")]
    (testing "one entry per edge, joining the nodes the graph joins"
      (is (= #{[":a" ":b"] [":b" ":c"]}
             (set (map (juxt #(get % "source") #(get % "target")) edges)))))
    (testing "the ids distinguish the edges"
      (is (= (count edges) (count (distinct (map #(get % "id") edges))))))))

(deftest test-graph->json-uses-the-given-positions
  (testing "positions come from the supplied function, which is what makes a
            drawn layout serializable"
    (let [nodes (-> (graph->json test-graph (fn [n] [(count (name n)) 7]))
                    json/read-str
                    (get "nodes"))]
      (is (every? #(= 7 (get % "y")) nodes))
      (is (every? #(= (count (subs (get % "id") 1)) (get % "x")) nodes)))))

(deftest test-graph->json-survives-small-coordinates
  (testing "a coordinate small enough that Java prints it in scientific
            notation still has to parse back as the number it was"
    (let [parsed (-> (graph->json test-graph (fn [_] [9.25937777300545E-4 1.0E-7]))
                     json/read-str
                     (get "nodes"))]
      (is (every? #(= 9.25937777300545E-4 (get % "x")) parsed))
      (is (every? #(= 1.0E-7 (get % "y")) parsed)))))

(deftest test-graph->json-empty-graph
  (let [parsed (json/read-str (graph->json (lg/digraph)))]
    (is (empty? (get parsed "nodes")))
    (is (empty? (get parsed "edges")))))

;;;

nil
