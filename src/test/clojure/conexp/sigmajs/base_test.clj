(ns conexp.sigmajs.base-test
  (:require [ubergraph.core :as uber])
  (:use clojure.test
        conexp.sigmajs.base))

(deftest test-graph->json
  (is
    (let [g (uber/digraph [:a :b] [:b :c])]
      (.matches (graph->json g) " *\\{
? *\"nodes\": *\\[
? *\\{
? *\"id\": *\":a\",(
? *\"label\": *\":a\",)?
? *\"x\": *\"?[0-9.]+\"?,
? *\"y\": *\"?[0-9.]+\"?(,
? *\"size: *\"?[0-9.]+\"?)?
? *\\},
? *\\{
? *\"id\": *\":b\",
? *\"label\": *\":b\",
? *\"x\": *\"?[0-9.]+\"?,
? *\"y\": *\"?[0-9.]+\"?(,
? *\"size: *\"?[0-9.]+\"?)?
? *\\},
? *\\{
? *\"id\": *\":c\",
? *\"label\": *\":c\",
? *\"x\": *\"?[0-9.]+\"?,
? *\"y\": *\"?[0-9.]+\"?(,
? *\"size: *\"?[0-9.]+\"?)?
? *\\}
? *\\],
? *\"edges\": *\\[
? *\\{
? *\"id\": *\".*\",
? *\"source\": *\":a\",
? *\"target\": *\":b\"
? *\\},
? *\\{
? *\"id\": *\".*\",
? *\"source\": *\":b\",
? *\"target\": *\":c\"
? *\\}
? *\\]
? *\\}"))))

;;;

nil
