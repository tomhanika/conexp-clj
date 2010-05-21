;; Copyright (c) Daniel Borchmann. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.contrib.dl.util.graphs
  (:use conexp.base)
  (:require [clojure.contrib.graph :as graph]))

(ns-doc
 "A custom implementation of graph algorithms on top of
 clojure.contrib.graph.")

;;;

(defn make-directed-graph
  "Constructs a directed graph."
  [nodes neighbour-fn]
  (struct graph/directed-graph
          nodes
          neighbour-fn))

(def self-recursive-sets graph/self-recursive-sets)
(def dependency-list graph/dependency-list)

;;;

(defn- post-ordered-visit               ;from clojure.contrib.graph
  "Starting at node n, perform a post-ordered walk."
  [g n [visited acc :as state]]
  (if (visited n)
    state
    (let [[v2 acc2] (reduce (fn [st nd] (post-ordered-visit g nd st))
                            [(conj visited n) acc]
                            (graph/get-neighbors g n))]
      [v2 (conj acc2 n)])))

(defn scc                               ;from clojure.contrib.graph
  "Returns, as a sequence of sets, the strongly connected components of g."
  [g]
  (let [po (reverse (graph/post-ordered-nodes g))
        rev (graph/reverse-graph g)
        step (fn [stack visited acc]
               (if (empty? stack)
                 acc
                 (let [[nv comp] (post-ordered-visit rev
                                                     (first stack)
                                                     [visited #{}])
                       ns (doall (remove nv stack))] ;doall prevents StackOverflow
                   (recur ns nv (conj acc comp)))))]
    (step po #{} [])))

;;;

nil
