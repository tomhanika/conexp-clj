;; Copyright ⓒ the conexp-clj developers; all rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.layouts.movement
  "Pure order-theoretic traversals over a neighbourhood relation, used to decide
  which diagram nodes move together during interactive layout editing.

  Every function is parameterised by a `neighbours` function (node -> seq of
  neighbour nodes), so the algorithms are independent of any particular node
  representation.  In the GUI the neighbours function reads the drawn diagram's
  nodes; the same functions work on any abstract node model.")

;;;

(defn reachable-nodes
  "Returns the set of all nodes reachable from `node` (exclusive, unless a cycle
  leads back to it) by repeatedly following `neighbours`."
  [neighbours node]
  (loop [to-process (seq (set (neighbours node)))
         visited    #{}]
    (if (empty? to-process)
      visited
      (let [nxt (first to-process)]
        (if (contains? visited nxt)
          (recur (rest to-process) visited)
          (recur (into (rest to-process) (neighbours nxt))
                 (conj visited nxt)))))))

(defn reachable-irreducible-nodes
  "Returns `node` together with all nodes reachable from it via `neighbours`,
  keeping only those that have exactly one neighbour (the irreducible ones)."
  [neighbours node]
  (loop [to-process (seq #{node})
         visited    #{}]
    (if (empty? to-process)
      visited
      (let [nxt (first to-process)]
        (if (contains? visited nxt)
          (recur (rest to-process) visited)
          (let [neighs (neighbours nxt)]
            (if (= 1 (count neighs))
              (recur (into (rest to-process) neighs) (conj visited nxt))
              (recur (into (rest to-process) neighs) visited))))))))

(defn additively-influenced-nodes
  "Returns all nodes which are additively influenced by `node`, each paired with
  a weight representing the strength of the influence.  `uppers` and `lowers`
  are neighbour functions (their roles may be interchanged without harm)."
  [node uppers lowers]
  (let [irrs      (reachable-irreducible-nodes uppers node),
        others    (vals (group-by identity
                                  (mapcat #(reachable-nodes lowers %) irrs))),
        irr-count (count irrs)]
    (concat (for [n irrs
                  :when (not= n node)]
              [n (/ irr-count)])
            (for [nodes others
                  :when (not= (first nodes) node)]
              [(first nodes) (/ (count nodes) irr-count)]))))

;;;

nil
