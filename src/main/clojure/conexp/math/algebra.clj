;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.math.algebra
  (:use conexp.base)
  (:require [conexp.fca.contexts :refer :all]))

;;; Datastructure

(defprotocol Order
  (base-set [order] "Returns the base set.")
  (order [order] "Returns a function of one or two arguments representing the 
                  order relation. If called with one argument it is assumed 
                  that this argument is a pair of elements."))

;;;

(defn closure-induced-preorder
  "Given a closure operation returns the preorder function for two elements a
   and b; 'a (<=_L) b'."
  [closure]
  (fn [a b] (some #{b} (closure a))))

(defn closure-equivalence
  "Given a set and closure operation computes the equivalence classes."
  [set closure]
  (group-by closure set))

;;; Linear Extensions
(defn all-linear-extensions 
  "Computes a list of all linear extensions of a Poset O. Also works
  with concept lattices of conexp.fca.lattices.  The algorithm
  generates a linear extensions by iteratively adding a minimal
  elements (according to ord) to the previously generated sequence of
  elements. All possible extensions by minimal elements are stored in
  a queue. The algorithm continues until all sequences of the queue
  are contain all elements of the ordered set."
  ([ord-set] (all-linear-extensions (base-set ord-set) (order ord-set)))
  ([elements ord]
   (loop [les-iter [[[] (set elements)]] les #{}] 
     (if (empty? les-iter) les
         (let [[lin non-lin] (first les-iter)]
           (if (empty? non-lin)
             (recur (rest les-iter) (conj les lin))
             (let [pareto-mins (filter (fn [ci] (not (some #(ord % ci) (disj non-lin ci)))) non-lin)
                   lin+pm (map (fn [ci] [(into lin [ci]) (disj non-lin ci)]) pareto-mins)]
               (recur (into (rest les-iter) lin+pm) les))))))))
;;;

nil
