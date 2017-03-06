;; Copyright ⓒ the conexp-clj developers; all rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.contrib.util.general-sorted-sets-test
  (:use conexp.base
        conexp.contrib.util.general-sorted-sets)
  (:use clojure.test))

;;;

(deftest- adds-elements-in-correct-order
  (are [order elts] (let [gss-seq (make-general-sorted-set order elts)]
                      (forall [x elts, y elts]
                        (=> (order x y)
                            (some #(= % y) (drop-while #(not= % x) gss-seq)))))
       <= [1 2 3 4 5 6 7 8 9],
       subset? [#{} #{1} #{2} #{3} #{1 2 3} #{1 2} #{1 2 3 4}]))

(deftest- no-equivalent-elements
  (are [num order elts] (= num (count (seq (make-general-sorted-set order elts))))
       4 <= [1 2 3 4 4 3 2 1],
       4 subset? [#{} #{1} #{2} #{1 2} #{2} #{1 2} #{}],
       6 subset? [#{1} #{2} #{3} #{4} #{1 3} #{3 4} #{1} #{2}]))

(deftest- finds-correct-neighbours
  (are [order elts new-elt lowers uppers] (let [gss (make-general-sorted-set order elts)]
                                            (and (= (set lowers)
                                                    (set (map :node
                                                              (@#'conexp.contrib.util.general-sorted-sets/find-lower-neighbours gss new-elt))))
                                                 (= (set uppers)
                                                    (set (map :node
                                                              (@#'conexp.contrib.util.general-sorted-sets/find-upper-neighbours gss new-elt))))))
       <= [1 2 4 5] 3 [2] [4],
       subset? [#{} #{1} #{2} #{3} #{1 2 3} #{1 2}] #{1 2 3 4} [#{1 2 3}] []))

(deftest- correct-hasse-graph
  (are [order elts edges] (let [gss (make-general-sorted-set order elts)]
                            (= (set edges) (set (hasse-graph gss))))
       <= [1 2 3 4 5] [[1 2] [2 3] [3 4] [4 5]],
       subset? [#{} #{1} #{2} #{3} #{1 2 3} #{1 2} #{1 2 3 4}]
               [[#{} #{1}] [#{} #{2}] [#{} #{3}]
                [#{1} #{1 2}] [#{2} #{1 2}] [#{1 2} #{1 2 3}]
                [#{3} #{1 2 3}] [#{1 2 3} #{1 2 3 4}]]))

(deftest- containment
  (are [order elts no-elts] (let [gss (make-general-sorted-set order elts)]
                              (and (forall [elt elts]
                                           (contained-in-gss? gss elt))
                                   (forall [elt no-elts]
                                           (not (contained-in-gss? gss elt)))))
       <= [1 2 3 4] [5 6 7 8],
       subset? [#{} #{1} #{2} #{3} #{1 2 3} #{1 2} #{1 2 3 4}]
               [#{4} #{1 3 4} #{5} #{2 3 4}]))

(deftest- correct-gss-elements
  (are [order elements] (= (set elements)
                           (set (gss-elements (make-general-sorted-set order elements))))
       <= [1 2 3 4 5 6]
       subset? [#{1} #{2}]))

(defn test-ns-hook []
  (adds-elements-in-correct-order)
  (no-equivalent-elements)
  (finds-correct-neighbours)
  (correct-hasse-graph)
  (containment)
  (correct-gss-elements))

;;;

nil
