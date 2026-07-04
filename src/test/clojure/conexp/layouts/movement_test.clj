;; Copyright ⓒ the conexp-clj developers; all rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.layouts.movement-test
  (:require [clojure.test :refer [deftest is testing]]
            [conexp.layouts.movement :refer [reachable-nodes
                                             reachable-irreducible-nodes
                                             additively-influenced-nodes]]))

;;; A diamond lattice  bot < a,b < top  as up/down neighbour maps.

(def ^:private uppers {:bot [:a :b], :a [:top], :b [:top], :top []})
(def ^:private lowers {:top [:a :b], :a [:bot], :b [:bot], :bot []})
(defn- up [n] (uppers n))
(defn- lo [n] (lowers n))

(deftest test-reachable-nodes
  (is (= #{:a :b :top} (reachable-nodes up :bot)))
  (is (= #{:top}       (reachable-nodes up :a)))
  (is (= #{:a :b :bot} (reachable-nodes lo :top)))
  (is (= #{}           (reachable-nodes up :top))))

(deftest test-reachable-irreducible-nodes
  ;; bot has two upper neighbours (not irreducible); a and b each have one.
  (is (= #{:a :b} (reachable-irreducible-nodes up :bot)))
  ;; starting at a: a has one upper neighbour (kept), top has none (dropped).
  (is (= #{:a}    (reachable-irreducible-nodes up :a))))

(deftest test-additively-influenced-nodes
  (testing "infimum-additive influence of the bottom element"
    (is (= #{[:a 1/2] [:b 1/2]}
           (set (additively-influenced-nodes :bot up lo)))))
  (testing "roles of upper/lower may be swapped (supremum-additive)"
    (is (= #{[:a 1/2] [:b 1/2]}
           (set (additively-influenced-nodes :top lo up))))))

;;;

nil
