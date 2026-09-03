;; Copyright ⓒ the conexp-clj developers; all rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.gui-shared.layout-json-test
  "Pins the layout->json -> render-model contract on the JVM (the browser relies
  on the same .cljc), and checks that movement.cljc drives reachability over the
  derived neighbour functions."
  (:require [clojure.test :refer [deftest is testing]]
            [conexp.fca.contexts :refer [make-context-from-matrix]]
            [conexp.fca.lattices :refer [concept-lattice]]
            [conexp.layouts :refer [standard-layout]]
            [conexp.layouts.base :refer [positions connections]]
            [conexp.io.layouts :refer [layout->json]]
            [conexp.gui-shared.layout-json :refer [render-model neighbour-fns]]
            [conexp.layouts.movement :refer [reachable-nodes]]))

(deftest test-render-model
  (let [ctx   (make-context-from-matrix [0 1 2] [0 1 2] [1 0 0, 1 1 0, 1 1 1])
        lat   (concept-lattice ctx)
        lay   (standard-layout lat)
        model (render-model (layout->json lay))]
    (testing "node and edge counts match the layout"
      (is (= (count (positions lay))   (count (:nodes model))))
      (is (= (count (connections lay)) (count (:edges model)))))
    (testing "each node carries coordinates and concept flags"
      (is (every? (fn [n] (and (number? (:x n)) (number? (:y n))
                               (contains? n :attribute-concept?)
                               (contains? n :object-concept?)))
                  (:nodes model))))
    (testing "bounds cover all node positions"
      (is (some? (:bounds model))))
    (testing "movement.cljc reachability over the derived neighbours: the bottom
              element reaches every other node upward"
      (let [{:keys [uppers lowers ids]} (neighbour-fns model)
            bottom (first (filter #(empty? (lowers %)) ids))]
        (is (some? bottom))
        (is (= (dec (count ids)) (count (reachable-nodes uppers bottom))))))))

;;;

nil
