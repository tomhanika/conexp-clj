;; Copyright (c) Daniel Borchmann. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.tests.layouts.base
  (:use conexp.base
        conexp.layouts.base)
  (:use clojure.test))

;;;

(deftest test-make-layout
  (is (layout? (make-layout [] [])))
  (is (layout? (make-layout {1 [0,0], 2 [1,1], 3 [2,2]}
                            #{[1 2] [2 3]}))))

(deftest test-positions-and-connections
  (let [pos {1 [0,0], 2 [1,1], 3 [2,2]},
        con #{[1 2] [2 3]},
        lay (make-layout pos con)]
    (is (= pos (positions lay)))
    (is (= con (connections lay)))))

;;;

(defn- rand-layout []
  (let [nodes           (set-of-range (rand 50)),
        number-of-edges (rand 150)]
    (make-layout (map-by-fn (fn [_] [(rand 40) (rand 40)])
                            nodes)
                 (take number-of-edges
                       (shuffle (cross-product nodes nodes))))))

(defvar- testing-layouts
  (concat
   [(make-layout {1 [0,0], 2 [1,1], 3 [2,2]}
                 #{[1 2] [2 3]})]
   (repeatedly 10 rand-layout)))

;;;

(deftest test-update-positions
  (with-testing-data [layout testing-layouts]
    (let [updated (update-positions layout
                                    (map-by-fn (constantly [0 0])
                                               (vals (positions layout))))]
      (and (= (connections layout)
              (connections updated))
           (= (positions updated)
              (map-by-fn (constantly [0 0])
                         (vals (positions layout))))))))

(deftest test-nodes
  (with-testing-data [layout testing-layouts]
    (= (nodes layout)
       (set (keys (positions layout))))))

;;;

;; test memoization of layout functions

;; upper-neighbours
;; lower-neighbours
;; upper-neighbours-of-inf-irreducibles
;; inf-irreducibles
;; sup-irreducibles
;; full-order-relation
;; lattice
;; context
;; concept-lattice-layout?
;; annotation

;;;

nil
