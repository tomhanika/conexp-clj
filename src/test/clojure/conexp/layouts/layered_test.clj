;; Copyright ⓒ the conexp-clj developers; all rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.layouts.layered-test
  (:use conexp.base
        conexp.math.algebra
        conexp.fca.contexts
        conexp.fca.protoconcepts
        conexp.fca.lattices
        conexp.layouts.base
        conexp.layouts.util
        conexp.layouts.layered
        conexp.layouts.util-test)
  (:use clojure.test))

;;;

(def test-ctx-1
  (make-context-from-matrix [1 2 3]
                            ['a 'b 'c]
                            [1 0 0
                             0 1 0
                             0 1 1]))

(deftest test-simple-layered-layout
  (with-testing-data [poset (apply conj test-lattices test-posets)]
    (let [simply-layered (simple-layered-layout poset),
          layers         (layers poset),
          simple-layers  (sort-by first
                                  (reduce! (fn [map [a [x y]]]
                                             (assoc! map y (conj (get map y) x)))
                                           {}
                                           (positions simply-layered)))]
      (and (layout? simply-layered)
           (= (count layers)
              (count simple-layers))
           (= (map count layers)
              (map (comp count second) simple-layers))
           (forall [[_ coords] simple-layers]
                   (forall [x coords]
                           (exists [y coords]
                                   (= x (- y))))))))
  ;; for protoconcepts
  (let [test-protoconcepts (protoconcepts-order test-ctx-1)
        protoconcept-layout (simple-layered-layout test-protoconcepts)
        layers (layers test-protoconcepts)
        protoconcept-layers (sort
                             (group-by #(second (second %))
                                       (positions protoconcept-layout)))]
    (is (layout? protoconcept-layout))
    (is (= (count layers)
              (count protoconcept-layers)))
    (is (= (map count layers)
           (map (comp count second) protoconcept-layers)))))

(deftest test-as-chain
  (with-testing-data [poset (apply conj test-lattices test-posets)]
    (let [chain (as-chain poset)]
      (and (layout? chain)
           (>= 1 (count (set-of a | [a _] (vals (positions chain)))))
           (forall [[x [_ b]] (positions chain),
                    [y [_ d]] (positions chain)]
                   (=> (and ((order poset) x y)
                            (not= x y))
                       (< b d)))))))

;;;

nil
