;; Copyright ⓒ the conexp-clj developers; all rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.layouts.util-test
  (:use conexp.base
        conexp.fca.contexts
        conexp.math.algebra
        conexp.fca.lattices
        conexp.fca.posets
        conexp.layouts.common
        conexp.layouts.force
        conexp.layouts.util
        conexp.layouts.base)
  (:use clojure.test)
  (:import [conexp.fca.posets Poset]))

(def test-lattices
  [(make-lattice [1 2 3 4 5] <=),
   (concept-lattice (rand-context 10 10 0.7))])

(def test-posets
  [(make-poset [1 2 3 4] (fn [A B] (contains? #{[1 1] [2 2] [3 3] [4 4]
                                                [1 3] [2 3] [1 4] [2 4]} [A B])))])

(def test-layouts
  [(make-layout {1 [0 0],
                 2 [1 2],
                 3 [10 20]}
                #{[1 2] [2 3]}),
   (make-layout {1 [0 0],
                 2 [-1 1],
                 3 [1 1],
                 4 [0 2]}
                #{[1 2] [1 3] [2 4] [3 4]})
   (make-layout {1 [0 0],
                 2 [-1 1],
                 3 [1 1]}
                #{[1 2] [1 3]})
   (make-layout {2 [-1 1],
                 3 [1 1],
                 4 [0 2]}
                #{[2 4] [3 4]})])

;;;

(deftest test-enclosing-rectangle
  (is (thrown? IllegalArgumentException
               (enclosing-rectangle [])))
  (is (= (enclosing-rectangle [[1 2] [2 3] [3 4]])
         [1 2 3 4])))

(deftest test-scale-layout
  (with-testing-data [layout test-layouts]
    (let [scaled-layout (scale-layout [0 0] [100 100] layout)]
      (is (= (enclosing-rectangle (vals (positions scaled-layout)))
             [0 0 100 100]))
      (is (= (connections layout)
             (connections scaled-layout)))
      (is (= (poset layout)
             (poset scaled-layout)))
      (is (= (upper-labels layout)
             (upper-labels scaled-layout)))
      (is (= (lower-labels layout)
             (lower-labels scaled-layout))))))

;;;

(deftest test-layers
  (with-testing-data [poset (apply conj test-lattices test-posets)]
    (let [<=     (order poset),
          layers (layers poset)]
      (every? (fn [[lower upper]]
                (forall [x lower]
                  (exists [y upper]
                    (<= x y))))
              (partition 2 1 layers)))))

(deftest test-edges
  (with-testing-data [poset (apply conj test-lattices test-posets)]
    (let [edges (edges poset)]
      (forall [a (base-set poset),
               b (base-set poset)]
        (<=> (contains? edges [a b])
             (directly-neighboured? poset a b))))))

(deftest test-top-down-elements-in-layout
  (with-testing-data [layout test-layouts]
    (let [index (zipmap (top-down-elements-in-layout layout)
                        (range))]
      (forall [[a b] (connections layout)]
        (< (index b) (index a))))))

;;;

(deftest test-fit-layout-to-grid
  (with-testing-data [layout test-layouts]
    (let [positions (vals (positions (fit-layout-to-grid layout
                                                         [1/2 -13/10]
                                                         2/5
                                                         7/10)))]
      (forall [[a b] positions,
               [c d] positions]
       (and (integer? (/ (- a c) 2/5))
            (integer? (/ (- b d) 7/10)))))))

(deftest test-discretize-layout
  (with-testing-data [layout test-layouts]
    (let [[xmin ymin xmax ymax] (enclosing-rectangle (vals (positions layout))),
          origin [xmin ymin],
          layout (discretize-layout layout 3 7),
          x_pad  (/ (- xmax xmin) 3),
          y_pad  (/ (- ymax ymin) 7)]
      (forall [[a b] (vals (positions layout))]
        (and (integer? (/ (- a xmin) x_pad))
             (integer? (/ (- b ymin) y_pad)))))))

;;;

(deftest test-compute-below-above
  (is (= (compute-below-above [[4 5] [1 2] [3 4] [2 3]])
         [{5 #{1 2 3 4 5},
           4 #{1 2 3 4},
           3 #{1 2 3},
           2 #{1 2},
           1 #{1}}
          {5 #{5},
           4 #{4 5},
           3 #{3 4 5},
           2 #{2 3 4 5},
           1 #{1 2 3 4 5}}])))

;;;

(deftest test-layout-fn-on-poset
  (let [poset-layouts (filter #(= Poset (type (poset %))) test-layouts)]
    (with-testing-data [layout poset-layouts]
      (let [new-layout (layout-fn-on-poset force-layout layout)]
        (= (nodes layout)
           (nodes new-layout))
        (= (connections layout)
           (connections new-layout))))
    (with-testing-data [layout poset-layouts]
      (let [new-layout (layout-fn-on-poset force-layout layout 10)]
        (= (nodes layout)
           (nodes new-layout))
        (= (connections layout)
           (connections new-layout))))
    (with-testing-data [layout poset-layouts]
      (let [new-layout (layout-fn-on-poset to-inf-additive-layout layout)]
        (= (nodes layout)
           (nodes new-layout))
        (= (connections layout)
           (connections new-layout))))))

;;;

nil
