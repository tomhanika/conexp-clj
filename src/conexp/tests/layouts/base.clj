;; Copyright (c) Daniel Borchmann. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.tests.layouts.base
  (:use conexp.base
        conexp.fca.contexts
        conexp.fca.lattices
        conexp.layouts.base
        conexp.layouts.layered)
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
  (let [lattice (concept-lattice (rand-context 7 7 0.7)),
        indexes (vec (base-set lattice)),
        lattice (make-lattice-nc (range (count indexes))
                                 (fn [x y]
                                   (subset? (first (indexes x))
                                            (first (indexes y)))))]
    (simple-layered-layout lattice)))

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

(deftest test-layout-memoization
  (with-testing-data [lay testing-layouts]
    (let [layout  (make-layout (positions lay) ;get fresh layout
                               (connections lay)),
          counter (atom 0),
          old-con connections]
      ;;we hook up «connections» to see how often it is called
      (with-var-bindings [connections (fn [lay]
                                        (swap! counter inc)
                                        (old-con lay))]
        (let [count-1 (do (upper-neighbours layout) @counter),
              count-2 (do (upper-neighbours layout) @counter)]
          (and (< 0 count-1)             ;is called in the first place
               (= count-1 count-2))))))) ;but not in the second

;;;

(deftest test-upper-neighbours
  (with-testing-data [lay testing-layouts]
    (let [uppers (upper-neighbours lay)]
      (forall [x (nodes lay)]
        (= (set (uppers x))
           (lattice-upper-neighbours (lattice lay) x))))))

(deftest test-lower-neighbours
  (with-testing-data [lay testing-layouts]
    (let [lowers (lower-neighbours lay)]
      (forall [x (nodes lay)]
        (= (set (lowers x))
           (lattice-lower-neighbours (lattice lay) x))))))

(deftest test-upper-neighbours-of-inf-irreducibles
  (with-testing-data [lay testing-layouts]
    (let [uppers (upper-neighbours-of-inf-irreducibles lay),
          infs   (lattice-inf-irreducibles (lattice lay))]
      (forall [x infs]
        (= (set (list (uppers x)))
           (lattice-upper-neighbours (lattice lay) x))))))

(deftest test-inf-irreducibles
  (with-testing-data [lay testing-layouts]
    (= (set (inf-irreducibles lay))
       (set (lattice-inf-irreducibles (lattice lay))))))

(deftest test-sup-irreducibles
  (with-testing-data [lay testing-layouts]
    (= (set (sup-irreducibles lay))
       (set (lattice-sup-irreducibles (lattice lay))))))

(deftest test-full-order-relation
  (let [lay (make-layout {1 [0,0], 2 [1,1], 3 [2,2]}
                         #{[1 2] [2 3]})]
    (is (= (full-order-relation lay)
           #{[1 1] [2 2] [3 3]
             [1 2] [2 3] [1 3]}))))

(deftest test-lattice
  (with-testing-data [lay testing-layouts]
    (let [lattice (lattice lay)]
      (and (= (nodes lay) (base-set lattice))
           (forall [x (nodes lay),
                    y (nodes lay)]
             (<=> (directly-neighboured? lattice x y)
                  (contains? (connections lay) [x y])))))))

(deftest test-context
  (with-testing-data [lay testing-layouts]
    (= (context lay)
       (standard-context (lattice lay)))))

(deftest test-concept-lattice-layout?
  (is (concept-lattice-layout?
       (simple-layered-layout
        (concept-lattice
         (rand-context 10 10 0.3)))))
  (is (not (concept-lattice-layout?
            (make-layout {1 [0,0], 2 [1,1], 3 [2,2]}
                         #{[1 2] [2 3]})))))

(deftest test-annotation
  (let [ctx (make-context (range 7)
                          (range 7)
                          #{[2 1] [4 3] [5 4] [1 0] [2 2]
                            [6 6] [1 1] [5 6] [1 3] [3 6]
                            [0 3] [1 4] [2 6] [0 4] [1 5]
                            [0 5] [5 1] [6 2] [3 0] [4 1]
                            [2 0]})]
    (is (= (annotation (simple-layered-layout (concept-lattice ctx)))
           {[#{1 4} #{1 3}] ["" "4"],
            [#{1 5} #{1 4}] ["" ""],
            [#{2 5} #{1 6}] ["" ""],
            [#{2 6} #{2 6}] ["2" "6"],
            [#{2 3} #{0 6}] ["" "3"],
            [#{5} #{1 4 6}] ["" "5"],
            [#{2} #{0 1 2 6}] ["" "2"],
            [#{0 1 2 3 4 5 6} #{}] ["" ""],
            [#{0 1} #{3 4 5}] ["5" "0"],
            [#{1} #{0 1 3 4 5}] ["" "1"],
            [#{1 2 4 5} #{1}] ["1" ""],
            [#{} #{0 1 2 3 4 5 6}] ["" ""],
            [#{2 3 5 6} #{6}] ["6" ""],
            [#{1 2 3} #{0}] ["0" ""],
            [#{1 2} #{0 1}] ["" ""],
            [#{0 1 4} #{3}] ["3" ""],
            [#{0 1 5} #{4}] ["4" ""]})))
  (is (= (annotation (make-layout {1 [0,0], 2 [1,1], 3 [2,2]}
                                  #{[1 2] [2 3]}))
         {1 [1, ""],
          2 [2, ""],
          3 [3, ""]})))

;;;

nil
