;; Copyright ⓒ the conexp-clj developers; all rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.layouts.base-test
  (:use conexp.base
        conexp.fca.contexts
        conexp.fca.lattices
        conexp.layouts.base
        conexp.layouts.layered)
  (:use clojure.test))

;;;

(deftest test-make-layout
  (is (layout? (make-layout {} [])))
  (is (layout? (make-layout {1 [0,0], 2 [1,1], 3 [2,2]}
                            #{[1 2] [2 3]})))
  (is (layout? (make-layout {1 [0 0], 2 [1 1], 3 [2 2]}
                            [[1 2] [2 3]])))
  (is (thrown-with-msg? IllegalArgumentException
        #"Positions must be a map."
        (make-layout 1 2)))
  (is (thrown-with-msg? IllegalArgumentException
        #"Connections must be given as a collection of pairs."
        (make-layout {} 1)))
  (is (thrown-with-msg? IllegalArgumentException
        #"Points must be positioned with pairs."
        (make-layout {1 2 3 4}
                     [])))
  (is (thrown-with-msg? IllegalArgumentException
        #"Connections must be given between positioned points."
        (make-layout {1 [0,0]} [[1 2]])))
  (is (thrown-with-msg? IllegalArgumentException
        #"Positioned points must be the elements of the given lattice."
        (make-layout (make-lattice-nc [1 2 3] <)
                     {}
                     [])))
  (is (thrown-with-msg? IllegalArgumentException
        #"The given connections must represent the edges of the given lattice."
        (make-layout (make-lattice-nc [1 2 3] <)
                     {1 [0 0] 2 [1 1] 3 [2 2]}
                     [])))
  (is (thrown-with-msg? IllegalArgumentException
        #"Given set of edges is cyclic."
        (make-layout {1 [0 0] 2 [1 1] 3 [2 2]}
                     [[1 2] [2 3] [3 1]])))
  (is (thrown-with-msg? IllegalArgumentException
        #"Given set of edges is cyclic."
        (make-layout {1 [0 0] 2 [0 0] 3 [0 0] 4 [0 0] 5 [0 0]}
                     [[1 2] [2 3] [2 4] [4 5] [3 2]])))
  (is (layout? (make-layout {1 [0 0], 2 [0 1]}
                            #{[1 2]}
                            {1 ["1u" nil], 2 ["2u" [0 2]]}
                            {1 ["1l" [0 -1]], 2 ["2l" nil]})))
  (is (thrown-with-msg? IllegalArgumentException
        #"Labels must be given as map."
        (make-layout {1 [0 0], 2 [0 1]}
                            #{[1 2]}
                            [1 ["1u" nil], 2 ["2u" [0 2]]]
                            {1 ["1l" [0 -1]], 2 ["2l" nil]})))
  (is (thrown-with-msg? IllegalArgumentException
        #"Nodes in layout and given labeled nodes are different."
        (make-layout {1 [0 0], 2 [0 1]}
                            #{[1 2]}
                            {1 ["1u" nil], 2 ["2u" [0 2]], 3 ["3u" [0 3]]}
                            {1 ["1l" [0 -1]]})))
  (is (thrown-with-msg? IllegalArgumentException
        #"Nodes must be labeled with pairs"
        (make-layout {1 [0 0], 2 [0 1]}
                            #{[1 2]}
                            {1 ["1u" nil], 2 ["2u" 0 2]}
                            {1 ["1l" [0 -1]], 2 ["2l" nil]})))
  (is (thrown-with-msg? IllegalArgumentException
        #"Labels must be above the labeled node"
        (make-layout {1 [0 0], 2 [0 1]}
                            #{[1 2]}
                            {1 ["1u" nil], 2 ["2u" [0 1]]}
                            {1 ["1l" [0 1]], 2 ["2l" nil]}))))

(deftest test-positions-and-connections
  (let [pos {1 [0,0], 2 [1,1], 3 [2,2]},
        con #{[1 2] [2 3]},
        lay (make-layout pos con)]
    (is (= pos (positions lay)))
    (is (= con (connections lay)))))

(deftest test-labels
  (let [layout (make-layout {1 [0 0], 2 [0 1]}
                            #{[1 2]}
                            {1 ["1u" nil], 2 ["2u" [0 2]]}
                            {1 ["1l" [0 -1]], 2 ["2l" nil]})]
    (is (= (upper-label layout 1) "1u"))
    (is (= (lower-label layout 2) "2l"))
    (is (= (upper-labels layout) {1 ["1u" nil], 2 ["2u" [0 2]]}))
    (is (= (lower-labels layout) {1 ["1l" [0 -1]], 2 ["2l" nil]}))
    (is (= (upper-label-position layout 1) nil))
    (is (= (lower-label-position layout 1) [0 -1]))))

;;;

(defn- rand-layout []
  (let [lattice (concept-lattice (rand-context 7 7 0.7)),
        indexes (vec (base-set lattice)),
        lattice (make-lattice-nc (range (count indexes))
                                 (fn [x y]
                                   (subset? (first (indexes x))
                                            (first (indexes y)))))]
    (simple-layered-layout lattice)))

(def- testing-layouts
  (concat
   [(make-layout {1 [0,0], 2 [1,1], 3 [2,2]}
                 #{[1 2] [2 3]})]
   (repeatedly 10 rand-layout)))

;;;

(deftest test-lattice
  (with-testing-data [lay testing-layouts]
    (let [lattice (lattice lay)]
      (and (= (nodes lay) (base-set lattice))
           (forall [x (nodes lay),
                    y (nodes lay)]
             (<=> (directly-neighboured? lattice x y)
                  (contains? (connections lay) [x y])))))))

(deftest test-update-positions
  (with-testing-data [layout testing-layouts]
    (let [updated (update-positions layout
                                    (map-by-fn (constantly [0 0])
                                               (keys (positions layout))))]
      (and (= (connections layout)
              (connections updated))
           (= (positions updated)
              (map-by-fn (constantly [0 0])
                         (keys (positions layout))))))))

(deftest test-nodes
  (with-testing-data [layout testing-layouts]
    (= (nodes layout)
       (set (keys (positions layout))))))

;;;

(deftest test-layout-memoization
  (with-testing-data [lay testing-layouts]
    (let [layout  (make-layout-nc (positions lay) ;get fresh layout
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

(deftest test-concept-lattice-annotation
  (let [ctx (make-context (range 7)
                          (range 7)
                          #{[2 1] [4 3] [5 4] [1 0] [2 2]
                            [6 6] [1 1] [5 6] [1 3] [3 6]
                            [0 3] [1 4] [2 6] [0 4] [1 5]
                            [0 5] [5 1] [6 2] [3 0] [4 1]
                            [2 0]})]
    (is (= (concept-lattice-annotation (simple-layered-layout (concept-lattice ctx)))
           {[#{1 4} #{1 3}] [#{} #{4}],
            [#{1 5} #{1 4}] [#{} #{}],
            [#{2 5} #{1 6}] [#{} #{}],
            [#{2 6} #{2 6}] [#{2} #{6}],
            [#{2 3} #{0 6}] [#{} #{3}],
            [#{5} #{1 4 6}] [#{} #{5}],
            [#{2} #{0 1 2 6}] [#{} #{2}],
            [#{0 1 2 3 4 5 6} #{}] [#{} #{}],
            [#{0 1} #{3 4 5}] [#{5} #{0}],
            [#{1} #{0 1 3 4 5}] [#{} #{1}],
            [#{1 2 4 5} #{1}] [#{1} #{}],
            [#{} #{0 1 2 3 4 5 6}] [#{} #{}],
            [#{2 3 5 6} #{6}] [#{6} #{}],
            [#{1 2 3} #{0}] [#{0} #{}],
            [#{1 2} #{0 1}] [#{} #{}],
            [#{0 1 4} #{3}] [#{3} #{}],
            [#{0 1 5} #{4}] [#{4} #{}]}))))

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
          3 [3, ""]}))
  (is (= (annotation (make-layout {1 [0,0], 2 [1,1], 3 [2,2]}
                                  #{[1 2] [2 3]}
                                  (constantly ['x nil])
                                  (constantly ['y nil])))
         {1 ['x 'y],
          2 ['x 'y],
          3 ['x 'y]})))

;;;

nil
