;; Copyright ⓒ the conexp-clj developers; all rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.layouts.dim-flux-test
  (:require [clojure.test :refer :all]
            [conexp.fca.contexts :refer [make-context make-context-from-matrix
                                         objects attributes]]
            [conexp.fca.lattices :refer [concept-lattice lattice-inf-irreducibles
                                         lattice-sup-irreducibles
                                         lattice-upper-neighbours]]
            [conexp.layouts.base :as lay]
            [conexp.layouts.dim-draw :refer [dim-draw-layout]]
            [conexp.layouts.dim-flux :refer :all]
            [conexp.layouts.layered :refer [simple-layered-layout]]
            [conexp.math.algebra :refer [base-set]]))

;;; Test data

(def ^:private dwarf-planets
  "The five dwarf planets and where they are found, the running example of the
  DimFlux paper."
  (make-context-from-matrix ["Ceres" "Pluto" "Eris" "Haumea" "Makemake"]
                            ["asteroid-belt" "kuiper-belt" "scattered-disc" "moons"]
                            [1 0 0 0
                             0 1 0 1
                             0 0 1 1
                             0 1 0 1
                             0 1 0 0]))

(def ^:private boolean-3
  "The contranominal scale on three elements, whose concept lattice is the
  Boolean lattice on three atoms."
  (make-context #{"1" "2" "3"} #{"a" "b" "c"}
                #{["1" "b"] ["1" "c"] ["2" "a"] ["2" "c"] ["3" "a"] ["3" "b"]}))

(def ^:private chain-3
  "The ordinal scale on three elements, whose concept lattice is a chain."
  (make-context-from-matrix ["1" "2" "3"] ["a" "b" "c"]
                            [1 0 0
                             1 1 0
                             1 1 1]))

(def ^:private crown
  (make-context-from-matrix (vec (map str (range 6))) (vec (map str "abcde"))
                            [1 0 1 0 1
                             0 1 1 0 0
                             1 1 0 1 0
                             0 0 1 1 1
                             1 0 0 1 0
                             0 1 0 0 1]))

(def ^:private all-contexts [dwarf-planets boolean-3 chain-3 crown])

;;; Helpers

(defn- line-diagram?
  "Every node is placed strictly below all of its upper neighbours."
  [layout]
  (let [pos (lay/positions layout)]
    (every? (fn [[a b]] (< (second (pos a)) (second (pos b))))
            (lay/connections layout))))

(defn- edges-of
  [lattice]
  (set (for [n (base-set lattice), u (lattice-upper-neighbours lattice n)]
         [n u])))

;;; The set representation matrix

(deftest test-set-representation-matrix
  (let [lattice    (concept-lattice dwarf-planets)
        nodes      (vec (base-set lattice))
        objs       (vec (lattice-sup-irreducibles lattice))
        attrs      (vec (lattice-inf-irreducibles lattice))
        srm        (set-representation-matrix lattice nodes objs attrs)
        leq        (conexp.math.algebra/order lattice)]
    (testing "one row per node and one column per irreducible element"
      (is (= (count nodes) (alength srm)))
      (is (every? #(= (+ (count objs) (count attrs)) (alength ^doubles %)) srm)))
    (testing "a column of an object marks the nodes above it, a column of an
              attribute the nodes not below it"
      (doseq [[i node] (map-indexed vector nodes)]
        (let [^doubles row (aget srm i)]
          (doseq [[j g] (map-indexed vector objs)]
            (is (= (if (leq g node) 1.0 0.0) (aget row j))))
          (doseq [[j m] (map-indexed vector attrs)]
            (is (= (if (leq node m) 0.0 1.0) (aget row (+ (count objs) j))))))))
    (testing "entries are zero or one"
      (is (every? (fn [^doubles row]
                    (every? #(or (= 0.0 %) (= 1.0 %)) row))
                  srm)))))

;;; Projection into the space of additive diagrams

(deftest test-additive-layout
  (doseq [ctx all-contexts]
    (let [lattice (concept-lattice ctx)
          layered (simple-layered-layout lattice)
          projected (additive-layout layered)]
      (testing "the projection keeps the lattice and its edges"
        (is (= (lay/poset layered) (lay/poset projected)))
        (is (= (set (lay/connections layered)) (set (lay/connections projected)))))
      (testing "the projection is doubly-additive"
        (is (doubly-additive? projected)))
      (testing "the projection is idempotent"
        (let [twice (additive-layout projected)]
          (is (every? (fn [[node [x y]]]
                        (let [[u v] ((lay/positions twice) node)]
                          (and (< (Math/abs (- (double x) (double u))) 1e-9)
                               (< (Math/abs (- (double y) (double v))) 1e-9))))
                      (lay/positions projected))))))))

(deftest test-doubly-additive?
  (testing "a diagram moved off the additive space is recognized as such"
    (let [lattice   (concept-lattice dwarf-planets)
          projected (additive-layout (simple-layered-layout lattice))
          node      (first (filter #(seq (lattice-upper-neighbours lattice %))
                                   (base-set lattice)))
          moved     (lay/update-positions
                     projected
                     (update (lay/positions projected) node
                             (fn [[x y]] [(+ 5 x) y])))]
      (is (doubly-additive? projected))
      (is (not (doubly-additive? moved))))))

(deftest test-additive-layout-needs-a-lattice
  (is (thrown? IllegalArgumentException
               (additive-layout
                (lay/make-layout-nc {1 [0 0], 2 [1 1], 3 [2 0]}
                                    #{[1 2] [3 2]})))))

;;; DimFlux

(deftest test-dim-flux-layout
  (doseq [ctx all-contexts]
    (let [lattice (concept-lattice ctx)
          layout  (dim-flux-layout ctx)]
      (testing "the layout draws exactly the concept lattice"
        (is (= (base-set lattice) (set (keys (lay/positions layout)))))
        (is (= (edges-of lattice) (set (lay/connections layout)))))
      (testing "the layout is a line diagram"
        (is (line-diagram? layout)))
      (testing "the layout is doubly-additive"
        (is (doubly-additive? layout)))
      (testing "all positions are finite"
        (is (every? (fn [[x y]] (and (Double/isFinite (double x))
                                     (Double/isFinite (double y))))
                    (vals (lay/positions layout))))))))

(def ^:private golden-positions
  "Positions the default pipeline produced before the layout was made faster,
  sorted by height and then by width.  They pin the algorithm as published: the
  bit set forces and the choice of starting diagram are meant to leave this
  untouched, and any change to it has to be a deliberate one."
  {:dwarf-planets [[0.0                 0.0]
                   [8.19935225251826    2.346964615822346]
                   [1.5080519851248935  2.7503682438937167]
                   [5.2663583159666185  3.63685157189151]
                   [-2.5915881534869167 4.251313112569929]
                   [9.187239538652053   7.566931803569037]
                   [4.876583774118542   9.170601222949625]]
   :crown         [[0.0                 0.0]
                   [0.8466887913409333  4.330029082813631]
                   [11.42214199577128   5.371920889808214]
                   [-2.6619399196473585 5.781949703748222]
                   [5.006672072366866   5.997766182579722]
                   [-5.7242857395793    6.631734031383484]
                   [1.9566450274783709  7.555458515492352]
                   [-8.349016755546916  9.745757438424809]
                   [8.092072349194877   10.722769863709924]
                   [4.587945288778855   10.734761834827436]
                   [1.6766024257904606  11.348615156481433]
                   [-3.730431808421188  11.519266250168938]
                   [-7.23906051940948   12.971186871103528]
                   [2.4050407820196287  16.64311394470097]]})

(deftest test-dim-flux-layout-is-unchanged
  (doseq [[key ctx] {:dwarf-planets dwarf-planets, :crown crown}]
    (testing (str "the default pipeline still draws " (name key) " as it did")
      (let [drawn (sort-by (juxt second first) (vals (lay/positions (dim-flux-layout ctx))))]
        (is (= (count (golden-positions key)) (count drawn)))
        (doseq [[[gx gy] [x y]] (map vector (golden-positions key) drawn)]
          (is (< (Math/abs (- (double gx) (double x))) 1.0e-9))
          (is (< (Math/abs (- (double gy) (double y))) 1.0e-9)))))))

(deftest test-dim-flux-layout-accepts-lattices
  (let [ctx  dwarf-planets
        from-context (dim-flux-layout ctx)
        from-lattice (dim-flux-layout (concept-lattice ctx))]
    (is (= (lay/positions from-context) (lay/positions from-lattice)))))

(deftest test-dim-flux-layout-parameters
  (testing "the refinement can be switched off by allowing no iteration"
    (let [layout (dim-flux-layout dwarf-planets {:iterations 0})]
      (is (line-diagram? layout))
      (is (doubly-additive? layout))))
  (testing "additional arguments are handed on to DimDraw"
    (let [layout (dim-flux-layout dwarf-planets {} "greedy")]
      (is (line-diagram? layout))
      (is (doubly-additive? layout)))))

(deftest test-dim-flux-layout-initial
  (testing "every starting diagram yields a valid additive line diagram"
    (doseq [ctx      all-contexts
            initial  [:dim-draw :greedy :planarity-enhancer :layered
                      simple-layered-layout]]
      (let [layout (dim-flux-layout ctx {:initial initial})]
        (is (line-diagram? layout)
            (str "line diagram for " initial))
        (is (doubly-additive? layout)
            (str "additive for " initial))
        (is (= (edges-of (concept-lattice ctx)) (set (lay/connections layout)))))))
  (testing "an unknown starting diagram is rejected"
    (is (thrown? IllegalArgumentException
                 (dim-flux-layout dwarf-planets {:initial :nonsense})))))

(deftest test-planarity-enhancer-layout
  (doseq [ctx all-contexts]
    (let [lattice (concept-lattice ctx)
          layout  (planarity-enhancer-layout lattice)]
      (testing "the enhancer alone already draws the lattice"
        (is (= (base-set lattice) (set (keys (lay/positions layout)))))
        (is (= (edges-of lattice) (set (lay/connections layout)))))
      (testing "and does so as a doubly-additive line diagram"
        (is (line-diagram? layout))
        (is (doubly-additive? layout)))
      (testing "with the object vectors pointing up and the attribute vectors down,
                which is what makes the diagram respect the order by construction"
        (let [pos (lay/positions layout)]
          (doseq [[a b] (lay/connections layout)]
            (is (< (second (pos a)) (second (pos b))))))))))

(deftest test-spring-defaults-reach-the-converged-order
  (testing "the spring model of the planarity enhancer feeds exactly one thing
            into the rest of the algorithm, the linear order of the irreducible
            elements, and the default bounds are high enough to reach the order a
            far longer run settles on"
    (let [sup-inf-distances @#'conexp.layouts.dim-flux/sup-inf-distances
          spring-positions  @#'conexp.layouts.dim-flux/spring-positions
          element-scalars   @#'conexp.layouts.dim-flux/element-scalars]
      (doseq [ctx all-contexts]
        (let [lattice (concept-lattice ctx)
              objects (vec (lattice-sup-irreducibles lattice))
              attrs   (vec (lattice-inf-irreducibles lattice))
              ne      (+ (count objects) (count attrs))
              dsi     (sup-inf-distances lattice objects attrs)
              order-of (fn [parameters]
                         (->> (element-scalars (spring-positions dsi ne parameters) ne)
                              (map-indexed vector)
                              (sort-by second)
                              (mapv first)))]
          (is (= (order-of {:spring-iterations 4000 :spring-evaluations 1000000})
                 (order-of default-parameters))))))))

(deftest test-dim-flux-layout-rejects-other-arguments
  (is (thrown? IllegalArgumentException (dim-flux-layout 42)))
  (is (thrown? IllegalArgumentException
               (dim-flux-layout (simple-layered-layout (concept-lattice chain-3))))))

;;;

nil
