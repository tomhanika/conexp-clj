;; Copyright ⓒ the conexp-clj developers; all rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.layouts.dim-flux
  "DimFlux, force-directed doubly-additive line diagrams.

  DimFlux draws a concept lattice in three steps:

    1. a realizer-embedded diagram is computed with DimDraw,
    2. that diagram is orthogonally projected onto the space of
       doubly-additive diagrams,
    3. the resulting element vectors are refined by a force-directed
       placement maximizing the conflict distance between nodes and
       non-incident edges.

  See M. Nöhre, D. Dürrschnabel, B. Ganter, G. Stumme, `DimFlux:
  Force-Directed Additive Line Diagrams', International Journal of
  Approximate Reasoning 197 (2026) 109734,
  https://arxiv.org/abs/2603.16366"
  (:require [conexp.base :refer [illegal-argument]]
            [conexp.fca.contexts :refer [context?]]
            [conexp.fca.lattices :refer [concept-lattice inf sup
                                         lattice-atoms lattice-coatoms
                                         lattice-inf-irreducibles
                                         lattice-sup-irreducibles
                                         lattice-upper-neighbours]]
            [conexp.layouts.base :as lay]
            [conexp.layouts.dim-draw :refer [dim-draw-layout]]
            [conexp.layouts.layered :refer [simple-layered-layout]]
            [conexp.math.algebra :refer [base-set order]]
            [conexp.math.optimize :refer [minimize-with-gradient]])
  (:import [conexp.fca.lattices Lattice]
           [org.apache.commons.math3.linear Array2DRowRealMatrix RealMatrix
                                            SingularValueDecomposition]))

;;; Parameters

(def default-parameters
  "Default parameters of the force-directed refinement.

  The three weights are those of the reference implementation of the DimFlux
  paper, :iterations bounds the number of conjugate gradient iterations and
  :min-distance is the smallest conflict distance the repulsive energy is
  evaluated at, which keeps degenerate placements from producing infinite
  energies.

  :initial chooses the diagram the refinement starts from and is the parameter
  to reach for on larger lattices, where the two-dimension extension of DimDraw
  dominates the running time.  See `initial-layout`.

  :spring-iterations and :spring-evaluations bound the spring model of the
  planarity enhancer and are ignored by every other starting diagram.  The
  spring model feeds exactly one thing into the rest of the algorithm, the
  linear order of the irreducible elements, and on every lattice tried that
  order stopped changing at 100 iterations; the bound of 200 is a safety net
  rather than the operative limit, since the optimizer converges and stops on
  its own well before it."
  {:w-rep               50.0
   :w-att                1.0
   :w-grav              30.0
   :iterations        1000
   :min-distance         1.0e-3
   :initial           :dim-draw
   :spring-iterations  200
   :spring-evaluations 2000})

;;; The doubly-additive set representation
;;
;; For a finite lattice L the standard context is (J(L), M(L), <=), where J(L)
;; are the supremum irreducible and M(L) the infimum irreducible elements.  Its
;; doubly-additive set representation places a node c at
;;
;;   POS(c) = sum {VEC(g) | g in J(L), g <= c} + sum {VEC(m) | m in M(L), c not<= m}.
;;
;; For the force model we use the equivalent dual form, in which every node is
;; the sum of the vectors of the objects below it and of the attributes above
;; it, the attribute vectors being negated.  The two forms differ by the sum of
;; all attribute vectors, that is by a translation of the whole diagram.

(defn- irreducible-elements
  "Returns `[objects attributes]` of the standard context of `lattice`, that is
  its supremum irreducible and its infimum irreducible elements, each as a
  vector."
  [lattice]
  [(vec (lattice-sup-irreducibles lattice))
   (vec (lattice-inf-irreducibles lattice))])

(defn- membership-array
  "Returns a flat `boolean[nc*ne]` whose entry `c*ne + e` states whether the
  vector of element `e` contributes to the position of node `c`, that is whether
  `e` is an object below `c` or an attribute above `c`."
  [lattice nodes objects attributes]
  (let [leq (order lattice)
        ng  (count objects)
        ne  (+ ng (count attributes))
        out (boolean-array (* (count nodes) ne))]
    (dotimes [c (count nodes)]
      (let [node (nth nodes c)]
        (dotimes [i ng]
          (aset out (+ (* c ne) i) (boolean (leq (nth objects i) node))))
        (dotimes [i (count attributes)]
          (aset out (+ (* c ne) ng i) (boolean (leq node (nth attributes i)))))))
    out))

(defn set-representation-matrix
  "Returns the set representation matrix (SRM) of `lattice` as a `double[nc][ne]`.

  Rows correspond to the nodes in the order of `nodes`, columns to the elements
  of the standard context, objects first.  The entry in row `c` and column `s`
  is 1 iff `s` belongs to the doubly-additive set representation of `c`, that is
  iff `s` is an object below `c` or an attribute not above `c`."
  [lattice nodes objects attributes]
  (let [ng     (count objects)
        ne     (+ ng (count attributes))
        member (membership-array lattice nodes objects attributes)]
    (into-array
     (for [c (range (count nodes))]
       (double-array
        (for [e (range ne)]
          (let [in? (aget ^booleans member (+ (* c ne) e))]
            (if (< e ng)
              (if in? 1.0 0.0)
              (if in? 0.0 1.0)))))))))

(defn- least-squares-solution
  "Returns the least squares solution `V` of `A V = B` as a `double[n][k]`, by
  way of the pseudo inverse of `A`.  `A V` is then the orthogonal projection of
  the columns of `B` onto the column space of `A`."
  [^"[[D" a ^"[[D" b]
  (.getData ^RealMatrix
            (.solve (.getSolver (SingularValueDecomposition.
                                 (Array2DRowRealMatrix. a false)))
                    ^RealMatrix (Array2DRowRealMatrix. b false))))

(defn- element-vectors-of
  "Returns the element vectors of the doubly-additive diagram closest to the
  given node positions, as a flat `double[2*ne]` in the dual convention, that is
  with negated attribute vectors."
  [^"[[D" srm ^"[[D" positions ^long ng]
  (let [^"[[D" solution (least-squares-solution srm positions)
        ne              (alength solution)
        out             (double-array (* 2 ne))]
    (dotimes [e ne]
      (let [^doubles row (aget solution e)
            sign         (double (if (< e ng) 1.0 -1.0))]
        (aset out (* 2 e) (* sign (aget row 0)))
        (aset out (inc (* 2 e)) (* sign (aget row 1)))))
    out))

;;; The physical model
;;
;; All forces act on the element vectors, not on the nodes themselves: moving a
;; single vector moves every node it contributes to, which is what keeps the
;; diagram additive throughout the optimization.

(defn- node-positions
  "Positions of all nodes as a flat `double[2*nc]`, given the element vectors `v`
  as a flat `double[2*ne]`."
  [model ^doubles v]
  (let [nc  (long (:nc model))
        idx (:node-elements model)
        out (double-array (* 2 nc))]
    (dotimes [c nc]
      (let [^ints elems (nth idx c)]
        (dotimes [k (alength elems)]
          (let [e (aget elems k)]
            (aset out (* 2 c)
                  (+ (aget out (* 2 c)) (aget v (* 2 e))))
            (aset out (inc (* 2 c))
                  (+ (aget out (inc (* 2 c))) (aget v (inc (* 2 e)))))))))
    out))

(defn- repulsive-energy-and-force
  "The repulsive energy `E_rep = sum_v sum_{v1v2} 1/d(w,f)`, taken over all nodes
  `v` and all edges `v1v2` not incident with `v`, together with the force it
  exerts on the element vectors.  Here `d` is the conflict distance between a
  node and a non-incident edge, which the repulsion maximizes.

  Only elements that can move the conflict distance at all are visited.  An
  object matters exactly if it lies below the upper end of the edge but not
  below the node, or below the node but not below the lower end; dually for an
  attribute.  Both sets are intersections of words of the membership bit sets,
  so the inner loop runs over a handful of elements instead of over all of
  them."
  [model ^doubles pos]
  (let [nc              (long (:nc model))
        ne              (long (:ne model))
        ng              (long (:ng model))
        words           (long (:words model))
        ^"[[J" bits     (:bits model)
        ^longs obj-mask (:object-mask model)
        ^longs att-mask (:attribute-mask model)
        ^ints edge-lo   (:edge-lo model)
        ^ints edge-hi   (:edge-hi model)
        n-edges         (alength edge-lo)
        min-d           (double (:min-distance model))
        force           (double-array (* 2 ne))
        energy          (double-array 1)]
    (dotimes [c nc]
      (let [wx        (aget pos (* 2 c))
            wy        (aget pos (inc (* 2 c)))
            ^longs bc (aget bits c)]
        (dotimes [ei n-edges]
          (let [v1 (aget edge-lo ei)
                v2 (aget edge-hi ei)]
            (when (and (not= c v1) (not= c v2))
              (let [^longs b1 (aget bits v1)
                    ^longs b2 (aget bits v2)
                    w1x  (aget pos (* 2 v1)), w1y (aget pos (inc (* 2 v1)))
                    w2x  (aget pos (* 2 v2)), w2y (aget pos (inc (* 2 v2)))
                    fx   (- w2x w1x), fy (- w2y w1y)
                    flen (Math/max (Math/sqrt (+ (* fx fx) (* fy fy))) min-d)
                    d1   (Math/sqrt (+ (* (- w1x wx) (- w1x wx))
                                       (* (- w1y wy) (- w1y wy))))
                    d2   (Math/sqrt (+ (* (- w2x wx) (- w2x wx))
                                       (* (- w2y wy) (- w2y wy))))
                    ;; where the node lies relative to the edge
                    below? (> (+ (* (- w1x wx) fx) (* (- w1y wy) fy)) 0.0)
                    above? (< (+ (* (- w2x wx) fx) (* (- w2y wy) fy)) 0.0)
                    cross  (- (* (- w1x wx) (- w2y wy)) (* (- w1y wy) (- w2x wx)))
                    ;; +1 if the node lies left of the edge, -1 otherwise, so
                    ;; that the force always pushes it away
                    l      (double (if (>= cross 0.0) 1.0 -1.0))
                    dist   (Math/max min-d (double (cond below? d1
                                                         above? d2
                                                         :else  (/ (Math/abs cross) flen))))
                    scale  (/ 1.0 (* dist dist))
                    ;; direction the conflict distance grows in
                    ux     (double (cond below? (/ (- w1x wx) dist)
                                         above? (/ (- w2x wx) dist)
                                         :else  (/ (* (- fy) l) flen)))
                    uy     (double (cond below? (/ (- w1y wy) dist)
                                         above? (/ (- w2y wy) dist)
                                         :else  (/ (* fx l) flen)))
                    ;; scaled variants for elements moving only one end of the edge
                    s1     (/ (Math/sqrt (Math/abs (- (* d1 d1) (* dist dist)))) flen)
                    s2     (/ (Math/sqrt (Math/abs (- (* d2 d2) (* dist dist)))) flen)]
                (aset energy 0 (+ (aget energy 0) (/ 1.0 dist)))
                (dotimes [w words]
                  (let [bcw (aget bc w)
                        b1w (aget b1 w)
                        b2w (aget b2 w)
                        relevant (bit-or
                                  (bit-and (aget obj-mask w)
                                           (bit-or (bit-and b2w (bit-not bcw))
                                                   (bit-and bcw (bit-not b1w))))
                                  (bit-and (aget att-mask w)
                                           (bit-or (bit-and b1w (bit-not bcw))
                                                   (bit-and bcw (bit-not b2w)))))]
                    (loop [m (long relevant)]
                      (when-not (zero? m)
                        (let [b       (Long/numberOfTrailingZeros m)
                              bit     (bit-shift-left 1 b)
                              e       (+ (* w 64) b)
                              object? (< e ng)
                              ;; the eight ways an element can be distributed
                              ;; over the node and the two ends of the edge,
                              ;; numbered as in the paper
                              k       (inc (bit-or (if (zero? (bit-and bcw bit)) 0 4)
                                                   (if (zero? (bit-and b1w bit)) 0 2)
                                                   (if (zero? (bit-and b2w bit)) 0 1)))
                              factor  (double
                                       (cond
                                         below?
                                         (cond (if object? (= k 4) (or (= k 3) (= k 4)))   1.0
                                               (if object? (or (= k 5) (= k 6)) (= k 5))  -1.0
                                               :else                                       0.0)
                                         above?
                                         (cond (if object? (or (= k 2) (= k 4)) (= k 4))   1.0
                                               (if object? (= k 5) (or (= k 5) (= k 7)))  -1.0
                                               :else                                       0.0)
                                         :else
                                         (cond (and object? (= k 2))       (- s1)
                                               (and (not object?) (= k 3)) (- s2)
                                               (= k 4)                     -1.0
                                               (= k 5)                      1.0
                                               (and object? (= k 6))        s2
                                               (and (not object?) (= k 7))  s1
                                               :else                        0.0)))]
                          (when-not (zero? factor)
                            (aset force (* 2 e)
                                  (+ (aget force (* 2 e)) (* scale factor ux)))
                            (aset force (inc (* 2 e))
                                  (+ (aget force (inc (* 2 e))) (* scale factor uy)))))
                        (recur (bit-and m (dec m)))))))))))))
    [(aget energy 0) force]))

(defn- attractive-energy-and-force
  "The attractive energy `E_att = sum_{v1v2} |f|^2`, taken over all edges, which
  keeps the diagram from falling apart, together with the force it exerts on the
  element vectors.

  Only elements separating the two ends of an edge can shorten it: objects of
  the upper end pull it down, attributes of the lower end pull it up.  Both sets
  come straight out of the membership bit sets."
  [model ^doubles pos]
  (let [ne              (long (:ne model))
        ng              (long (:ng model))
        words           (long (:words model))
        ^"[[J" bits     (:bits model)
        ^longs obj-mask (:object-mask model)
        ^longs att-mask (:attribute-mask model)
        ^ints edge-lo   (:edge-lo model)
        ^ints edge-hi   (:edge-hi model)
        n-edges         (alength edge-lo)
        force           (double-array (* 2 ne))
        energy          (double-array 1)]
    (dotimes [ei n-edges]
      (let [v1        (aget edge-lo ei)
            v2        (aget edge-hi ei)
            ^longs b1 (aget bits v1)
            ^longs b2 (aget bits v2)
            dx        (- (aget pos (* 2 v2)) (aget pos (* 2 v1)))
            dy        (- (aget pos (inc (* 2 v2))) (aget pos (inc (* 2 v1))))]
        (aset energy 0 (+ (aget energy 0) (* dx dx) (* dy dy)))
        (dotimes [w words]
          (let [b1w (aget b1 w)
                b2w (aget b2 w)
                relevant (bit-or (bit-and (aget obj-mask w) b2w (bit-not b1w))
                                 (bit-and (aget att-mask w) b1w (bit-not b2w)))]
            (loop [m (long relevant)]
              (when-not (zero? m)
                (let [b    (Long/numberOfTrailingZeros m)
                      e    (+ (* w 64) b)
                      sign (double (if (< e ng) -1.0 1.0))]
                  (aset force (* 2 e)
                        (+ (aget force (* 2 e)) (* 2.0 sign dx)))
                  (aset force (inc (* 2 e))
                        (+ (aget force (inc (* 2 e))) (* 2.0 sign dy))))
                (recur (bit-and m (dec m)))))))))
    [(aget energy 0) force]))

(defn- gravitational-energy-and-force
  "The gravitational energy penalizing element vectors that leave their safe
  zone, together with the force it exerts on them.  Object vectors are meant to
  point upwards and attribute vectors downwards, each staying at an angle of at
  least `phi_0` away from the horizontal axis."
  [model ^doubles v]
  (let [ne      (long (:ne model))
        ng      (long (:ng model))
        phi0-g  (double (:phi0-g model))
        phi0-m  (double (:phi0-m model))
        ;; integration constants making the energy continuous at the border of
        ;; the safe zone
        e0      (- (- phi0-g) (* (Math/sin phi0-g) (Math/cos phi0-g)))
        e1      (+ e0 Math/PI)
        e2      (+ phi0-m (* (Math/sin phi0-m) (Math/cos phi0-m)))
        e3      (- e2 Math/PI)
        penalty 1.0e3
        force   (double-array (* 2 ne))
        energy  (double-array 1)]
    (dotimes [e ne]
      (let [x (aget v (* 2 e))
            y (aget v (inc (* 2 e)))]
        (when (>= (Math/abs y) 1.0e-15)
          (let [object?     (< e ng)
                phi0        (double (if object? phi0-g phi0-m))
                phi         (let [p (Math/atan2 y x)]
                              (if (<= (Math/abs p) 1.0e-15)
                                (if object? 1.0e-3 -1.0e-3)
                                p))
                flat-right? (if object?
                              (and (<= 0.0 phi) (<= phi phi0))
                              (and (<= (- phi0) phi) (<= phi 0.0)))
                flat-left?  (if object?
                              (and (<= (- Math/PI phi0) phi) (<= phi Math/PI))
                              (and (<= (- Math/PI) phi) (<= phi (+ (- Math/PI) phi0))))
                wrong-way?  (if object? (< phi 0.0) (> phi 0.0))]
            (cond
              (or flat-right? flat-left?)
              (let [dir (double (if flat-right?
                                  (if object? 1.0 -1.0)
                                  (if object? -1.0 1.0)))
                    cot (/ (Math/cos phi) (Math/sin phi))
                    sq  (* (Math/sin phi0) (Math/sin phi0))
                    de  (if flat-right?
                          (+ phi (* cot sq) (if object? e0 e2))
                          (+ (- phi) (- (* cot sq)) (if object? e1 e3)))
                    fac (* (/ (- (* (Math/sin phi) (Math/sin phi)) sq) (* y y)) dir)]
                (aset energy 0 (+ (aget energy 0) (double de)))
                (aset force (* 2 e) (+ (aget force (* 2 e)) (* y fac)))
                (aset force (inc (* 2 e))
                      (+ (aget force (inc (* 2 e))) (* (- x) fac))))
              ;;
              wrong-way?
              (do (aset energy 0 (+ (aget energy 0) (* penalty y y)))
                  (aset force (inc (* 2 e))
                        (+ (aget force (inc (* 2 e))) (* -2.0 penalty y))))
              ;;
              :else nil)))))
    [(aget energy 0) force]))

(defn- total-energy-and-force
  "Returns `[energy force]` of the whole physical model at the element vectors
  `v`, where `force` is the negative gradient of the energy."
  [model ^doubles v]
  (let [pos             (node-positions model v)
        [e-rep  f-rep]  (repulsive-energy-and-force model pos)
        [e-att  f-att]  (attractive-energy-and-force model pos)
        [e-grav f-grav] (gravitational-energy-and-force model v)
        w-rep           (double (:w-rep model))
        w-att           (double (:w-att model))
        w-grav          (double (:w-grav model))
        force           (double-array (alength v))]
    (dotimes [i (alength v)]
      (aset force i (+ (* w-rep (aget ^doubles f-rep i))
                       (* w-att (aget ^doubles f-att i))
                       (* w-grav (aget ^doubles f-grav i)))))
    [(+ (* w-rep (double e-rep))
        (* w-att (double e-att))
        (* w-grav (double e-grav)))
     force]))

(defn- cached-energy-and-force
  "Returns a function of the element vectors caching the last computed
  `[energy force]`, so that the optimizer evaluating energy and gradient at the
  same point does not compute the model twice."
  [model]
  (let [cache (atom nil)]
    (fn [^doubles v]
      (let [key (vec v)]
        (if-let [[k result] @cache]
          (if (= k key)
            result
            (let [result (total-energy-and-force model v)]
              (reset! cache [key result])
              result))
          (let [result (total-energy-and-force model v)]
            (reset! cache [key result])
            result))))))

;;; Putting it together

(defn- make-model
  "Precomputes everything the force model needs from `lattice`: the nodes, the
  elements of its standard context, the membership relation between them, the
  edges of the diagram and the parameters."
  [lattice nodes objects attributes parameters]
  (let [ng     (count objects)
        nm     (count attributes)
        ne     (+ ng nm)
        nc     (count nodes)
        words  (max 1 (int (Math/ceil (/ ne 64.0))))
        member (membership-array lattice nodes objects attributes)
        index  (into {} (map-indexed (fn [i c] [c i]) nodes))]
    (merge parameters
           {:nc            nc
            :ne            ne
            :ng            ng
            :nm            nm
            :member        member
            :phi0-g        (/ Math/PI (inc ng))
            :phi0-m        (/ Math/PI (inc nm))
            :node-elements (vec (for [c (range nc)]
                                  (int-array (filter #(aget ^booleans member (+ (* c ne) %))
                                                     (range ne)))))
            ;; the membership relation once more, packed into words, so that the
            ;; forces can single out the elements they have to look at
            :words         words
            :bits          (into-array
                            (for [c (range nc)]
                              (let [row (long-array words)]
                                (dotimes [e ne]
                                  (when (aget ^booleans member (+ (* c ne) e))
                                    (aset row (quot e 64)
                                          (bit-set (aget row (quot e 64)) (rem e 64)))))
                                row)))
            :object-mask   (let [mask (long-array words)]
                             (dotimes [e ng]
                               (aset mask (quot e 64)
                                     (bit-set (aget mask (quot e 64)) (rem e 64))))
                             mask)
            :attribute-mask (let [mask (long-array words)]
                              (dotimes [i nm]
                                (let [e (+ ng i)]
                                  (aset mask (quot e 64)
                                        (bit-set (aget mask (quot e 64)) (rem e 64)))))
                              mask)
            :edge-lo       (int-array (for [n nodes, u (lattice-upper-neighbours lattice n)]
                                        (index n)))
            :edge-hi       (int-array (for [n nodes, u (lattice-upper-neighbours lattice n)]
                                        (index u)))
            :edges         (vec (for [n nodes, u (lattice-upper-neighbours lattice n)]
                                  [(index n) (index u)]))})))

(defn- positions-array
  "The positions of `nodes` in `layout` as a `double[nc][2]`."
  [layout nodes]
  (let [pos (lay/positions layout)]
    (into-array (for [c nodes]
                  (let [[x y] (get pos c)]
                    (double-array [(double x) (double y)]))))))

(defn- diagram-edges
  "The edges of the line diagram of `lattice`, as pairs of a node and one of its
  upper neighbours."
  [lattice nodes]
  (vec (for [n nodes, u (lattice-upper-neighbours lattice n)]
         [n u])))

(defn- layout-of-vectors
  "Builds the layout of `lattice` placing every node at the sum of the vectors of
  the objects below and the attributes above it.

  The whole diagram is shifted by the sum of all attribute vectors, which turns
  the dual placement used by the force model into the placement of the set
  representation itself, so that the result lies in the space of doubly-additive
  diagrams and not merely in a translate of it."
  [lattice nodes model ^doubles v]
  (let [pos (node-positions model v)
        ng  (long (:ng model))
        ne  (long (:ne model))
        ox  (double (reduce (fn [s e] (- s (aget v (* 2 e)))) 0.0 (range ng ne)))
        oy  (double (reduce (fn [s e] (- s (aget v (inc (* 2 e))))) 0.0 (range ng ne)))]
    (lay/make-layout-nc lattice
                        (into {} (map-indexed (fn [i c]
                                                [c [(+ ox (aget pos (* 2 i)))
                                                    (+ oy (aget pos (inc (* 2 i))))]])
                                              nodes))
                        (diagram-edges lattice nodes))))

(defn- respects-order?
  "Tests whether `layout` is a line diagram, that is whether every node is placed
  strictly below all of its upper neighbours."
  [layout]
  (let [pos (lay/positions layout)]
    (every? (fn [[a b]] (< (double (second (get pos a)))
                           (double (second (get pos b)))))
            (lay/connections layout))))

(defn- sup-inf-distances
  "The doubly-additive sup-inf distance between every pair of irreducible
  elements, as a flat `double[ne*ne]`.

  For two incomparable elements the distance counts how much the interval
  between their meet and their join spans.  Written over the standard context
  this needs closures of unions of extents and intents, but the closure of a
  union of extents is the extent of the join and dually for intents, so all four
  quantities are just counts of irreducibles below the meet and above the join.
  Comparable pairs get distance zero.

  See Section 6.1 of the DimFlux paper, extending the sup-inf distance of
  C. Zschalig from the attribute-additive to the doubly-additive case."
  [lattice objects attributes]
  (let [leq       (order lattice)
        meet      (inf lattice)
        join      (sup lattice)
        elements  (vec (concat objects attributes))
        ng        (count objects)
        ne        (count elements)
        object?   (fn [i] (< (long i) ng))
        below     (memoize (fn [x] (count (filter #(leq % x) objects))))
        above     (memoize (fn [x] (count (filter #(leq x %) attributes))))
        out       (double-array (* ne ne))]
    (doseq [i (range ne), j (range (inc (long i)) ne)]
      (let [a (nth elements i)
            b (nth elements j)
            d (cond
                ;; two objects, or two attributes
                (= (object? i) (object? j))
                (if (or (leq a b) (leq b a))
                  0.0
                  (if (object? i)
                    (- (below (join a b)) (below (meet a b)) 1)
                    (- (above (meet a b)) (above (join a b)) 1)))
                ;; an object and an attribute; the reference implementation
                ;; treats the pair as incomparable exactly when the object does
                ;; not lie below the attribute
                :else
                (let [g (if (object? i) a b)
                      m (if (object? i) b a)]
                  (if (leq g m)
                    0.0
                    (let [meet-gm (meet g m)
                          join-gm (join g m)]
                      (- (- (below meet-gm) (above join-gm))
                         (- (below join-gm) (above meet-gm))
                         1)))))]
        (aset out (+ (* (long i) ne) (long j)) (double d))
        (aset out (+ (* (long j) ne) (long i)) (double d))))
    out))

(defn- spring-energy-and-gradient
  "The energy `E_SI = sum_{i<j} (|n_i - n_j| - d_SI(i,j))^2` of the spring model
  and its gradient, in one pass over the pairs."
  [^doubles dsi ^long ne ^doubles p]
  (let [grad   (double-array (* 2 ne))
        energy (double-array 1)]
    (dotimes [i ne]
      (let [xi (aget p (* 2 i))
            yi (aget p (inc (* 2 i)))]
        (loop [j (inc i)]
          (when (< j ne)
            (let [dx (- xi (aget p (* 2 j)))
                  dy (- yi (aget p (inc (* 2 j))))
                  r  (Math/max (Math/sqrt (+ (* dx dx) (* dy dy))) 1.0e-9)
                  t  (- r (aget dsi (+ (* i ne) j)))
                  f  (* 2.0 (/ t r))]
              (aset energy 0 (+ (aget energy 0) (* t t)))
              (aset grad (* 2 i) (+ (aget grad (* 2 i)) (* f dx)))
              (aset grad (inc (* 2 i)) (+ (aget grad (inc (* 2 i))) (* f dy)))
              (aset grad (* 2 j) (- (aget grad (* 2 j)) (* f dx)))
              (aset grad (inc (* 2 j)) (- (aget grad (inc (* 2 j))) (* f dy))))
            (recur (inc j))))))
    [(aget energy 0) grad]))

(defn- spring-positions
  "Places the irreducible elements in the plane so that their Euclidean
  distances approximate the sup-inf distances, by minimizing

      E_SI = sum_{i<j} (|n_i - n_j| - d_SI(i,j))^2

  from a start on the unit circle.  Returns a flat `double[2*ne]`.

  The minimization is bounded because this is only the starting point of a
  starting point.  All that reaches the rest of the algorithm is the linear
  order the layout induces, and that order stops changing long before the
  positions do."
  [^doubles dsi ^long ne parameters]
  (let [start (mapcat (fn [i]
                        (let [phi (/ (* 2.0 Math/PI i) ne)]
                          [(Math/cos phi) (Math/sin phi)]))
                      (range ne))
        cache (atom nil)
        both  (fn [^doubles p]
                (let [key (vec p)]
                  (if-let [[k r] @cache]
                    (if (= k key)
                      r
                      (let [r (spring-energy-and-gradient dsi ne p)]
                        (reset! cache [key r]) r))
                    (let [r (spring-energy-and-gradient dsi ne p)]
                      (reset! cache [key r]) r))))]
    (double-array
     (try (first (minimize-with-gradient
                  (fn [^doubles p] (first (both p)))
                  (fn [^doubles p] (seq ^doubles (second (both p))))
                  start
                  {:iterations      (:spring-iterations parameters 200)
                   :max-evaluations (:spring-evaluations parameters 2000)}))
          (catch Exception _ start)))))

(defn- element-scalars
  "Projects the spring layout onto the axis spanned by its two most distant
  points, which turns it into the linear order of the irreducible elements the
  placement then follows."
  [^doubles pts ^long ne]
  (let [pt   (fn [i] [(aget pts (* 2 (long i))) (aget pts (inc (* 2 (long i))))])
        [i j] (if (< ne 2)
                [0 0]
                (apply max-key
                       (fn [[a b]]
                         (let [[ax ay] (pt a), [bx by] (pt b)]
                           (+ (* (- bx ax) (- bx ax)) (* (- by ay) (- by ay)))))
                       (for [a (range ne), b (range (inc (long a)) ne)] [a b])))
        ;; the left of the two points anchors the axis
        [n1 n2] (let [[ix _] (pt i), [jx _] (pt j)]
                  (if (< (double ix) (double jx)) [(pt i) (pt j)] [(pt j) (pt i)]))
        [n1x n1y] n1
        [n2x n2y] n2
        fx      (- (double n2x) (double n1x))
        fy      (- (double n2y) (double n1y))]
    (vec (for [k (range ne)]
           (let [[x y] (pt k)]
             (+ (* (- (double x) (double n1x)) fx)
                (* (- (double y) (double n1y)) fy)))))))

(defn- round-to-tenth
  ^double [^double x]
  (/ (Math/round (* 10.0 x)) 10.0))

(defn planarity-enhancer-layout
  "Returns the initial doubly-additive layout of `lattice` computed by the
  planarity enhancer, the alternative to DimDraw offered by the DimFlux paper.

  The irreducible elements are first laid out by a spring model whose rest
  lengths are their sup-inf distances, which spreads apart those elements whose
  chains would otherwise cross.  Projecting that layout onto its longest axis
  gives a linear order.  Following it, the atoms are placed along an upward
  parabola and the coatoms along its mirror image, and the remaining elements
  are filled in by chain decomposition: an object goes to the mean of the
  objects below it, an attribute to the mean of the attributes above it, each
  shifted slightly so that elements sharing their neighbours do not coincide.

  This costs time quadratic in the number of irreducible elements rather than in
  the number of concepts, and involves no satisfiability search, which is what
  makes it the cheap alternative to the two-dimension extension of DimDraw.

  See Section 6.1 of the DimFlux paper and C. Zschalig, `A Force Directed
  Approach to Drawing Concept Lattices', for the attribute-additive original."
  ([lattice]
   (planarity-enhancer-layout lattice default-parameters))
  ([lattice parameters]
   (let [[objects attributes] (irreducible-elements lattice)
        ng       (count objects)
        nm       (count attributes)
        ne       (+ ng nm)
        nodes    (vec (base-set lattice))]
    (if (zero? ne)
      (simple-layered-layout lattice)
      (let [leq      (order lattice)
            dsi      (sup-inf-distances lattice objects attributes)
            scalars  (element-scalars (spring-positions dsi ne parameters) ne)
            obj-idx  (into {} (map-indexed (fn [i g] [g i]) objects))
            att-idx  (into {} (map-indexed (fn [i m] [m (+ ng i)]) attributes))
            vectors  (double-array (* 2 ne))
            place!   (fn [i x y]
                       (aset vectors (* 2 (long i)) (double x))
                       (aset vectors (inc (* 2 (long i))) (double y)))
            ;; the atoms ride an upward parabola, the coatoms its mirror image,
            ;; both spaced 1.8 apart and centred on the origin
            parabola (fn [elements index up?]
                       (let [ordered (sort-by #(nth scalars (index %)) elements)
                             n       (count ordered)]
                         (doseq [[k e] (map-indexed vector ordered)]
                           (let [x (round-to-tenth (- (* 1.8 (inc (long k)))
                                                      (* 0.9 (inc n))))
                                 y (+ (* 0.09 x x) 1.75)]
                             (if up?
                               (place! (index e) x y)
                               (place! (index e) (- x) (- y)))))))
            _        (parabola (filter obj-idx (lattice-atoms lattice)) obj-idx true)
            _        (parabola (filter att-idx (lattice-coatoms lattice)) att-idx false)
            ;; chain decomposition, bottom up for objects and top down for
            ;; attributes; a strictly ordered pass guarantees the neighbours are
            ;; already placed
            done-obj (atom (set (filter obj-idx (lattice-atoms lattice))))
            done-att (atom (set (filter att-idx (lattice-coatoms lattice))))]
        (doseq [g (sort-by #(count (filter (fn [h] (leq h %)) objects))
                           (remove @done-obj objects))]
          (let [lower (filter #(and (leq % g) (not= % g)) objects)]
            (if (empty? lower)
              (place! (obj-idx g) (* 1.0e-3 (obj-idx g)) 1.75)
              (let [n  (count lower)
                    mx (/ (reduce + (map #(aget vectors (* 2 (long (obj-idx %)))) lower)) n)
                    my (/ (reduce + (map #(aget vectors (inc (* 2 (long (obj-idx %)))))
                                         lower)) n)
                    d  (* 1.0e-3 (obj-idx g))]
                (place! (obj-idx g) (+ mx d) (+ my d))))
            (swap! done-obj conj g)))
        (doseq [m (sort-by #(- (count (filter (fn [k] (leq % k)) attributes)))
                           (remove @done-att attributes))]
          (let [upper (filter #(and (leq m %) (not= m %)) attributes)]
            (if (empty? upper)
              (place! (att-idx m) (* 1.0e-3 (att-idx m)) -1.75)
              (let [n  (count upper)
                    mx (/ (reduce + (map #(aget vectors (* 2 (long (att-idx %)))) upper)) n)
                    my (/ (reduce + (map #(aget vectors (inc (* 2 (long (att-idx %)))))
                                         upper)) n)
                    d  (* 1.0e-3 (att-idx m))]
                (place! (att-idx m) (+ mx d) (+ my d))))
            (swap! done-att conj m)))
        (layout-of-vectors lattice nodes
                           (make-model lattice nodes objects attributes parameters)
                           vectors))))))

(defn- rotated-dim-draw-layout
  "The DimDraw diagram of `lattice`, scaled to the canonical aspect ratio of the
  paper: the realizer coordinates are turned by 45 degrees, then stretched
  horizontally and squeezed vertically."
  [lattice dim-draw-args]
  (let [layout (apply dim-draw-layout lattice dim-draw-args)]
    (lay/update-positions
     layout
     (into {} (for [[c [x y]] (lay/positions layout)]
                [c [(* (Math/sqrt 2.0) (double x))
                    (/ (double y) (Math/sqrt 2.0))]])))))

(defn initial-layout
  "The diagram the force-directed refinement starts from, chosen by the :initial
  parameter:

    :dim-draw  the realizer-embedded diagram of DimDraw, using its exact
               two-dimension extension.  This is the algorithm of the paper and
               the default, and it is what dominates the running time as soon as
               a lattice grows past roughly 25 elements.
    :greedy    the same, but with the greedy two-dimension extension of DimDraw,
               which gives up minimality of the inserted relation in exchange for
               dropping the SAT search.
    :planarity-enhancer
               the initial layout of the paper's own alternative to DimDraw,
               costing quadratic time in the number of irreducible elements
               rather than in the number of concepts and using no satisfiability
               search.  See `planarity-enhancer-layout`.
    :layered   the simple layered layout, skipping DimDraw altogether.  The
               cheapest start, and the crudest.

  A function of the lattice returning a layout may be given instead, which is how
  any other layout of conexp-clj can serve as the starting diagram."
  [lattice parameters dim-draw-args]
  (let [initial (:initial parameters :dim-draw)]
    (cond
      (fn? initial)         (initial lattice)
      (= :dim-draw initial) (rotated-dim-draw-layout lattice dim-draw-args)
      (= :greedy initial)   (rotated-dim-draw-layout lattice (cons "greedy" dim-draw-args))
      (= :layered initial)  (simple-layered-layout lattice)
      (= :planarity-enhancer initial) (planarity-enhancer-layout lattice parameters)
      :else (illegal-argument "Unknown initial layout for DimFlux: " initial))))

(defn- as-lattice
  "Returns the lattice to be drawn, accepting a lattice or a formal context."
  [thing]
  (let [lattice (if (context? thing) (concept-lattice thing) thing)]
    (when-not (instance? Lattice lattice)
      (illegal-argument "DimFlux needs a lattice or a formal context."))
    lattice))

;;; Projection into the space of additive diagrams

(defn additive-layout
  "Returns the doubly-additive layout closest to `layout`, whose poset must be a
  lattice.  This is the orthogonal projection of the placement onto the space of
  doubly-additive placements of the standard context, computed with the pseudo
  inverse of the set representation matrix.

  A layout is doubly-additive exactly if it equals its own projection."
  [layout]
  (let [lattice (lay/poset layout)]
    (when-not (instance? Lattice lattice)
      (illegal-argument "The layout must be that of a lattice."))
    (let [nodes                (vec (base-set lattice))
          [objects attributes] (irreducible-elements lattice)]
      (if (empty? (concat objects attributes))
        layout
        (let [srm       (set-representation-matrix lattice nodes objects attributes)
              vectors   (least-squares-solution srm (positions-array layout nodes))
              ^RealMatrix projected
              (.multiply (Array2DRowRealMatrix. ^"[[D" srm false)
                         (Array2DRowRealMatrix. ^"[[D" vectors false))]
          (lay/update-positions
           layout
           (into {} (map-indexed (fn [i c]
                                   [c [(.getEntry projected i 0)
                                       (.getEntry projected i 1)]])
                                 nodes))))))))

(defn doubly-additive?
  "Tests whether `layout` is a doubly-additive layout of its lattice, up to the
  given tolerance (1e-6 by default)."
  ([layout]
   (doubly-additive? layout 1.0e-6))
  ([layout tolerance]
   (let [projected (lay/positions (additive-layout layout))]
     (every? (fn [[node [x y]]]
               (let [[px py] (get projected node)]
                 (and (< (Math/abs (- (double x) (double px))) (double tolerance))
                      (< (Math/abs (- (double y) (double py))) (double tolerance)))))
             (lay/positions layout)))))

;;; DimFlux

(defn dim-flux-layout
  "Returns a DimFlux layout for the given lattice or formal context.

  The initial diagram is computed with DimDraw, projected onto the closest
  doubly-additive diagram and then refined by a force-directed placement
  maximizing the conflict distance between nodes and non-incident edges.  Should
  the refinement not yield a valid line diagram, the projected diagram is
  returned instead, and should that one be invalid as well, the initial diagram
  is.

  When the first argument after the lattice is a map, it overrides
  `default-parameters`; all remaining arguments are passed on to DimDraw.

  On lattices past roughly 25 elements the exact two-dimension extension of
  DimDraw dominates the running time.  The :initial parameter then buys most of
  it back, at the price of a weaker starting diagram; see `initial-layout`.

  See M. Nöhre, D. Dürrschnabel, B. Ganter, G. Stumme, `DimFlux:
  Force-Directed Additive Line Diagrams', International Journal of
  Approximate Reasoning 197 (2026) 109734,
  https://arxiv.org/abs/2603.16366"
  [lattice-or-context & args]
  (let [lattice                    (as-lattice lattice-or-context)
        [parameters dim-draw-args] (if (map? (first args))
                                     [(merge default-parameters (first args)) (rest args)]
                                     [default-parameters args])
        nodes                      (vec (base-set lattice))
        [objects attributes]       (irreducible-elements lattice)
        ;; step 1: the diagram to start from, by default the realizer-embedded
        ;; one of DimDraw
        start                      (initial-layout lattice parameters dim-draw-args)]
    (if (empty? (concat objects attributes))
      start
      (let [model     (make-model lattice nodes objects attributes parameters)
            srm       (set-representation-matrix lattice nodes objects attributes)
            ;; step 2: the closest doubly-additive diagram, as element vectors
            initial   (element-vectors-of srm
                                          (positions-array start nodes)
                                          (count objects))
            projected (layout-of-vectors lattice nodes model initial)
            ;; step 3: force-directed refinement of the element vectors
            energy    (cached-energy-and-force model)
            optimized (try
                        (let [[point _] (minimize-with-gradient
                                         (fn [^doubles p] (first (energy p)))
                                         (fn [^doubles p]
                                           (map - (seq ^doubles (second (energy p)))))
                                         (seq initial)
                                         {:iterations (:iterations parameters)})]
                          (layout-of-vectors lattice nodes model (double-array point)))
                        (catch Exception _ nil))]
        (cond
          (and optimized (respects-order? optimized)) optimized
          (respects-order? projected)                 projected
          :else                                       start)))))

;;;

nil
