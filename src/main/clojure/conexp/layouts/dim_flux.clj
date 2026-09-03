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
            [conexp.fca.lattices :refer [concept-lattice
                                         lattice-inf-irreducibles
                                         lattice-sup-irreducibles
                                         lattice-upper-neighbours]]
            [conexp.layouts.base :as lay]
            [conexp.layouts.dim-draw :refer [dim-draw-layout]]
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
  energies."
  {:w-rep          50.0
   :w-att           1.0
   :w-grav         30.0
   :iterations   1000
   :min-distance    1.0e-3})

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
  node and a non-incident edge, which the repulsion maximizes."
  [model ^doubles pos]
  (let [nc               (long (:nc model))
        ne               (long (:ne model))
        ng               (long (:ng model))
        ^booleans member (:member model)
        edges            (:edges model)
        min-d            (double (:min-distance model))
        force            (double-array (* 2 ne))
        energy           (double-array 1)]
    (dotimes [c nc]
      (let [wx (aget pos (* 2 c))
            wy (aget pos (inc (* 2 c)))]
        (doseq [[a b] edges
                :when (and (not= c (long a)) (not= c (long b)))]
          (let [v1   (long a)
                v2   (long b)
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
                ;; +1 if the node lies left of the edge, -1 otherwise, so that
                ;; the force always pushes it away
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
            (dotimes [e ne]
              (let [object? (< e ng)
                    mv      (aget member (+ (* c ne) e))
                    m1      (aget member (+ (* v1 ne) e))
                    m2      (aget member (+ (* v2 ne) e))
                    ;; the eight ways an element can be distributed over the node
                    ;; and the two ends of the edge, numbered as in the paper
                    k       (inc (bit-or (if mv 4 0) (if m1 2 0) (if m2 1 0)))
                    ;; k = 1 and k = 8 leave the conflict distance untouched, the
                    ;; other excluded cases contradict the order relation
                    skip?   (or (= k 1) (= k 8)
                                (if object?
                                  (or (= k 3) (= k 7))
                                  (or (= k 2) (= k 6))))
                    factor  (double
                             (if skip?
                               0.0
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
                                       :else                        0.0))))]
                (when-not (zero? factor)
                  (aset force (* 2 e)
                        (+ (aget force (* 2 e)) (* scale factor ux)))
                  (aset force (inc (* 2 e))
                        (+ (aget force (inc (* 2 e))) (* scale factor uy))))))))))
    [(aget energy 0) force]))

(defn- attractive-energy-and-force
  "The attractive energy `E_att = sum_{v1v2} |f|^2`, taken over all edges, which
  keeps the diagram from falling apart, together with the force it exerts on the
  element vectors."
  [model ^doubles pos]
  (let [ne               (long (:ne model))
        ng               (long (:ng model))
        ^booleans member (:member model)
        force            (double-array (* 2 ne))
        energy           (double-array 1)]
    (doseq [[a b] (:edges model)]
      (let [v1  (long a)
            v2  (long b)
            dx  (- (aget pos (* 2 v2)) (aget pos (* 2 v1)))
            dy  (- (aget pos (inc (* 2 v2))) (aget pos (inc (* 2 v1))))]
        (aset energy 0 (+ (aget energy 0) (* dx dx) (* dy dy)))
        (dotimes [e ne]
          (let [object? (< e ng)
                m1      (aget member (+ (* v1 ne) e))
                m2      (aget member (+ (* v2 ne) e))]
            ;; only elements separating the two ends of an edge can shorten it:
            ;; objects of the upper end pull it down, attributes of the lower end
            ;; pull it up
            (when (if object? (and m2 (not m1)) (and m1 (not m2)))
              (let [sign (double (if object? -1.0 1.0))]
                (aset force (* 2 e)
                      (+ (aget force (* 2 e)) (* 2.0 sign dx)))
                (aset force (inc (* 2 e))
                      (+ (aget force (inc (* 2 e))) (* 2.0 sign dy)))))))))
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
  returned instead, and should that one be invalid as well, the DimDraw diagram
  is.

  When the first argument after the lattice is a map, it overrides
  `default-parameters`; all remaining arguments are passed on to DimDraw.

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
        ;; step 1: the realizer-embedded diagram of DimDraw, scaled to the
        ;; canonical aspect ratio used in the paper
        dim-draw                   (let [layout (apply dim-draw-layout lattice dim-draw-args)]
                                     (lay/update-positions
                                      layout
                                      (into {} (for [[c [x y]] (lay/positions layout)]
                                                 [c [(* (Math/sqrt 2.0) (double x))
                                                     (/ (double y) (Math/sqrt 2.0))]]))))]
    (if (empty? (concat objects attributes))
      dim-draw
      (let [model     (make-model lattice nodes objects attributes parameters)
            srm       (set-representation-matrix lattice nodes objects attributes)
            ;; step 2: the closest doubly-additive diagram, as element vectors
            initial   (element-vectors-of srm
                                          (positions-array dim-draw nodes)
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
          :else                                       dim-draw)))))

;;;

nil
