;; Copyright ⓒ the conexp-clj developers; all rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.layouts.force
  "Force layout as described by C. Zschalig."
  (:use [conexp.base :exclude (sum)]
        conexp.fca.lattices
        conexp.layouts.base
        conexp.layouts.common
        conexp.math.util
        conexp.math.optimize))

;; Helpers

(defn- square ^double [^double x]
  (* x x))

(defn- line-length-squared
  "Returns the square of the length of the line between [x_1, y_1] and [x_2, y_2]."
  [[x_1, y_1], [x_2, y_2]]
  (+ (square (- x_1 x_2))
     (square (- y_1 y_2))))

(defn- distance
  "Returns distance of two points."
  [[x_1 y_1] [x_2 y_2]]
  (with-doubles [x_1 y_1 x_2 y_2]
    (Math/sqrt (+ (square (- x_1 x_2)) (square (- y_1 y_2))))))

(defmacro- sum
  "Sums up all values of bindings obtained with expr. See doseq."
  [bindings expr]
  `(let [sum# (atom (double 0))]
     (doseq ~bindings
       (swap! sum# + (double ~expr)))
     (double @sum#)))


;; Repulsive Energy

(defn- node-line-distance
  "Returns the distance from node [x,y] to the line between [x_1, y_1]
  and [x_2, y_2]."
  [[x, y] [x_1, y_1] [x_2, y_2]]
  (with-doubles [x, y, x_1, y_1, x_2, y_2]
    (if (and (= x_1 x_2) (= y_1 y_2))
      (distance [x, y] [x_1, y_1])
      (let [;; position of projection of [x y] onto the line, single coordinate
            r (/ (+ (* (- x x_1)
                       (- x_2 x_1))
                    (* (- y y_1)
                       (- y_2 y_1)))
                 (+ (square (- x_2 x_1))
                    (square (- y_2 y_1)))),
            r (min (max r 0) 1)]
        (distance [x, y] [(+ x_1 (* r (- x_2 x_1))),
                          (+ y_1 (* r (- y_2 y_1)))])))))

(defn- repulsive-energy
  "Computes the repulsive energy of the given layout."
  [layout]
  (let [node-positions   (positions layout),
        node-connections (connections layout)]
    (try
     (let [edges (map (fn [[x y]]
                        [x y (get node-positions x) (get node-positions y)])
                      node-connections)]
       (sum [[v pos-v] node-positions,
             [x y pos-x pos-y] edges,
             :when (not (#{x y} v))]
            (/ (node-line-distance pos-v pos-x pos-y))))
     (catch Exception e
       Double/MAX_VALUE))))


;; Attractive Energy

(defn- attractive-energy
  "Computes the attractive energy of the given layout."
  [layout]
  (let [node-positions (positions layout),
        node-connections (connections layout)]
    (sum [[x y] node-connections]
         (line-length-squared (node-positions x)
                              (node-positions y)))))


;; Gravitative Energy

(defn- phi
  "Returns the angle between an inf-irreducible node [x_1,y_1]
  and its upper neighbor [x_2, y_2]."
  [[x_1,y_1] [x_2,y_2]]
  (with-doubles [x_1, y_1, x_2, y_2]
    (let [result (Math/atan2 (- y_2 y_1) (- x_2 x_1))]
      (min (max 0 result) Math/PI))))

(defn- gravitative-energy
  "Returns the gravitative energy of the given layout."
  [layout]
  (let [node-positions   (positions layout),
        node-connections (connections layout),
        inf-irrs         (inf-irreducibles layout),
        upper-neighbours (upper-neighbours-of-inf-irreducibles layout),
        E_0              (double (- (/ Math/PI 2.0)))]
    (try
      (sum [n inf-irrs,
            :let [phi_n (double (phi (node-positions n)
                                     (node-positions (upper-neighbours n))))]]
           (* (if (<= phi_n (/ Math/PI 2.0))
                1
                -1)
              (+ phi_n
                 (/ (Math/tan phi_n))
                 E_0)))
     (catch Exception e
       Double/MAX_VALUE))))


;; Overall Energy

(def ^{:dynamic true} *repulsive-amount* 500.0)
(def ^{:dynamic true} *attractive-amount* 0.005)
(def ^{:dynamic true} *gravitative-amount* 100.0)

(defn layout-energy
  "Returns the overall energy of the given layout."
  [layout]
  (+ (* *repulsive-amount* (repulsive-energy layout))
     (* *attractive-amount* (attractive-energy layout))
     (* *gravitative-amount* (gravitative-energy layout))))

;;

(defn- energy-by-inf-irr-positions
  "Returns pair of energy function and function returning the n-th
  partial derivative when given index n."
  [layout seq-of-inf-irrs]
  (let [lattice  (lattice layout),
        top-pos  ((positions layout) (lattice-one lattice)),
        energy   (fn [point-coordinates]
                   (let [points            (partition 2 point-coordinates),
                         inf-irr-placement (apply hash-map
                                                  (interleave seq-of-inf-irrs
                                                              points))]
                     (layout-energy
                      (update-positions layout
                                        (placement-by-initials lattice top-pos inf-irr-placement)))))]
    energy))


;;; Force Layout

(defn force-layout
  "Improves given layout with force layout."
  ([layout]
     (force-layout layout nil))
  ([layout iterations]
     (let [;; compute lattice from layout and ensure proper starting layout
           lattice             (lattice layout),
           layout              (to-inf-additive-layout layout),

           ;; get positions of inf-irreducibles from layout as starting point
           inf-irrs            (inf-irreducibles layout),
           node-positions      (positions layout),
           inf-irr-points      (map node-positions inf-irrs),
           top-pos             (node-positions (lattice-one lattice)),

           ;; minimize layout energy with above placement as initial value
           energy              (energy-by-inf-irr-positions layout inf-irrs),
           [new-points, value] (minimize energy
                                         (apply concat inf-irr-points)
                                         {:iterations iterations}),

           ;; make hash
           point-hash          (apply hash-map (interleave inf-irrs
                                                           (partition 2 new-points)))]

       ;; final layout
       (layout-by-placement lattice top-pos point-hash))))

;;;

nil
