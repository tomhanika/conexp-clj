;; Copyright (c) Daniel Borchmann. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.io.util.layout-svg
  "A small library for dumping layouts into the SVG format."
  (:use conexp.util.xml
        conexp.base
        conexp.layouts.base
        conexp.layouts.util)
  (:use clojure.java.io))

;;;

(declare positions->svg
         connections->svg)

(defn- layout->svg
  "Returns a vector representing the given layout in SVG format."
  [layout]
  (let [[x_min y_min x_max y_max] (enclosing-rectangle (vals (positions layout))),
        [x_min y_min x_max y_max] [0 0 100 (int (ceil (* 100 (/ (- y_max y_min)
                                                                (- x_max x_min)))))]
        layout                    (scale-layout [0 0] [x_max y_max]
                                                layout)]
    [:svg {:xmlns "http://www.w3.org/2000/svg",
           :version "1.2",
           :baseProfile "tiny",
           :viewBox (str (- x_min 10) " " (- y_min 10) " " (+ 20 x_max) " " (+ 20 y_max))}
     (connections->svg layout y_max)
     (positions->svg layout y_max)]))

(defn- positions->svg [layout y_max]
  (vec (cons :g
             (for [[_ [x y]] (positions layout)]
               [:circle {:cx (str x),
                         :cy (str (- y_max y)),
                         :r  5,
                         :fill "white",
                         :stroke "black"}
                ""]))))

(defn- connections->svg [layout y_max]
  (vec (cons :g
             (for [[a b] (connections layout)]
               (let [[x_a y_a] (get (positions layout) a),
                     [x_b y_b] (get (positions layout) b)]
                 [:line {:x1 x_a,
                         :y1 (- y_max y_a),
                         :x2 x_b,
                         :y2 (- y_max y_b),
                         :stroke "black"}
                  ""])))))

;;;

(defn- print-layout-in-svg [layout]
  (binding [*prxml-indent* 2]
    (prxml [:decl!])
    (prxml (layout->svg layout))))

(defn svg-layout-to-str
  "Returns a string representing the given layout in SVG format."
  [layout]
  (with-out-str
    (print-layout-in-svg layout)))

(defn svg-layout-to-file
  "Exports given layout to the given file in SVG format."
  [layout filename]
  (with-open [out (writer filename)]
    (binding [*out* out]
      (print-layout-in-svg layout))))

;;;

nil
