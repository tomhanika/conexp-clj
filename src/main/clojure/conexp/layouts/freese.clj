;; Copyright ⓒ the conexp-clj developers; all rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.layouts.freese
  (:use conexp.base
        [conexp.math.algebra :only (base-set)]
        [conexp.fca.lattices :only (lattice-upper-neighbours)]
        [conexp.layouts.util :only (edges)]
        [conexp.layouts.base :only (make-layout-nc)])
  (:import [org.latdraw.diagram Diagram Vertex]))

;;;

(defn interactive-freese-layout
  "Returns the interactive Freese Layout of the given ordered set. This is
  a function of one argument (the projection angle) returning the
  corresponding layout."
  [poset]
  (let [nodes (seq (base-set poset)),
        ucs   (map #(or (seq (lattice-upper-neighbours poset %)) [])
                   nodes),
        ^Diagram
        diag  (Diagram. "" nodes ucs),
        edges (edges poset)]
    (.improve diag)
    (fn [angle]
      (.project2d diag ^double angle)
      (make-layout-nc poset
                      (reduce! (fn [map, ^Vertex vertex]
                                 (assoc! map
                                         (.. vertex getUnderlyingElem getUnderlyingObject)
                                         [(.getProjectedX vertex) (.getProjectedY vertex)]))
                               {}
                               (.getVertices diag))
                      edges))))

(defn freese-layout
  "Returns the Freese Layout of the given ordered set."
  [poset]
  ((interactive-freese-layout poset) 0.0))

;;;

nil
