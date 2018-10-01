;; Copyright ⓒ the conexp-clj developers; all rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.layouts.freese
  (:use conexp.base
        [conexp.fca.lattices :only (base-set lattice-upper-neighbours)]
        [conexp.layouts.util :only (edges)]
        [conexp.layouts.base :only (make-layout-nc)])
  (:import [org.latdraw.diagram Diagram Vertex]))

;;;

(defn interactive-freese-layout
  "Returns the interactive Freese Layout of the given lattice. This is
  a function of one argument (the projection angle) returning the
  corresponding layout."
  [lattice]
  (let [nodes (seq (base-set lattice)),
        ucs   (map #(or (seq (lattice-upper-neighbours lattice %)) [])
                   nodes),
        ^Diagram
        diag  (Diagram. "" nodes ucs),
        edges (edges lattice)]
    (.improve diag)
    (fn [angle]
      (.project2d diag ^double angle)
      (make-layout-nc lattice
                      (reduce! (fn [map, ^Vertex vertex]
                                 (assoc! map
                                         (.. vertex getUnderlyingElem getUnderlyingObject)
                                         [(.getProjectedX vertex) (.getProjectedY vertex)]))
                               {}
                               (.getVertices diag))
                      edges))))

(defn freese-layout
  "Returns the Freese Layout of the given lattice."
  [lattice]
  ((interactive-freese-layout lattice) 0.0))

;;;

nil
