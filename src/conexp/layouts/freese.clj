;; Copyright (c) Daniel Borchmann. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.layouts.freese
  (:use [conexp.fca.lattices :only (base-set lattice-upper-neighbours)]
        [conexp.layouts.util :only (edges)]
        [conexp.layouts.base :only (make-layout)])
  (:import [org.latdraw.diagram Diagram Vertex]))

;;;

(defn freese-layout
  "Returns the Freese Layout of lattice, with the given angle for
  projection. If not given, the angle defaults to 0."
  ([lattice]
     (freese-layout lattice 0.0))
  ([lattice angle]
     (let [nodes (seq (base-set lattice)),
           ucs   (map #(or (seq (lattice-upper-neighbours lattice %)) [])
                      nodes),
           ^Diagram
           diag  (Diagram. "" nodes ucs)]
       (.improve diag)
       (.project2d diag ^double angle)
       (make-layout
        (into {} (for [^Vertex vertex (.getVertices diag)]
                   [(.getUnderlyingObject (.getUnderlyingElem vertex)),
                    [(.getProjectedX vertex) (.getProjectedY vertex)]]))
        (edges lattice)))))

;;;

nil
