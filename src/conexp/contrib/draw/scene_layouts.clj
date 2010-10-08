;; Copyright (c) Daniel Borchmann. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.contrib.draw.scene-layouts
  (:use [conexp.base :only (ns-doc, illegal-argument, defvar-)]
	[conexp.layouts.base :only (make-layout, positions, connections,
                                                 nodes, update-positions, annotation)]
	[conexp.layouts.util :only (edges-of-points)]
	conexp.contrib.draw.nodes-and-connections
	conexp.contrib.draw.scenes)
  (:use [clojure.contrib.swing-utils :only (do-swing)])
  (:import [javax.swing JFrame JButton JPanel JLabel]
	   [java.awt Dimension BorderLayout Color]
	   [no.geosoft.cc.graphics GWindow GScene GStyle]))

(ns-doc
 "Basic namespace for drawing lattice.")


;;; get diagram from scene

(defn get-diagram-from-scene
  "Returns nodes and lines of a scene."
  [^GScene scene]
  (seq (.getChildren scene)))

;;; node and line iterators

(defmacro do-nodes
  "Do whatever with every node on the scene. Does not redraw the scene
  afterwards."
  [[node scene] & body]
  `(do-swing
     (doseq [~node (filter node? (get-diagram-from-scene ~scene))]
       ~@body)
     (redraw-scene ~scene)))

(defmacro do-lines
  "Do whatever with every connection on the scene. Does not redraw the
  scene afterwards."
  [[line scene] & body]
  `(do-swing
     (doseq [~line (filter connection? (get-diagram-from-scene ~scene))]
       ~@body)
     (redraw-scene ~scene)))

;;; manipulate layout of scene

(defn get-layout-from-scene
  "Returns layout from a scene."
  [scn]
  (update-positions (get-data-from-scene scn :layout)
		    (reduce (fn [hash node]
			      (assoc hash (get-name node) (position node)))
			    {}
			    (filter node? (get-diagram-from-scene scn)))))

(defn fit-scene-to-layout
  "Adjusts scene such that layout fits on it. Uses stored layout if
  none is given. Calls :image-changed hook."
  ([^GScene scene]
     (fit-scene-to-layout scene (get-layout-from-scene scene)))
  ([^GScene scene, layout]
     (let [[x_min y_min x_max y_max] (edges-of-points (vals (positions layout))),
           max-radius                (reduce #(max %1 (radius %2))
                                             0
                                             (filter node? (get-diagram-from-scene scene)))]
       (.setWorldExtent scene
                        (double (- x_min (* 2 max-radius)))
                        (double (- y_min (* 2 max-radius)))
                        (double (- x_max x_min (* -4 max-radius)))
                        (double (- y_max y_min (* -4 max-radius))))
       (.unzoom scene))
     (call-scene-hook scene :image-changed)))

(defn update-layout-of-scene
  "Updates layout according to new layout. The underlying lattice must
  not be changed."
  [^GScene scene layout]
  (do-swing
   (let [pos (positions layout)]
     (do-nodes [node scene]
       (let [[x y] (pos (get-name node))]
         (move-node-unchecked-to node x y))))))

(defn set-layout-of-scene
  "Sets given layout as current layout of scene."
  [^GScene scene layout]
  (do-swing
   (doto scene
     (.removeAll)
     (add-nodes-with-connections (positions layout) (connections layout) (annotation layout))
     (add-data-to-scene :layout layout))))


;;; draw nodes with coordinates and connections on a scene

(defn draw-on-scene
  "Draws given layout on a GScene and returns it."
  [layout]
  (let [^GWindow wnd (make-window),
	scn (make-scene wnd)]
    (doto scn
      (set-layout-of-scene layout)
      (fit-scene-to-layout layout))
    (doto wnd
      (.startInteraction (move-interaction scn)))
    scn))


;;;

nil
