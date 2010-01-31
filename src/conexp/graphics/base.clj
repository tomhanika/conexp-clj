(ns conexp.graphics.base
  (:use [conexp.util :only (update-ns-meta!)]
	[conexp.base :only (defvar-)]
	[conexp.layout.util :only (edges-of-points)]
	[clojure.contrib.ns-utils :only (immigrate)])
  (:import [javax.swing JFrame JButton JPanel JLabel]
	   [java.awt Dimension BorderLayout Color]
	   [no.geosoft.cc.graphics GWindow GScene GStyle]))

(immigrate 'conexp.graphics.util
	   'conexp.graphics.nodes-and-connections)

(update-ns-meta! conexp.graphics.base
  :doc "Basic namespace for drawing lattice.")


;;; get layout and diagram from scene

(defn get-diagram-from-scene
  "Returns nodes and lines of a scene."
  [scene]
  (seq (.getChildren scene)))

(defn get-layout-from-scene
  "Returns layout from a scene."
  [scn]
  (let [[nodes connections] (loop [things (get-diagram-from-scene scn),
				   nodes [],
				   connections []]
			      (if (empty? things)
				[nodes connections]
				(let [thing (first things)]
				  (cond
				   (node? thing)
				   (recur (rest things) (conj nodes thing) connections),
				   (connection? thing)
				   (recur (rest things) nodes (conj connections thing)),
				   :else
				   (throw (IllegalStateException. "Invalid item in lattice diagram."))))))]
    [(reduce (fn [hash node]
	       (assoc hash (get-name node) (position node)))
	     {}
	     nodes),
     (map #(vector (get-name (lower-node %))
		   (get-name (upper-node %)))
	  connections)]))

(defn set-layout-of-scene
  "Sets given layout as current layout of scene."
  [#^GScene scene, layout]
  (let [[x_min y_min x_max y_max] (edges-of-points (vals (first layout)))]
    (doto scene
      (.removeAll)
      (.setWorldExtent (double (- x_min (* 2 *default-node-radius*)))
		       (double (- y_min (* 2 *default-node-radius*)))
		       (double (- x_max x_min (* -4 *default-node-radius*)))
		       (double (- y_max y_min (* -4 *default-node-radius*))))
      (add-nodes-with-connections (first layout) (second layout))
      (.unzoom)))) ; ? the only way I kown to call updateRegion...


;;; draw nodes with coordinates and connections on a scene

(defvar- *default-scene-style* (doto (GStyle.)
				 (.setBackgroundColor Color/WHITE)
				 (.setAntialiased true))
  "Default GScene style.")

(defn draw-on-scene
  "Draws given layout on a GScene and returns it."
  [[points-to-coordinates point-connections]]
  (let [wnd (GWindow. Color/WHITE)
	scn (GScene. wnd)]
    (doto scn
      (set-layout-of-scene [points-to-coordinates point-connections])
      (.shouldZoomOnResize true)
      (.shouldWorldExtentFitViewport false)
      (.setStyle *default-scene-style*))
    (doto wnd
      (.startInteraction (move-interaction)))
    scn))


;;; node and line iterators

(defmacro donodes
  "Do whatever with every node on the scene."
  [[node scene] & body]
  `(doseq [~node (filter node? (get-diagram-from-scene ~scene))]
     ~@body))

(defmacro dolines
  "Do whatever with every connection on the scene."
  [[line scene] & body]
  `(doseq [~line (filter connection? (get-diagram-from-scene ~scene))]
     ~@body))



;;;

nil
