(ns conexp.graphics.base
  (:use [conexp.util :only (update-ns-meta!)]
	[clojure.contrib.ns-utils :only (immigrate)])
  (:import [javax.swing JFrame JButton JPanel JLabel]
	   [java.awt Dimension BorderLayout]
	   [no.geosoft.cc.graphics GWindow GScene]))

(immigrate 'conexp.graphics.util
	   'conexp.graphics.nodes-and-connections)

(update-ns-meta! conexp.graphics.base
  :doc "Basic namespace for drawing lattice.")


;;; draw nodes with coordinates and connections on a scene

(defn draw-on-scene
  "Draws given layout on a GScene and returns it."
  [[x_min y_min] [x_max y_max] [points-to-coordinates point-connections]]
  (let [wnd (GWindow.)
	scn (GScene. wnd)]
    (doto scn
      (.setWorldExtent (double x_min) (double y_min) (double x_max) (double y_max))
      (.shouldZoomOnResize false)
      (.shouldWorldExtentFitViewport false)
      (add-nodes-with-connections points-to-coordinates point-connections))
    (doto wnd
      (.startInteraction (move-interaction)))
    scn))

(defn draw-on-canvas
  "Draws given layout on a canvas and returns it."
  [[x_min y_min] [x_max y_max] [points-to-coordinates point-connections]]
  (.. (draw-on-scene [x_min y_min] [x_max y_max] [points-to-coordinates point-connections])
      getWindow
      getCanvas))

(defn draw-in-frame
  "Draws given layout in a frame and shows it."
  [[x_min y_min] [x_max y_max] [points-to-coordinates point-connections]]
  (doto (JFrame. "conexp-clj lattice frame.")
    (.setLayout (BorderLayout.))
    (.. getContentPane
	(add (draw-on-canvas [x_min y_min] [x_max y_max]
			     [points-to-coordinates
			      point-connections])
	     BorderLayout/CENTER))
    (.pack)
    (.setSize (Dimension. (+ 50 (int (- x_max x_min)))
			  (+ 50 (int (- y_max y_min)))))
    (.setVisible true)))


;;; some testing

(defn- show-some-picture
  "Testdiagram for lattices."
  []
  (draw-in-frame [0.0 0.0] [100.0 100.0]
		 [{:x [100 100], :y [50 50], :z [0 0], :a [50 0]}
		  [[:z :y], [:y :x], [:a :y]]]))


;;;

nil
