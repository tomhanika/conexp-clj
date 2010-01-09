(ns conexp.graphics.nodes-and-connections
  (:use conexp.graphics.util)
  (:import [java.awt Color]
	   [no.geosoft.cc.geometry Geometry]
	   [no.geosoft.cc.graphics GWindow GScene GObject GSegment GStyle GInteraction]))


;;; nodes and connections

(defn node? [thing]
  (and (instance? GObject thing)
       (= (:type @(.getUserData #^GObject thing)) :node)))

(defn connection? [thing]
  (and (instance? GObject thing)
       (= (:type @(.getUserData #^GObject thing)) :connection)))

(defn position [#^GObject node]
  (:position @(.getUserData node)))

(defn lower-node [#^GObject conn]
  (:lower @(.getUserData conn)))

(defn upper-node [#^GObject conn]
  (:upper @(.getUserData conn)))

(defn upper-connections [#^GObject node]
  (:upper @(.getUserData node)))

(defn lower-connections [#^GObject node]
  (:lower @(.getUserData node)))

(defn upper-neighbors [node]
  (map upper-node (upper-connections node)))

(defn lower-neighbors [node]
  (map lower-node (lower-connections node)))


;;; create and connect nodes

(def *default-node-style* (let [style (GStyle.)]
			    (doto style
			      (.setForegroundColor (Color.  6  6 86))
			      (.setBackgroundColor (Color. 23 11 84)))
			    style))

(defn add-node 
  ([#^GScene scn x y]
     (add-node scn x y (str [x y])))
  ([#^GScene scn x y name]
     (let [segment (GSegment.)
	   object (proxy [GObject] []
		    (draw []
		      (let [[x y] (position this)]
			(.setGeometryXy segment (Geometry/createCircle (double x) (double y) 5.0)))))
	   style (GStyle.)]
       (doto segment
	 (.setStyle *default-node-style*))
       (doto object
	 (.addSegment segment)
	 (.setUserData (ref {:type :node,
			     :position [(double x) (double y)]}))
	 (.setName name))
       (doto scn
	 (.add object))
       object)))

(def *default-line-style* (let [style (GStyle.)]
			    (doto style
			      (.setLineWidth 2.0))
			    style))

(defn connect-nodes
  ([#^GScene scn, #^GObject x, #^GObject y]
     (connect-nodes scn x y (str (.getName x) " -> " (.getName y))))
  ([#^GScene scn, #^GObject x, #^GObject y, name]
     (let [line (GSegment.)
	   c    (proxy [GObject] []
		  (draw []
		    (let [[x1 y1] (position (lower-node this))
			  [x2 y2] (position (upper-node this))]
		      (.setGeometry line (double x1) (double y1) (double x2) (double y2)))))]
       (doto scn
	 (.add c))
       (doto line
	 (.setStyle *default-line-style*))
       (doto c
	 (.addSegment line)
	 (.toBack)
	 (.setUserData (ref {:type :connection,
			     :lower x,
			     :upper y}))
	 (.setName name))
       (dosync
	(alter (.getUserData x) update-in [:upper] conj c)
	(alter (.getUserData y) update-in [:lower] conj c)))))


;;; move nodes around

(defn height-of-lower-neighbors [node]
  (let [lowers (lower-neighbors node)]
    (if (empty? lowers)
      nil
      (reduce min (map (fn [node]
			 ((position node) 1))
		       lowers)))))

(defn height-of-upper-neighbors [node]
  (let [uppers (upper-neighbors node)]
    (if (empty? uppers)
      nil
      (reduce max (map (fn [node] 
			 ((position node) 1))
		       uppers)))))

(defn move-node-by [#^GObject node dx dy] ; race condition when move nodes too fast?
  (let [[x y] (position node),

	; make sure nodes don't go too far
	max-y (height-of-upper-neighbors node),
	min-y (height-of-lower-neighbors node),
	dy    (if max-y (min dy (- max-y y)) dy),
	dy    (if min-y (max dy (- min-y y)) dy),

	[new-x new-y] [(+ x dx) (+ y dy)]]

    ; update self position
    (dosync
     (alter (.getUserData node) assoc :position [new-x new-y]))

    ; move node on the device
    (.redraw node)

    ; update connections to upper neighbors
    (doseq [#^GObject c (upper-connections node)]
      (.redraw c))

    ; update connections to lower neighbors
    (doseq [#^GObject c (lower-connections node)]
      (.redraw c))

    ; done
    [new-x new-y]))

(defn move-node-to [node x y]
  (let [[current-x current-y] (position node)]
    (move-node-by node (- x current-x) (- y current-y))))

(defn move-interaction []
  (let [interaction-obj (atom nil)]
    (proxy [GInteraction] []
      (event [#^GScene scn evt x y]
	(condp = evt
	   GWindow/BUTTON1_DOWN  (let [thing (.find scn x y)]
				   (when (node? thing)
				     (reset! interaction-obj thing))),
	   GWindow/BUTTON1_DRAG  (when @interaction-obj
				   (let [[x y] (device-to-world scn x y)]
				     (move-node-to @interaction-obj x y)
				     (.refresh scn))),
	   GWindow/BUTTON1_UP    (reset! interaction-obj nil)
	   nil)))))

(defn add-nodes-with-connections
  "Adds to scene scn nodes placed by node-coordinate-map and connected
  via pairs in the sequence node-connections."
  [scn node-coordinate-map node-connections]
  (let [node-map (apply hash-map
			(apply concat (map (fn [[node [x y]]]
					     [node
					      (add-node scn x y (str node))])
					   node-coordinate-map)))]
    (doseq [[node-1 node-2] node-connections]
      (connect-nodes scn (node-map node-1) (node-map node-2)))
    node-map))

;;;

nil
