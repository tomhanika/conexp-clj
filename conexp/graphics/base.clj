(ns conexp.graphics.base
  (:use [clojure.contrib.seq-utils :only (flatten)])
  (:import [javax.swing JFrame JButton JPanel JLabel]
	   [java.awt Color Dimension Font BorderLayout]
	   [java.awt.event ActionListener]
	   [no.geosoft.cc.geometry Geometry Matrix4x4]
	   [no.geosoft.cc.graphics GWindow GScene GObject ZoomInteraction GSegment GStyle
	                           GText GPosition GScene GInteraction]))

(set! *warn-on-reflection* true)

;;; technical helpers

(defn device-to-world [#^GScene scn x y]
  (let [trf (.getTransformer scn)
	ptn (.deviceToWorld trf x y)]
    [(aget ptn 0) (aget ptn 1)]))

(defn world-to-device [#^GScene scn x y]
  (let [trf (.getTransformer scn)
	ptn (.worldToDevice trf x y)]
    [(aget ptn 0) (aget ptn 1)]))

(defn origin [scn]
  (world-to-device scn 0 0))

    
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
			(.setGeometryXy segment (Geometry/createCircle (double x) (double y) 10.0)))))
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
			      (.setLineWidth 3.0))
			    style))

(defn connect-nodes
  ([#^GScene scn #^GObject x #^GObject y]
     (connect-nodes scn x y (str (.getName x) " -> " (.getName y))))
  ([#^GScene scn #^GObject x #^GObject y name]
     (let [line (GSegment.)
	   c       (proxy [GObject] []
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
  (let [[x y] (position node)

	; make sure nodes don't go too far
	max-y (height-of-upper-neighbors node)
	min-y (height-of-lower-neighbors node)
	dy    (if max-y (min dy (- max-y y)) dy)
	dy    (if min-y (max dy (- min-y y)) dy)

	[new-x new-y] [(+ x dx) (+ y dy)]]

    ; move node on the device
    (let [[dx-1 dy-1] (world-to-device (.getScene node) dx dy)
	  [zx zy]     (world-to-device (.getScene node) 0 0)
	  device-dx   (- dx-1 zx)
	  device-dy   (- dy-1 zy)]
      (doseq [#^GSegment segment (.getSegments node)]
	(.translate segment device-dx device-dy)))  ;; noch nicht richtig?

    ; update self position
    (dosync
     (alter (.getUserData node) assoc :position [new-x new-y]))

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
				     (reset! interaction-obj thing)))
	   GWindow/BUTTON1_DRAG  (when @interaction-obj
				   (let [[x y] (device-to-world scn x y)]
				     (move-node-to @interaction-obj x y)
				     (.refresh scn)))
	   GWindow/BUTTON1_UP    (reset! interaction-obj nil)
	   nil)))))


;;; draw nodes with coordinates and connections on a scene

(defn draw-nodes-with-connections [scn node-coordinate-map node-connections]
  (let [node-map (apply hash-map
			(flatten (map (fn [[node [x y]]]
					[node
					 (add-node scn x y (str node))])
				      node-coordinate-map)))]
    (doseq [[node-1 node-2] node-connections]
      (connect-nodes scn (node-map node-1) (node-map node-2)))
    node-map))


;;; some testing

(defn- show-some-picture []
  (let [wnd (GWindow.)
	scn (GScene. wnd)
	frm (JFrame. "Main Frame")]
    (doto scn
      (.setWorldExtent 0.0 0.0 100.0 100.0)
      (.shouldZoomOnResize false)
      (.shouldWorldExtentFitViewport false)
      (draw-nodes-with-connections {:x [100 100], :y [50 50], :z [0 0]}
				   [[:z :y], [:y :x]]))
    (doto frm
      (.setLayout (BorderLayout.))
      (.. getContentPane (add (.getCanvas wnd) BorderLayout/CENTER))
      (.pack)
      (.setSize (Dimension. 500 500))
      (.setVisible true))
    (doto wnd
      (.startInteraction (move-interaction)))
    scn))
