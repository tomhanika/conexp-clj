(ns conexp.graphics.base
  (:import [javax.swing JFrame JButton JPanel JLabel]
	   [java.awt Color Dimension Font BorderLayout]
	   [java.awt.event ActionListener]
	   [no.geosoft.cc.geometry Geometry Matrix4x4]
	   [no.geosoft.cc.graphics GWindow GScene GObject ZoomInteraction GSegment GStyle
	                           GText GPosition GScene GInteraction]))

;;; technical helpers

(defn device-to-world [scn x y]
  (let [trf (.getTransformer scn)
	ptn (.deviceToWorld trf x y)]
    [(aget ptn 0) (aget ptn 1)]))

(defn world-to-device [scn x y]
  (let [trf (.getTransformer scn)
	ptn (.worldToDevice trf x y)]
    [(aget ptn 0) (aget ptn 1)]))

(defn origin [scn]
  (world-to-device scn 0 0))

    
;;; nodes and connections

(defn node? [thing]
  (and (instance? GObject thing)
       (= (:type @(.getUserData thing)) :node)))

(defn connection? [thing]
  (and (instance? GObject thing)
       (= (:type @(.getUserData thing)) :connection)))

(defn position [node]
  (:position @(.getUserData node)))

(defn lower-node [conn]
  (:lower @(.getUserData conn)))

(defn upper-node [conn]
  (:upper @(.getUserData conn)))

(defn upper-connections [node]
  (:upper @(.getUserData node)))

(defn lower-connections [node]
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

(defn add-node [scn x y]
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
			  :position [x y]}))
      (.setName (str [x y])))
    (doto scn
      (.add object))
    object))

(def *default-line-style* (let [style (GStyle.)]
			    (doto style
			      (.setLineWidth 3.0))
			    style))

(defn connect-nodes [scn x y]
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
      (.setName (str (.getName x) " -> " (.getName y))))
    (dosync
     (alter (.getUserData x) update-in [:upper] conj c)
     (alter (.getUserData y) update-in [:lower] conj c))))


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

(defn move-node-by [node dx dy] ; race condition when move nodes too fast?
  (let [[x y] (position node)

	; make sure nodes don't go too far
	max-y (height-of-upper-neighbors node)
	min-y (height-of-lower-neighbors node)
	dy    (if max-y (min dy (- max-y y)) dy)
	dy    (if min-y (max dy (- min-y y)) dy)

	[new-x new-y] [(+ x dx) (+ y dy)]]

    ; move nodes on the device
    (let [[dx-1 dy-1] (world-to-device scn dx dy)
	  [zx zy]     (world-to-device scn 0 0)
	  device-dx   (- dx-1 zx)
	  device-dy   (- dy-1 zy)]
      (doseq [segment (.getSegments node)]
	(.translate segment device-dx device-dy)))

    ; update self position
    (dosync
     (alter (.getUserData node) assoc :position [new-x new-y]))

    ; update connections to upper neighbors
    (doseq [c (upper-connections node)]
      (.redraw c))

    ; update connections to lower neighbors
    (doseq [c (lower-connections node)]
      (.redraw c))

    ; done
    [new-x new-y]))

(defn move-node-to [node x y]
  (let [[current-x current-y] (position node)]
    (move-node-by node (- x current-x) (- y current-y))))

(defn move-interaction []
  (let [interaction-obj (atom nil)]
    (proxy [GInteraction] []
      (event [scn evt x y]
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


;;; some testing

(defn- show-some-picture []
  (let [wnd (GWindow.)
	scn (GScene. wnd)
	frm (JFrame. "Main Frame")]
    (doto scn
      (.setWorldExtent 0.0 0.0 100.0 100.0)
      (.shouldZoomOnResize false)
      (.shouldWorldExtentFitViewport false))
    (let [x (add-node scn 100 100)
	  y (add-node scn 50 50)
	  z (add-node scn 0 0)]
      (connect-nodes scn z y)
      (connect-nodes scn y x))
    (doto frm
      (.setLayout (BorderLayout.))
      (.. getContentPane (add (.getCanvas wnd) BorderLayout/CENTER))
      (.pack)
      (.setSize (Dimension. 500 500))
      (.setVisible true))
    (doto wnd
      (.startInteraction (move-interaction)))
    scn))
