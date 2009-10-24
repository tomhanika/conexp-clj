(ns conexp.graphics.base
  (:import [javax.swing JFrame JButton JPanel JLabel]
	   [java.awt Color Dimension Font BorderLayout]
	   [java.awt.event ActionListener]
	   [no.geosoft.cc.geometry Geometry Matrix4x4]
	   [no.geosoft.cc.graphics GWindow GScene GObject ZoomInteraction GSegment GStyle
	                           GText GPosition GScene GInteraction]))

;;; nodes and connections

(defn node? [thing]
  (and (instance? GObject thing)
       (= (:type @(.getUserData thing)) :node)))

(defn connection? [thing]
  (and (instance? GObject thing)
       (= (:type @(.getUserData thing)) :connection)))

(defn position [node]
  (:position @(.getUserData node)))

(defn upper-neighbors [node]
  (:upper @(.getUserData node)))

(defn lower-neighbors [node]
  (:lower @(.getUserData node)))

(defn start-of-connection [conn]
  (let [segment (.getSegment conn)]
    [(aget (.getX segment) 0) (aget (.getY segment) 0)]))

(defn end-of-connection [conn]
  (let [segment (.getSegment conn)]
    [(aget (.getX segment) 1) (aget (.getY segment) 1)]))


;;; create and connect nodes

(defn add-node [scn x y]
  (let [object (GObject.)
	segment (GSegment.)
	style (GStyle.)]
    (.addSegment object segment)
    (.add scn object)
    (doto style
      (.setForegroundColor (Color.  6  6 86))
      (.setBackgroundColor (Color. 23 11 84)))
    (.setStyle segment style)
    (.setGeometryXy segment (Geometry/createCircle (double x) (double y) 10.0))
    (.setUserData object (ref {:type :node, :position [x y]}))
    object))

(defn connect-nodes [scn x y]
  (let [[x1 x2] (:position @(.getUserData x))
	[y1 y2] (:position @(.getUserData y))
	c (GObject.)
	line (GSegment.)]
    (.add scn c)
    (.addSegment c line)
    (.toBack c)
    (.setUserData c (ref {:type :connection}))
    (.setGeometry line (double x1) (double x2) (double y1) (double y2))
    (dosync
     (alter (.getUserData x) update-in [:upper] conj c)
     (alter (.getUserData y) update-in [:lower] conj c))))


;;; move nodes around

(defn height-of-lower-neighbors [node]
  (let [lowers (lower-neighbors node)]
    (if (not lowers)
      nil
      (reduce min (map (fn [conn] ((start-of-connection conn) 1)) lowers)))))

(defn height-of-upper-neighbors [node]
  (let [uppers (upper-neighbors node)]
    (if (not uppers)
      nil
      (reduce max (map (fn [node] ((end-of-connection node) 1)) uppers)))))

(defn move-node-by [node dx dy] ; race condition when move nodes too fast?
  (let [[x y] (position node)
	max-y (height-of-lower-neighbors node)
	min-y (height-of-upper-neighbors node)
	dy    (if max-y (min dy (- max-y y)) dy)
	dy    (if min-y (max dy (- min-y y)) dy)
	[new-x new-y] [(+ x dx) (+ y dy)]]
    (doseq [segment (.getSegments node)]
      (.translate segment dx dy))
    (doseq [c (upper-neighbors node)]
      (let [c (.getSegment c)
	    c1 (aget (.getX c) 0)
	    c2 (aget (.getY c) 0)
	    d1 (aget (.getX c) 1)
	    d2 (aget (.getY c) 1)]
	(.setGeometry c (+ c1 dx) (+ c2 dy) d1 d2)))
    (doseq [c (lower-neighbors node)]
      (let [c (.getSegment c)
	    c1 (aget (.getX c) 0)
	    c2 (aget (.getY c) 0)
	    d1 (aget (.getX c) 1)
	    d2 (aget (.getY c) 1)]
	(.setGeometry c c1 c2 (+ d1 dx) (+ d2 dy))))
    (dosync
     (alter (.getUserData node) assoc :position [new-x new-y]))
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
				   (move-node-to @interaction-obj x y)
				   (.refresh scn))
	   GWindow/BUTTON1_UP    (reset! interaction-obj nil)
	   nil)))))


;;; some testing

(defn- show-some-picture []
  (let [wnd (GWindow.)
	scn (GScene. wnd)
	frm (JFrame.)]
;    (.setWorldExtent scn 0.0 0.0 100.0 100.0)
    (.startInteraction wnd (move-interaction))
    (.setLayout frm (BorderLayout.))
    (-> frm .getContentPane (.add (.getCanvas wnd) BorderLayout/CENTER))
    (.setSize frm 300 300)
    (let [x (add-node scn 100 100)
	  y (add-node scn 50 50)
	  z (add-node scn 75 23)]
      (connect-nodes scn x y)
      (connect-nodes scn x z)
      (connect-nodes scn y z))
    (.setVisible frm true)))
