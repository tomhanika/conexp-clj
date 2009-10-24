(ns conexp.graphics.trials
  (:use )
  (:import [javax.swing JFrame JButton JPanel JLabel]
	   [java.awt Color Dimension Font BorderLayout]
	   [java.awt.event ActionListener]
	   [no.geosoft.cc.geometry Geometry Matrix4x4]
	   [no.geosoft.cc.graphics GWindow GScene GObject ZoomInteraction GSegment GStyle
	                           GText GPosition GScene GInteraction]))

;;; Demo 1a

(defn hello-world-object []
  (let [star (GSegment.)
	star-style (GStyle.)
	text (GText. "Hello World" GPosition/MIDDLE)
	text-style (GStyle.)
	object (proxy [GObject] []
		 (draw []
		   (.setGeometryXy star (Geometry/createStar 0.5 0.5 0.4 0.2 15))))]
    (doto star-style
      (.setForegroundColor (Color. 255 0 0))
      (.setBackgroundColor (Color. 255 255 0))
      (.setLineWidth 3))
    (doto text-style
      (.setForegroundColor (Color. 100 100 150))
      (.setBackgroundColor nil)
      (.setFont (Font. "Dialog" Font/BOLD 48)))
    (doto text
      (.setStyle text-style))
    (doto star
      (.setText text))
    (doto object
      (.setStyle star-style)
      (.addSegment star))))

(defn demo-1a []
  (let [main-frame (JFrame. "G Graphics Library -- Demo 1a")
	window (GWindow. (Color. 210 235 255))
	scene (GScene. window)
	object (hello-world-object)]
    (doto scene
      (.setWorldExtent 0.0 0.0 1.0 1.0)
      (.add object))
    (doto main-frame
      (.. getContentPane (add (.getCanvas window)))
      (.pack)
      (.setSize (Dimension. 500 500))
      (.setVisible true))
    (doto window
      (.startInteraction (ZoomInteraction. scene)))))


;;; Demo 2

(defn test-object [name angle color]
  (let [style         (GStyle.)

	large-circle  (GSegment.)
	small-circle  (GSegment.)
	arm           (GSegment.)

	text          (GText. name (bit-or GPosition/MIDDLE GPosition/CENTER))
	text-style    (GStyle.)
	
	object        (proxy [GObject] []
			(draw []
			  (let [x0 (int (Math/round (.. this getScene getViewport getCenterX)))
				y0 (int (Math/round (.. this getScene getViewport getCenterY)))
				width (int (Math/round (.. this getScene getViewport getWidth)))
				height (int (Math/round (.. this getScene getViewport getHeight)))

				length (- (Math/min width height) 20)
				qlength (int (Math/round (/ length 3.0)))

				large-circle-coords (Geometry/createCircle
						     x0 y0 (int (Math/round (* length 0.2))))
				small-circle-coords (Geometry/createCircle
						     x0 y0 (int (Math/round (* length 0.1))))
				arm-coords (into-array Integer/TYPE
						       [ (- x0 qlength), (- y0 5),
							 (+ x0 qlength), (- y0 5),
							 (+ x0 qlength), (+ y0 5),
							 (- x0 qlength), (+ y0 5) ])
				matrix (Matrix4x4.)]
			    (doto matrix
			      (.translate (- qlength) 0 0)
			      (.transformXyPoints small-circle-coords)
			      (.setIdentity)
			      (.translate qlength 0 0)
			      (.transformXyPoints large-circle-coords)
			      (.setIdentity)
			      (.translate (- x0) (- y0))
			      (.rotateZ angle)
			      (.translate x0 y0)
			      (.transformXyPoints large-circle-coords)
			      (.transformXyPoints small-circle-coords)
			      (.transformXyPoints arm-coords))
			    (.setGeometry large-circle large-circle-coords)
			    (.setGeometry small-circle small-circle-coords)
			    (.setGeometry arm arm-coords))))]
    (doto style
      (.setBackgroundColor color)
      (.setLineStyle GStyle/LINESTYLE_INVISIBLE))
    (doto text-style
      (.setForegroundColor (Color. 100 100 100))
      (.setBackgroundColor nil)
      (.setFont (Font. "Dialog" Font/BOLD 36)))
    (.setStyle text text-style)
    (.setText large-circle text)
    (doto object
      (.setStyle style)
      (.addSegment large-circle)
      (.addSegment small-circle)
      (.addSegment arm))))

(defn demo-2 []
  (let [main-frame       (JFrame.)
	top-level        (JPanel.)
	button-panel     (JPanel.)

	front-button     (JButton. "Front")
	back-button      (JButton. "Back")
	forward-button   (JButton. "Forward")
	backward-button  (JButton. "Backward")

	window           (GWindow.)
	scene            (GScene. window)

	color            (ref nil)
	interaction-obj  (ref nil)

	action-listener  (proxy [ActionListener] []
			   (actionPerformed [evt]
			     (when @interaction-obj
			       (let [source (.getSource evt)
				     parent (.getParent @interaction-obj)]
				 (condp = source
				     front-button (.reposition parent @interaction-obj (.front parent))
				     back-button  (.reposition parent @interaction-obj (.back parent))
				     forward-button (.reposition parent @interaction-obj (.forward parent))
				     backward-button (.reposition parent @interaction-obj (.backward parent)))
				 (.setEnabled front-button (not (.isInFront @interaction-obj)))
				 (.setEnabled forward-button (not (.isInFront @interaction-obj)))
				 (.setEnabled back-button (not (.isInBack @interaction-obj)))
				 (.setEnabled backward-button (not (.isInBack @interaction-obj)))
				 (.refresh @interaction-obj)))))

	interaction      (proxy [GInteraction] []
			   (event [scn evt x y]
     			     (when (= evt GWindow/BUTTON1_UP)
			       (let [object (.find scn x y)]
				 (when object
				   (if @interaction-obj
				     (-> @interaction-obj .getStyle (.setBackgroundColor @color)))
				   (do
				     (dosync
				      (ref-set interaction-obj object)
				      (ref-set color (-> @interaction-obj .getStyle .getBackgroundColor)))
				     (-> @interaction-obj .getStyle (.setBackgroundColor (Color. 255 255 255)))
				     (.refresh object)))))))]
    (doto button-panel
      (.add (JLabel. "Click on object to select"))
      (.add front-button)
      (.add back-button)
      (.add backward-button)
      (.add forward-button))
    (doseq [btn [front-button back-button forward-button backward-button]]
      (.addActionListener btn action-listener))
    (doto top-level
      (.setLayout (BorderLayout.))
      (.add button-panel BorderLayout/NORTH)
      (.add (.getCanvas window) BorderLayout/CENTER))
    (doto main-frame
      (.setDefaultCloseOperation JFrame/DISPOSE_ON_CLOSE)
      (.. getContentPane (add top-level))
      (.pack)
      (.setSize 500 500)
      (.setVisible true))
    (let [n 8]
      (doseq [i (range 1 (inc n))]
	(let [color (Color/getHSBColor (float (/ i n)) 0.5 1.0)]
	  (.add scene (test-object (str i) (/ (* 2.0 i Math/PI) n) color)))))
    (doto window
      (.startInteraction interaction))))


;;; simple thing which can be moved around

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
     (alter (.getUserData x) assoc :upper (conj (:upper @(.getUserData x)) c))
     (alter (.getUserData y) assoc :lower (conj (:lower @(.getUserData y)) c)))))

(defn node? [thing]
  (= (:type @(.getUserData thing)) :node))

(defn connection? [thing]
  (= (:type @(.getUserData thing)) :connection))

(defn move-interaction []
  (let [interaction-obj (ref nil)
	x0 (ref 0)
	y0 (ref 0)]
    (proxy [GInteraction] []
      (event [scn evt x y]
	(condp = evt
	   GWindow/BUTTON1_DOWN
	   (let [thing (.find scn x y)]
	     (when (node? thing)
	       (dosync
		(ref-set interaction-obj thing)
		(ref-set x0 x)
		(ref-set y0 y))))
	   GWindow/BUTTON1_DRAG
	   (let [dx (- x @x0)
		 dy (- y @y0)]
	     (when @interaction-obj
	       (.translate (.getSegment @interaction-obj) dx dy)
	       (doseq [c (:upper @(.getUserData @interaction-obj))]
		 (let [c (.getSegment c)
		       c1 (aget (.getX c) 0)
		       c2 (aget (.getY c) 0)
		       d1 (aget (.getX c) 1)
		       d2 (aget (.getY c) 1)]
		   (.setGeometry c (+ c1 dx) (+ c2 dy) d1 d2)))
	       (doseq [c (:lower @(.getUserData @interaction-obj))]
		 (let [c (.getSegment c)
		       c1 (aget (.getX c) 0)
		       c2 (aget (.getY c) 0)
		       d1 (aget (.getX c) 1)
		       d2 (aget (.getY c) 1)]
		   (.setGeometry c c1 c2 (+ d1 dx) (+ d2 dy))))
	       (.refresh scn))
	     (dosync
	      (ref-set x0 x)
	      (ref-set y0 y)
	      (alter (.getUserData @interaction-obj) assoc :position [x y])))
	   GWindow/BUTTON1_UP
	   (dosync
	    (ref-set interaction-obj nil))
	   nil)))))

(defn show-some-picture []
  (let [wnd (GWindow.)
	scn (GScene. wnd)
	frm (JFrame.)]
    (.setWorldExtent scn 0.0 0.0 200.0 200.0)
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
