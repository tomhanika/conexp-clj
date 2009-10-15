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

(defn add-thing-to-move-around [owner x y]
  (let [segment (GSegment.)
	style (GStyle.)]
    (.addSegment owner segment)
    (doto style
      (.setForegroundColor (Color.  6  6 86))
      (.setBackgroundColor (Color. 23 11 84)))
    (.setStyle segment style)
    (.setGeometryXy segment (Geometry/createCircle (double x) (double y) 10.0))
    segment))

(defn move-interaction []
  (let [interaction-obj (ref nil)
	x0 (ref 0)
	y0 (ref 0)]
    (proxy [GInteraction] []
      (event [scn evt x y]
	(condp = evt
	   GWindow/BUTTON1_DOWN
	   (dosync
	     (ref-set interaction-obj (.findSegment scn x y))
	     (ref-set x0 x)
	     (ref-set y0 y))
	   GWindow/BUTTON1_DRAG
	   (let [dx (- x @x0)
		 dy (- y @y0)]
	     (when @interaction-obj
	       (.translate @interaction-obj dx dy)
	       (.refresh scn))
	     (dosync
	      (ref-set x0 x)
	      (ref-set y0 y)))
	   GWindow/BUTTON1_UP
	   (dosync
	    (ref-set interaction-obj nil))
	   nil)))))

(defn show-some-picture []
  (let [wnd (GWindow.)
	scn (GScene. wnd)
	frm (JFrame.)]
    (.startInteraction wnd (move-interaction))
    (.setLayout frm (BorderLayout.))
    (-> frm .getContentPane (.add (.getCanvas wnd) BorderLayout/CENTER))
    (dotimes [_ 100]
      (add-thing-to-move-around scn (rand-int 300) (rand-int 300)))
    (.setSize frm 1000 1000)
    (.setVisible frm true)))