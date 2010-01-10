(ns conexp.graphics.draw
  (:use [conexp.util :only (update-ns-meta!)]
	[conexp.layout :only (*standard-layout*)]
	[conexp.layout.util :only (scale-layout)]
	[conexp.graphics.nodes-and-connections :only (*default-node-radius*, move-interaction)]
	[conexp.graphics.base :only (draw-on-scene)])
  (:import [javax.swing JFrame JPanel JButton]
	   [java.awt Dimension BorderLayout GridLayout]
	   [java.awt.event ActionListener]
	   [no.geosoft.cc.graphics ZoomInteraction]))

(update-ns-meta! conexp.graphics.draw
  :doc "This namespace provides a lattice editor and a convenience function to draw lattices.")


;;;

(defn- change-node-radius
  "Install node radius changer."
  [scn buttons]
  nil)

(defn- change-layout
  "Install lattice layout changer."
  [scn buttons]
  nil)

(defn- toggle-zoom-move
  "Install zoom-move-toggler."
  [scn buttons]
  (let [button (JButton. "Zoom")]
    (.add buttons button)
    (.addActionListener button
			(proxy [ActionListener] []
			  (actionPerformed [evt]
			    (if (= "Zoom" (.getText button))
			      (do
				(.. scn getWindow (startInteraction (ZoomInteraction. scn)))
				(.setText button "Move"))
			      (do
				(.. scn getWindow (startInteraction (move-interaction)))
				(.setText button "Zoom"))))))))

(defn- toggle-labels
  "Install label-toggler."
  [scn buttons]
  nil)

(defmacro install-changers
  "Installs given methods to scene with buttons."
  [scene buttons & methods]
  `(do
     ~@(map (fn [method#] `(~method# ~scene ~buttons))
	    methods)))

(defn make-lattice-editor
  "Creates a lattice editor for lattice with initial layout."
  [lattice layout]
  (let [main-panel (JPanel. (BorderLayout.)),

	scn (draw-on-scene [0.0 0.0] [400.0 400.0]
			   (scale-layout [0.0 0.0] [400.0 400.0]
					 (layout lattice))),
	canvas (.. scn getWindow getCanvas),

	buttons (JPanel. (GridLayout. 0 1))]
    (install-changers scn buttons
      change-node-radius
      toggle-zoom-move
      toggle-labels
      change-layout)
    (doto main-panel
      (.add canvas BorderLayout/CENTER)
      (.add buttons BorderLayout/WEST))
    main-panel))


;;;

(defn draw-lattice
  "Draws given lattice with given layout on a canvas and returns
  it. Uses *standard-layout* if no layout is given."
  ([lattice]
     (draw-lattice lattice *standard-layout*))
  ([lattice layout]
     (doto (JFrame. "conexp-clj Lattice")
       (.add (make-lattice-editor lattice layout))
       (.setSize (Dimension. 200 200))
       (.setVisible true))))


;;;

nil
