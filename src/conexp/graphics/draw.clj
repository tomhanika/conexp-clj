(ns conexp.graphics.draw
  (:use [conexp.util :only (update-ns-meta!)]
	[conexp.layout :only (*standard-layout-function*)]
	[conexp.layout.force :only (force-layout)]
	[conexp.graphics.base :only (draw-on-scene,
				     get-layout-from-scene,
				     set-layout-of-scene,
				     move-interaction,
				     zoom-interaction)])
  (:import [javax.swing JFrame JPanel JButton]
	   [java.awt Dimension BorderLayout FlowLayout]
	   [java.awt.event ActionListener]))

(update-ns-meta! conexp.graphics.draw
  :doc "This namespace provides a lattice editor and a convenience function to draw lattices.")


;;; lattice editor -- a lot TODO

(declare make-button)

;; editor features

(defn- change-parameters
  "Installs parameter list which influences lattice drawing."
  [scn buttons]
  ;; node radius
  ;; labels
  ;; layout
  nil)

;; improve with force layout

(defn- improve-with-force
  "Improves layout on scene with force layout."
  [scn]
  (set-layout-of-scene scn (force-layout (get-layout-from-scene scn))))

(defn- improve-layout-by-force
  "Improves layout on screen by force layout."
  [scn, buttons]
  (let [button (make-button buttons "Force")]
    (.addActionListener button
			(proxy [ActionListener] []
			  (actionPerformed [evt]
			    (improve-with-force scn))))))

;; TODO: Add energy label,
;;       Add sliders for repulsive, attractive and gravitative amount

;;

(defn- toggle-zoom-move
  "Install zoom-move-toggler."
  [scn buttons]
  (let [button (make-button buttons "Zoom")]
    (.addActionListener button
			(proxy [ActionListener] []
			  (actionPerformed [evt]
			    (if (= "Zoom" (.getText button))
			      (do
				(.. scn getWindow (startInteraction (zoom-interaction scn)))
				(.setText button "Move"))
			      (do
				(.. scn getWindow (startInteraction (move-interaction scn)))
				(.setText button "Zoom"))))))))

;;

(defn- export-as-file
  "Installs a file exporter."
  [scn buttons]
  nil)

;; technical helpers

(defmacro install-changers
  "Installs given methods to scene with buttons."
  [scene buttons & methods]
  `(do
     ~@(map (fn [method#] `(~method# ~scene ~buttons))
	    methods)))

(defn- make-button
  "Uniformly create buttons for lattice editor."
  [buttons text]
  (let [button (JButton. text)]
    (.add buttons button)
    (.setPreferredSize button (Dimension. 100 20))
    button))

;; constructor

(defn make-lattice-editor
  "Creates a lattice editor for lattice with initial layout."
  [lattice layout-function]
  (let [#^JPanel main-panel (JPanel. (BorderLayout.)),

	scn (draw-on-scene (layout-function lattice)),
	canvas (.. scn getWindow getCanvas),

	buttons (JPanel. (FlowLayout.))]
    (.setPreferredSize buttons (Dimension. 110 0))
    (install-changers scn buttons
      toggle-zoom-move
      change-parameters
      improve-layout-by-force
      export-as-file)
    (doto main-panel
      (.add canvas BorderLayout/CENTER)
      (.add buttons BorderLayout/WEST))
    main-panel))


;;; drawing routine for the repl

(defn draw-lattice
  "Draws given lattice with given layout-function on a canvas and returns
  it. Uses *standard-layout-function* if no layout-function is given."
  ([lattice]
     (draw-lattice lattice *standard-layout-function*))
  ([lattice layout-function]
     (doto (JFrame. "conexp-clj Lattice")
       (.add (make-lattice-editor lattice layout-function))
       (.setSize (Dimension. 300 300))
       (.setVisible true))))


;;;

nil
