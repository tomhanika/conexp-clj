(ns conexp.graphics.draw
  (:use [conexp.util :only (update-ns-meta!,
			    get-root-cause,
			    with-swing-error-msg,
			    with-printed-result)]
	[conexp.base :only (defvar-)]
	[conexp.layout :only (*standard-layout-function*)]
	[conexp.layout.base :only (lattice)]
	[conexp.layout.force :only (force-layout,
				    layout-energy,
				    *repulsive-amount*,
				    *attractive-amount*,
				    *gravitative-amount*)]
	[conexp.graphics.base :only (draw-on-scene,
				     get-layout-from-scene,
				     update-layout-of-scene,
				     move-interaction,
				     zoom-interaction,
				     do-nodes,
				     *default-node-radius*,
				     set-node-radius!,
				     add-callback-for-hook,
				     redraw-scene)]
	clojure.contrib.swing-utils)
  (:import [javax.swing JFrame JPanel JButton JTextField JLabel
	                JOptionPane JSeparator SwingConstants
	                BoxLayout Box JScrollBar JComboBox]
	   [java.awt Canvas Color Dimension BorderLayout GridLayout Component Graphics]
	   [java.awt.event ActionListener]
	   [no.geosoft.cc.graphics GScene]))

(update-ns-meta! conexp.graphics.draw
  :doc "This namespace provides a lattice editor and a convenience function to draw lattices.")


;;; Lattice Editor

(declare make-button, make-label, make-labeled-text-field,
	 make-padding, make-separator, make-combo-box)


;; editor features

(defn- change-parameters
  "Installs parameter list which influences lattice drawing."
  [frame scn buttons]
  ;; node radius
  (let [#^JTextField node-radius (make-labeled-text-field buttons "radius" (str *default-node-radius*))]
    (add-action-listener node-radius
			 (fn [evt]
			   (let [new-radius (Double/parseDouble (.getText node-radius))]
			     (do-swing
			      (do-nodes [n scn]
					(set-node-radius! n new-radius))
			      (redraw-scene scn))))))

  ;; labels
  (let [#^JButton label-toggler (make-button buttons "No Labels")]
    (.setVisibility scn GScene/ANNOTATION_INVISIBLE)
    (add-action-listener label-toggler
			 (fn [evt]
			   (do-swing
			    (if (= "Labels" (.getText label-toggler))
			      (do
				(.setVisibility scn GScene/ANNOTATION_INVISIBLE)
				(.setText label-toggler "No Labels"))
			      (do
				(.setVisibility scn GScene/ANNOTATION_VISIBLE)
				(.setText label-toggler  "Labels")))
			    (redraw-scene scn)))))

  ;; layouts
  (let [layouts {"standard" *standard-layout-function*},
	#^JComboBox combo-box (make-combo-box buttons (keys layouts))]
    (add-action-listener combo-box
			 (fn [evt]
			   (let [selected (.. evt getSource getSelectedItem),
				 layout-fn (get layouts selected)]
			     (do-swing
			      (update-layout-of-scene
			       scn
			       (layout-fn (lattice (get-layout-from-scene scn)))))))))

  ;; move mode (ideal, filter, chain, single)
  nil)


;; improve with force layout

(defn- improve-with-force
  "Improves layout on scene with force layout."
  [scn iterations r a g]
  (binding [*repulsive-amount* r,
	    *attractive-amount* a,
	    *gravitative-amount* g]
    (update-layout-of-scene scn
			    (if (<= iterations 0)
			      (force-layout (get-layout-from-scene scn))
			      (force-layout (get-layout-from-scene scn) iterations)))))

(defn- improve-layout-by-force
  "Improves layout on screen by force layout."
  [frame scn buttons]
  (let [#^JTextField rep-field    (make-labeled-text-field buttons "rep"  (str *repulsive-amount*)),
	#^JTextField attr-field   (make-labeled-text-field buttons "attr" (str *attractive-amount*)),
	#^JTextField grav-field   (make-labeled-text-field buttons "grav" (str *gravitative-amount*)),
	#^JTextField iter-field   (make-labeled-text-field buttons "iter" (str "300")),
	_                         (make-padding buttons),
	#^JButton button          (make-button buttons "Force"),

	get-force-parameters      (fn []
				    (let [r (Double/parseDouble (.getText rep-field)),
					  a (Double/parseDouble (.getText attr-field)),
					  g (Double/parseDouble (.getText grav-field)),
					  i (Integer/parseInt (.getText iter-field))]
				      [r a g i]))]
    (add-action-listener button (fn [evt]
				  (with-swing-error-msg frame "An Error occured."
				   (let [[r a g i] (get-force-parameters)]
				     (improve-with-force scn i r a g)))))))


;; zoom-move

(defn- toggle-zoom-move
  "Install zoom-move-toggler."
  [frame scn buttons]
  (let [#^JButton zoom-move (make-button buttons "Move"),
	#^JLabel  zoom-info   (make-label buttons "1.0")]
    (add-action-listener zoom-move
			 (fn [evt]
			   (do-swing
			    (if (= "Move" (.getText zoom-move))
			      (do
				(.. scn getWindow (startInteraction (zoom-interaction scn)))
				(.setText zoom-move "Zoom"))
			      (do
				(.. scn getWindow (startInteraction (move-interaction scn)))
				(.setText zoom-move "Move"))))))
    (add-callback-for-hook scn :zoom-event
			   (fn []
			     (do-swing
			      ;; TODO: Show current zoom factor
			      (.setText zoom-info "??"))))))


;; export images to files

(defn- export-as-file
  "Installs a file exporter."
  [frame scn buttons]
  nil)


;;; Buttons, Labels and the like

(defvar- *item-width* 100
  "Width of items in toolbar.")

(defvar- *item-height* 25
  "Heights of items on toolbar.")

(defvar- *toolbar-width* (+ 10 *item-width*)
  "Width of toolbar containing buttons, labels and so on.")

(defn- make-padding
  "Adds a padding to buttons."
  [buttons]
  (.add buttons (Box/createRigidArea (Dimension. 0 2))))

(defn- make-separator
  "Adds a separator to buttons."
  [buttons]
  (let [sep (JSeparator. SwingConstants/HORIZONTAL)]
    (.setMaximumSize sep (Dimension. *item-width* 1))
    (.add buttons sep)
    sep))

(defn- make-button
  "Uniformly creates buttons for lattice editor."
  [buttons text]
  (let [button (JButton. text)]
    (.add buttons button)
    (.setAlignmentX button Component/CENTER_ALIGNMENT)
    (.setMaximumSize button (Dimension. *item-width* *item-height*))
    button))

(defn- make-label
  "Uniformly creates labels for lattice editor."
  [buttons text]
  (let [label (JLabel. text)]
    (.add buttons label)
    (.setMaximumSize label (Dimension. *item-width* *item-height*))
    (.setAlignmentX label Component/CENTER_ALIGNMENT)
    (.setHorizontalAlignment label SwingConstants/CENTER)
    label))

(defn- make-labeled-text-field
  "Uniformly creates a text field for lattice editor."
  [buttons label text]
  (let [#^JTextField text-field (JTextField. text),
	#^JLabel label (JLabel. label),
	#^JPanel panel (JPanel.)]
    (doto panel
      (.add label)
      (.add text-field)
      (.setMaximumSize (Dimension. *item-width* *item-height*)))
    (.setPreferredSize label (Dimension. (* 0.4 *item-width*) (* 0.8 *item-height*)))
    (.setPreferredSize text-field (Dimension. (* 0.5 *item-width*) (* 0.8 *item-height*)))
    (.add buttons panel)
    text-field))

(defn- make-combo-box
  "Uniformly creates a combo box from the given choices. First item is
  selected by default."
  [buttons choices]
  (let [#^JComboBox combo-box (JComboBox. (into-array String choices))]
    (doto combo-box
      (.setMaximumSize (Dimension. *item-width* *item-height*)))
    (.add buttons combo-box)
    combo-box))


;;; Technical Helpers

(defmacro install-changers
  "Installs given methods to scene with buttons."
  [frame scene buttons & methods]
  `(do
     (make-padding ~buttons)
     ~@(map (fn [method#]
	      `(~method# ~frame ~scene ~buttons))
	    (interpose (fn [_ _ buttons]
			 (make-padding buttons)
			 (make-separator buttons)
			 (make-padding buttons))
		       methods))))


;;; Constructor

(defn make-lattice-editor
  "Creates a lattice editor for lattice with initial layout."
  [frame lattice layout-function]
  (let [#^JPanel main-panel (JPanel. (BorderLayout.)),

	scn (draw-on-scene (layout-function lattice)),
	canvas (.. scn getWindow getCanvas),

	#^JPanel canvas-panel (JPanel. (BorderLayout.)),
	hscrollbar (JScrollBar. JScrollBar/HORIZONTAL),
	vscrollbar (JScrollBar. JScrollBar/VERTICAL),

	#^JPanel buttons (JPanel.),
	box-layout (BoxLayout. buttons BoxLayout/Y_AXIS)]
    (.setLayout buttons box-layout)
    (.setPreferredSize buttons (Dimension. *toolbar-width* 0))
    (install-changers frame scn buttons
      toggle-zoom-move
      change-parameters
      improve-layout-by-force
      export-as-file)
    (doto canvas-panel
      (.add canvas BorderLayout/CENTER)
      (.add hscrollbar BorderLayout/SOUTH)
      (.add vscrollbar BorderLayout/EAST))
    (.installScrollHandler scn hscrollbar vscrollbar)
    (doto main-panel
      (.add canvas-panel BorderLayout/CENTER)
      (.add buttons BorderLayout/WEST))
    main-panel))


;;; Drawing Routine for the REPL

(defn draw-lattice
  "Draws given lattice with given layout-function on a canvas and returns
  it. Uses *standard-layout-function* if no layout-function is given."
  ([lattice]
     (draw-lattice lattice *standard-layout-function*))
  ([lattice layout-function]
     (let [#^JFrame frame (JFrame. "conexp-clj Lattice")]
       (doto frame
	 (.add (make-lattice-editor frame lattice layout-function))
	 (.setSize (Dimension. 600 600))
	 (.setVisible true)))))


;;;

nil
