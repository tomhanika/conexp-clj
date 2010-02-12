(ns conexp.graphics.draw
  (:use [conexp.util :only (update-ns-meta!)]
	[conexp.layout :only (*standard-layout-function*)]
	[conexp.layout.force :only (force-layout,
				    *repulsive-amount*,
				    *attractive-amount*,
				    *gravitative-amount*)]
	[conexp.graphics.scenes :only (add-callback-for-hook)]
	[conexp.graphics.base :only (draw-on-scene,
				     get-layout-from-scene,
				     set-layout-of-scene,
				     move-interaction,
				     zoom-interaction)])
  (:import [javax.swing JFrame JPanel JButton JTextField JLabel
	                JOptionPane JSeparator SwingConstants
	                BoxLayout Box]
	   [java.awt Canvas Color Dimension BorderLayout GridLayout Component Graphics]
	   [java.awt.event ActionListener]))

(update-ns-meta! conexp.graphics.draw
  :doc "This namespace provides a lattice editor and a convenience function to draw lattices.")


;;; lattice editor -- a lot TODO

(declare make-button, make-labeled-text-field, make-padding, make-separator)

;; editor features

(defn- change-parameters
  "Installs parameter list which influences lattice drawing."
  [frame scn buttons]
  ;; node radius
  ;; labels
  ;; layout
  nil)

;; improve with force layout

(defn- improve-with-force
  "Improves layout on scene with force layout."
  [scn r a g]
  (binding [*repulsive-amount* r,
	    *attractive-amount* a,
	    *gravitative-amount* g]
    (set-layout-of-scene scn (force-layout (get-layout-from-scene scn)))))

(defn- improve-layout-by-force
  "Improves layout on screen by force layout."
  [frame scn buttons]
  ;; Add parameters for repulsive, attractive and gravitative amount
  (let [#^JTextField rep-field  (make-labeled-text-field buttons "rep"  (str *repulsive-amount*)),
	#^JTextField attr-field (make-labeled-text-field buttons "attr" (str *attractive-amount*)),
	#^JTextField grav-field (make-labeled-text-field buttons "grav" (str *gravitative-amount*)),
	_                       (make-padding buttons),
	#^JButton button (make-button buttons "Force")]
    (.addActionListener button
			(proxy [ActionListener] []
			  (actionPerformed [evt]
			    (try
			     (let [r (Double/parseDouble (.getText rep-field)),
				   a (Double/parseDouble (.getText attr-field)),
				   g (Double/parseDouble (.getText grav-field))]
			       (improve-with-force scn r a g))
			     (catch NumberFormatException e
			       (JOptionPane/showMessageDialog
				frame,
				"Invalid number in parameters.",
				"Invalid parameters.",
				JOptionPane/ERROR_MESSAGE))
			     (catch Exception e
			       (JOptionPane/showMessageDialog
				frame,
				(.getMessage e)
				"An Error occured."
				JOptionPane/ERROR_MESSAGE))))))))

;; TODO: Add energy label


;; zoom-move

(defn- toggle-zoom-move
  "Install zoom-move-toggler."
  [frame scn buttons]
  (let [zoom-button (make-button buttons "Zoom"),
	move-button (make-button buttons "Move")]
    (.addActionListener zoom-button
			(proxy [ActionListener] []
			  (actionPerformed [evt]
			    (.. scn getWindow (startInteraction (zoom-interaction scn)))
			    (.setEnabled move-button true)
			    (.setEnabled zoom-button false))))
    (.addActionListener move-button
			(proxy [ActionListener] []
			  (actionPerformed [evt]
			    (.. scn getWindow (startInteraction (move-interaction scn)))
			    (.setEnabled zoom-button true)
			    (.setEnabled move-button false))))
    (.setEnabled zoom-button true)
    (.setEnabled move-button false)))

;;

(defn- export-as-file
  "Installs a file exporter."
  [frame scn buttons]
  nil)

;; technical helpers

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

(defn- make-padding
  "Adds a padding to buttons."
  [buttons]
  (.add buttons (Box/createRigidArea (Dimension. 0 2))))

(defn- make-separator
  "Adds a separator to buttons."
  [buttons]
  (let [sep (JSeparator. SwingConstants/HORIZONTAL)]
    (.setMaximumSize sep (Dimension. 100 1))
    (.add buttons sep)
    sep))

(defn- make-button
  "Uniformly creates buttons for lattice editor."
  [buttons text]
  (let [button (JButton. text)]
    (.add buttons button)
    (.setAlignmentX button Component/CENTER_ALIGNMENT)
    (.setMaximumSize button (Dimension. 100 20))
    button))

(defn- make-labeled-text-field
  "Uniformly creates a text field for lattice editor."
  [buttons label text]
  (let [#^JTextField text-field (JTextField. text),
	#^JLabel label (JLabel. label),
	#^JPanel panel (JPanel.)]
    (doto panel
      (.add label)
      (.add text-field)
      (.setMaximumSize (Dimension. 100 25)))
    (.setPreferredSize label (Dimension. 40 20))
    (.setPreferredSize text-field (Dimension. 50 20))
    (.add buttons panel)
    text-field))

;; constructor

(defn make-lattice-editor
  "Creates a lattice editor for lattice with initial layout."
  [frame lattice layout-function]
  (let [#^JPanel main-panel (JPanel. (BorderLayout.)),

	scn (draw-on-scene (layout-function lattice)),
	canvas (.. scn getWindow getCanvas),

	buttons (JPanel.),
	box-layout (BoxLayout. buttons BoxLayout/Y_AXIS)]
    (.setLayout buttons box-layout)
    (.setPreferredSize buttons (Dimension. 110 0))
    (install-changers frame scn buttons
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
     (let [#^JFrame frame (JFrame. "conexp-clj Lattice")]
       (doto frame
	 (.add (make-lattice-editor frame lattice layout-function))
	 (.setSize (Dimension. 300 300))
	 (.setVisible true)))))


;;;

nil
