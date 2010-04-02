;; Copyright (c) Daniel Borchmann. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.graphics.draw
  (:use [conexp.util :only (update-ns-meta!,
			    get-root-cause,
			    with-swing-error-msg,
			    with-printed-result,
			    now)]
	[conexp.base :only (defvar- defmacro-)]
	[conexp.math.util :only (with-doubles)]
	[conexp.layout :only (*standard-layout-function*)]
	[conexp.layout.base :only (lattice, annotation)]
	[conexp.layout.force :only (force-layout,
				    layout-energy,
				    *repulsive-amount*,
				    *attractive-amount*,
				    *gravitative-amount*)]
	[conexp.graphics.util :only (device-to-world)]
	[conexp.graphics.scenes :only (add-callback-for-hook,
				       redraw-scene,
				       start-interaction,
				       get-zoom-factors,
				       save-image,
				       get-canvas-from-scene,
				       show-labels)]
	[conexp.graphics.scene-layouts :only (draw-on-scene,
					      get-layout-from-scene,
					      update-layout-of-scene,
					      do-nodes)]
	[conexp.graphics.nodes-and-connections :only (move-interaction,
						      zoom-interaction,
						      move-node-by,
						      all-nodes-above,
						      all-nodes-below,
						      all-inf-add-influenced-nodes,
						      all-sup-add-influenced-nodes,
						      *default-node-radius*,
						      set-node-radius!)]
	clojure.contrib.swing-utils)
  (:import [javax.swing JFrame JPanel JButton JTextField JLabel
	                JOptionPane JSeparator SwingConstants
	                BoxLayout Box JScrollBar JComboBox JScrollPane
	                JFileChooser]
	   [javax.swing.filechooser FileNameExtensionFilter]
	   [java.awt Canvas Color Dimension BorderLayout GridLayout Component Graphics]
	   [java.awt.event ActionListener]
	   [java.io File]))

(update-ns-meta! conexp.graphics.draw
  :doc "This namespace provides a lattice editor and a convenience function to draw lattices.")


;;; Lattice Editor

(declare make-button, make-label, make-labeled-text-field,
	 make-padding, make-separator, make-combo-box)


;; editor features

(declare single-move-mode, ideal-move-mode, filter-move-mode, chain-move-mode,
	 infimum-additive-move-mode, supremum-additive-move-mode)

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
  (make-padding buttons)

  ;; labels
  (let [#^JButton label-toggler (make-button buttons "No Labels")]
    (show-labels scn false)
    (add-action-listener label-toggler
			 (fn [evt]
			   (do-swing
			    (if (= "Labels" (.getText label-toggler))
			      (do
				(show-labels scn false)
				(.setText label-toggler "No Labels"))
			      (do
				(show-labels scn true)
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
  (let [move-modes {"single" (single-move-mode),
		    "ideal"  (ideal-move-mode),
		    "filter" (filter-move-mode),
		    "chain"  (chain-move-mode),
		    "inf"    (infimum-additive-move-mode),
		    "sup"    (supremum-additive-move-mode)}
	#^JComboBox combo-box (make-combo-box buttons (keys move-modes)),
	current-move-mode (atom (move-modes "single"))]
    (add-callback-for-hook scn :move-drag
			   (fn [node dx dy]
			     (@current-move-mode node dx dy)))
    (add-action-listener combo-box
			 (fn [evt]
			   (let [selected (.. evt getSource getSelectedItem),
				 move-mode (get move-modes selected)]
			     (reset! current-move-mode move-mode)))))
  nil)

(defn- single-move-mode
  "Moves the single node only."
  []
  (fn [node dx dy]
    nil))

(defn- neighbor-move-mode
  "Moves nodes neighbored to node by [dx dy]."
  [neighbors]
  (fn [node dx dy]
    (do-swing
     (doseq [n (neighbors node)]
       (move-node-by n dx dy)))))

(defn- ideal-move-mode
  "Moves all nodes below the current node."
  []
  (neighbor-move-mode (memoize all-nodes-above)))

(defn- filter-move-mode
  "Moves all nodes above the current node."
  []
  (neighbor-move-mode (memoize all-nodes-below)))

(defn- chain-move-mode
  "Combined ideal and filter move mode."
  []
  (let [ideal (ideal-move-mode),
	filter (filter-move-mode)]
    (fn [node dx dy]
      (ideal node dx dy)
      (filter node dx dy))))

(defn- additive-move-mode
  "Abstract move mode for moving nodes according to additive
  influence."
  [influenced-nodes]
  (fn [node dx dy]
    (do-swing
     (doseq [[n weight] (influenced-nodes node)]
       (with-doubles [dx dy weight]
	 (move-node-by n (* dx weight) (* dy weight)))))))

(defn- infimum-additive-move-mode
  "Moves all nodes infimum-additively with node."
  []
  (additive-move-mode (memoize all-inf-add-influenced-nodes)))

(defn- supremum-additive-move-mode
  "Moves all nodes supremum-additively with node."
  []
  (additive-move-mode (memoize all-sup-add-influenced-nodes)))


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
				  (do-swing
				   (with-swing-error-msg frame "An Error occured."
				     (let [[r a g i] (get-force-parameters)]
				       (improve-with-force scn i r a g))))))))

;; zoom-move

(defn- toggle-zoom-move
  "Install zoom-move-toggler."
  [frame scn buttons]
  (let [zoom-factors (fn []
		       (let [[zoom-x zoom-y] (get-zoom-factors scn)]
			 (with-out-str
			   (printf "%1.2f" zoom-x)
			   (print " x ")
			   (printf "%1.2f" zoom-y)))),
	#^JButton zoom-move (make-button buttons "Move"),
	#^JLabel  zoom-info (make-label buttons " -- ")]
    (add-action-listener zoom-move
			 (fn [evt]
			   (do-swing
			    (if (= "Move" (.getText zoom-move))
			      (do
				(start-interaction scn zoom-interaction)
				(.setText zoom-move "Zoom"))
			      (do
				(start-interaction scn move-interaction)
				(.setText zoom-move "Move"))))))
    (add-callback-for-hook scn :image-changed
			   (fn []
			     (do-swing
			      (.setText zoom-info (zoom-factors))))))
  nil)

;; export images to files

(defn- get-file-extension
  "Returns file extension of given file."
  [#^File file]
  (let [name (.getName file),
	idx  (.lastIndexOf name ".")]
    (if-not (= -1 idx)
      (.toLowerCase (.substring name (+ 1 idx)))
      nil)))

(defn- export-as-file
  "Installs a file exporter."
  [frame scn buttons]
  (let [#^JButton save-button (make-button buttons "Save"),
	#^JFileChooser fc (JFileChooser.),
	jpg-filter (FileNameExtensionFilter. "JPEG Files" (into-array ["jpg" "jpeg"])),
	gif-filter (FileNameExtensionFilter. "GIF Files"  (into-array ["gif"])),
	png-filter (FileNameExtensionFilter. "PNG Files"  (into-array ["png"]))]
    (doto fc
      (.addChoosableFileFilter jpg-filter)
      (.addChoosableFileFilter gif-filter)
      (.addChoosableFileFilter png-filter))
    (add-action-listener save-button
			 (fn [evt]
			   (let [retVal (.showSaveDialog fc frame)]
			     (when (= retVal JFileChooser/APPROVE_OPTION)
			       (let [#^File file (.getSelectedFile fc)]
				 (try
				  (save-image scn file (get-file-extension file))
				  (catch Exception e
				    (JOptionPane/showMessageDialog
				     frame
				     (get-root-cause e)
				     "Error while saving"
				     JOptionPane/ERROR_MESSAGE)))))))))
  nil)

;; save changes

(defn- snapshot-saver
  "Installs a snapshot saver, which, whenever a node has been moved,
  saves the image."
  [frame scn buttons]
  (let [saved-layouts (atom {}),
	#^JComboBox combo (make-combo-box buttons @saved-layouts),
	save-layout   (fn [_]
			(do-swing
			 (let [layout (get-layout-from-scene scn),
			       key    (now)]
			   (swap! saved-layouts conj [key, layout])
			   (.addItem combo key))))]
    (add-callback-for-hook scn :move-stop save-layout)
    (add-action-listener combo
			 (fn [evt]
			   (do-swing
			    (let [selected (.. evt getSource getSelectedItem),
				  layout (@saved-layouts selected)]
			      (update-layout-of-scene scn layout)))))
    (save-layout nil)))


;;; Buttons, Labels and the like

(defvar- *item-width* 100
  "Width of items in toolbar.")

(defvar- *item-height* 25
  "Heights of items on toolbar.")

(defvar- *toolbar-width* (+ 20 *item-width*)
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

(defmacro- install-changers
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

(let [scenes (ref {})]

  (defn make-lattice-editor
    "Creates a lattice editor with initial layout."
    [frame layout]
    (let [#^JPanel main-panel (JPanel. (BorderLayout.)),

	  scn (draw-on-scene layout),
	  canvas (get-canvas-from-scene scn),

	  #^JPanel canvas-panel (JPanel. (BorderLayout.)),
	  hscrollbar (JScrollBar. JScrollBar/HORIZONTAL),
	  vscrollbar (JScrollBar. JScrollBar/VERTICAL),

	  #^JPanel buttons (JPanel.),
	  box-layout (BoxLayout. buttons BoxLayout/Y_AXIS)]

      ;; save scene
      (dosync (alter scenes assoc main-panel scn))

      ;; buttons
      (.setLayout buttons box-layout)
      (.setPreferredSize buttons (Dimension. *toolbar-width* 600))
      (install-changers frame scn buttons
        toggle-zoom-move
	change-parameters
	improve-layout-by-force
	snapshot-saver
	export-as-file)

      ;; drawing area
      (doto canvas-panel
	(.add canvas BorderLayout/CENTER)
	(.add hscrollbar BorderLayout/SOUTH)
	(.add vscrollbar BorderLayout/EAST))
      (.installScrollHandler scn hscrollbar vscrollbar)

      ;; main panel
      (doto main-panel
	(.add canvas-panel BorderLayout/CENTER)
	(.add (JScrollPane. buttons JScrollPane/VERTICAL_SCROLLBAR_ALWAYS
			    JScrollPane/HORIZONTAL_SCROLLBAR_NEVER)
	      BorderLayout/WEST))

      main-panel))

  (defn get-layout-from-panel
    "If given panel contains a lattice editor, return the
    corresponding layout and nil otherwise."
    [frame]
    (when-let [scn (get @scenes frame nil)]
      (get-layout-from-scene scn)))

  nil)


;;; Drawing Routine for the REPL

(defn draw-lattice
  "Draws given lattice with given layout-function on a canvas and returns
  it. Uses *standard-layout-function* if no layout-function is given."
  ([lattice]
     (draw-lattice lattice *standard-layout-function*))
  ([lattice layout-function]
     (let [#^JFrame frame (JFrame. "conexp-clj Lattice")]
       (doto frame
	 (.add (make-lattice-editor frame (layout-function lattice)))
	 (.setSize (Dimension. 600 600))
	 (.setVisible true)))))


;;;

nil
