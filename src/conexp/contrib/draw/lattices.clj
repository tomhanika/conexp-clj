;; Copyright (c) Daniel Borchmann. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.contrib.draw.lattices
  (:use [conexp.base
         :only (ns-doc,
                illegal-argument,
                get-root-cause,
                with-swing-error-msg,
                with-printed-result,
                now,
                defvar-,
                defmacro-,
                defnk)]
	[conexp.math.util
         :only (with-doubles)]
	[conexp.layout
         :only (*standard-layout-function*,
                inf-additive-layout)]
	[conexp.layout.base
         :only (lattice,
                annotation)]
	[conexp.layout.force
         :only (force-layout,
                layout-energy,
                *repulsive-amount*,
                *attractive-amount*,
                *gravitative-amount*)]
        ;; drawing
	[conexp.contrib.draw.scenes
         :only (add-callback-for-hook,
                call-hook-with,
                redraw-scene,
                start-interaction,
                get-zoom-factors,
                save-image,
                get-canvas-from-scene,
                show-labels,
                add-scrollbars)]
	[conexp.contrib.draw.scene-layouts
         :only (draw-on-scene,
                get-layout-from-scene,
                update-layout-of-scene,
                do-nodes)]
	[conexp.contrib.draw.nodes-and-connections
         :only (move-interaction,
                zoom-interaction,
                move-node-by,
                all-nodes-above,
                all-nodes-below,
                all-inf-add-influenced-nodes,
                all-sup-add-influenced-nodes,
                *default-node-radius*,
                set-node-radius!)],
        conexp.contrib.draw.buttons)
  (:use clojure.contrib.swing-utils)
  (:import [javax.swing JFrame JPanel JButton JTextField JLabel
	                JSeparator SwingConstants BoxLayout Box
	                JScrollBar JComboBox JScrollPane JFileChooser]
	   [javax.swing.filechooser FileNameExtensionFilter]
	   [java.awt Canvas Color Dimension BorderLayout GridLayout Component Graphics]
	   [java.awt.event ActionEvent ActionListener]
	   [java.io File]))

(ns-doc
 "This namespace provides a lattice editor and a convenience function
 to draw lattices.")

;;; Lattice Editor

;; editor features

(declare single-move-mode, ideal-move-mode, filter-move-mode, chain-move-mode,
	 infimum-additive-move-mode, supremum-additive-move-mode)

(defn- change-parameters
  "Installs parameter list which influences lattice drawing."
  [frame scn buttons]
  ;; node radius
  (let [^JTextField node-radius (make-labeled-text-field buttons "radius" (str *default-node-radius*))]
    (add-action-listener node-radius
			 (fn [evt]
			   (let [new-radius (Double/parseDouble (.getText node-radius))]
			      (do-nodes [n scn]
				(set-node-radius! n new-radius))))))
  (make-padding buttons)

  ;; labels
  (let [^JButton label-toggler (make-button buttons "No Labels")]
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
				(.setText label-toggler "Labels")))
			    (redraw-scene scn)))))

  ;; layouts
  (let [layouts {"standard" *standard-layout-function*,
		 "inf-add"  inf-additive-layout},
	^JComboBox combo-box (make-combo-box buttons (keys layouts))]
    (add-action-listener combo-box
			 (fn [evt]
			   (let [selected (.. evt getSource getSelectedItem),
				 layout-fn (get layouts selected)]
			     (do-swing
			      (update-layout-of-scene
			       scn
			       (layout-fn (lattice (get-layout-from-scene scn)))))))))

  ;; move mode
  (let [move-modes {"single" (single-move-mode),
		    "ideal"  (ideal-move-mode),
		    "filter" (filter-move-mode),
		    "chain"  (chain-move-mode),
		    "inf"    (infimum-additive-move-mode),
		    "sup"    (supremum-additive-move-mode)}
	^JComboBox combo-box (make-combo-box buttons (keys move-modes)),
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
  (neighbor-move-mode (memoize all-nodes-below)))

(defn- filter-move-mode
  "Moves all nodes above the current node."
  []
  (neighbor-move-mode (memoize all-nodes-above)))

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
  (let [^JTextField rep-field    (make-labeled-text-field buttons "rep"  (str *repulsive-amount*)),
	^JTextField attr-field   (make-labeled-text-field buttons "attr" (str *attractive-amount*)),
	^JTextField grav-field   (make-labeled-text-field buttons "grav" (str *gravitative-amount*)),
	^JTextField iter-field   (make-labeled-text-field buttons "iter" (str "300")),
	_                        (make-padding buttons),
	^JButton button          (make-button buttons "Force"),

	get-force-parameters     (fn []
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
	^JButton zoom-move (make-button buttons "Move"),
	^JLabel  zoom-info (make-label buttons " -- ")]
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
  [^File file]
  (let [name (.getName file),
	idx  (.lastIndexOf name ".")]
    (if-not (= -1 idx)
      (.toLowerCase (.substring name (+ 1 idx)))
      nil)))

(defn- export-as-file
  "Installs a file exporter."
  [frame scn buttons]
  (let [^JButton save-button (make-button buttons "Save"),
	^JFileChooser fc (JFileChooser.),
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
				 (with-swing-error-msg frame "Error while saving"
				   (save-image scn file (get-file-extension file)))))))))
  nil)

;; save changes

(defn- snapshot-saver
  "Installs a snapshot saver, which, whenever a node has been moved,
  saves the image."
  [frame scn buttons]
  (let [saved-layouts (atom {}),
	^JComboBox combo (make-combo-box buttons @saved-layouts),
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


;;; Technical Helpers

(defmacro- with-layout-modifiers
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

(let [scenes (ref {})]			;memory leak possible

  (defn make-lattice-editor
    "Creates a lattice editor with initial layout."
    [frame layout]
    (let [^JPanel main-panel (JPanel. (BorderLayout.)),

	  scn (draw-on-scene layout),
	  ^Canvas canvas (get-canvas-from-scene scn),

	  ^JPanel canvas-panel (JPanel. (BorderLayout.)),
	  ^JScrollBar hscrollbar (JScrollBar. JScrollBar/HORIZONTAL),
	  ^JScrollBar vscrollbar (JScrollBar. JScrollBar/VERTICAL),

	  ^JPanel buttons (JPanel.),
	  box-layout (BoxLayout. buttons BoxLayout/Y_AXIS)]

      ;; save scene
      (dosync (alter scenes assoc main-panel scn))

      ;; buttons
      (.setLayout buttons box-layout)
      (.setPreferredSize buttons (Dimension. *toolbar-width* 600))
      (with-layout-modifiers frame scn buttons
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
      (add-scrollbars scn hscrollbar vscrollbar)

      ;; main panel
      (doto main-panel
	(.add canvas-panel BorderLayout/CENTER)
	(.add (JScrollPane. buttons JScrollPane/VERTICAL_SCROLLBAR_ALWAYS
			    JScrollPane/HORIZONTAL_SCROLLBAR_NEVER)
	      BorderLayout/WEST)
	(.setMinimumSize (Dimension. 0 0)))

      ;;
      main-panel))

  (defn get-scene-from-panel
    "If the given panel contains a lattice editor, returns the
    corresponding scene, nil otherwise."
    [panel]
    (get @scenes panel nil))

  (defn get-layout-from-panel
    "If the given panel contains a lattice editor, return the
    corresponding layout and nil otherwise."
    [panel]
    (when-let [scn (get-scene-from-panel panel)]
      (get-layout-from-scene scn)))

  nil)


;;; Drawing Routine for the REPL

(defnk draw-lattice
  "Draws given lattice with given layout-function on a canvas. Returns
  the frame and the scene (as map). The following options are allowed,
  their default values are given in parantheses:

    - layout-fn (*standard-layout-function*)
    - visible (true)
    - dimension [600 600]
  "
  [lattice
   :layout-fn *standard-layout-function*
   :visible true
   :dimension [600 600]]
  (let [^JFrame frame (JFrame. "conexp-clj Lattice"),
        ^JPanel lattice-editor (make-lattice-editor frame (layout-fn lattice))]
    (doto frame
      (.add lattice-editor)
      (.setSize (Dimension. (first dimension) (second dimension)))
      (.setVisible visible))
    {:frame frame,
     :scene (get-scene-from-panel lattice-editor)}))

;;;

nil
