;; Copyright ⓒ the conexp-clj developers; all rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.contrib.draw.lattices
  "This namespace provides a lattice editor and a convenience function to draw lattices."
  (:use [conexp.fca.lattices               :only (concept-lattice)]
        [conexp.layouts                    :only (standard-layout)]
        [conexp.layouts.util               :only (scale-layout)]
        [conexp.io.layouts                 :only (write-layout)]
        [conexp.contrib.draw.scenes        :only (scene-canvas,
                                                  add-scrollbars
                                                  save-image)]
        [conexp.contrib.draw.scene-layouts :only (draw-on-scene,
                                                  get-layout-from-scene
                                                  fit-scene-to-layout)]
        [conexp.contrib.draw.control util
                                     parameters
                                     freese
                                     file-exporter
                                     force-layout
                                     snapshots
                                     zoom-move]
        conexp.contrib.gui.util)
  (:use seesaw.core)
  (:import [javax.swing JFrame JPanel BoxLayout JScrollBar JScrollPane]
           [java.awt Dimension BorderLayout]))

;;; Lattice Editor

(defprotocol WithScene
  "Storing scene objects."
  (^Gscene getScene [this]
    "Returns the associated scene."))

(defn ^JPanel make-lattice-editor
  "Creates a lattice editor with initial layout."
  [frame layout]
  (let [layout       (scale-layout [0 0] [100 100] layout),

        scn          (draw-on-scene layout),
        canvas       (scene-canvas scn),
        main-panel   (proxy [JPanel conexp.contrib.draw.lattices.WithScene] [(BorderLayout.)]
                       (getScene []
                         scn)),

        canvas-panel (JPanel. (BorderLayout.)),
        hscrollbar   (JScrollBar. JScrollBar/HORIZONTAL),
        vscrollbar   (JScrollBar. JScrollBar/VERTICAL),

        buttons      (JPanel.),
        box-layout   (BoxLayout. buttons BoxLayout/Y_AXIS)]

    ;; buttons
    (.setLayout buttons box-layout)
    (.setPreferredSize buttons (Dimension. *toolbar-width* 600))
    (with-layout-modifiers frame scn buttons
      toggle-zoom-move
      change-parameters,
      (control-choice "Freese" freese,
                      "Force"  improve-layout-by-force),
      snapshot-saver,
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
    (listen main-panel :component-hidden
            (fn [_]
              (.setVisible canvas false)))
    (listen main-panel :component-shown
            (fn [_]
              (.setVisible canvas true)))

    ;; return main panel
    main-panel))

(defn get-scene-from-panel
  "If the given panel contains a lattice editor, returns the
  corresponding scene, nil otherwise."
  [panel]
  (when (instance? conexp.contrib.draw.lattices.WithScene panel)
    (.getScene ^conexp.contrib.draw.lattices.WithScene panel)))

(defn get-layout-from-panel
  "If the given panel contains a lattice editor, return the
  corresponding layout and nil otherwise."
  [panel]
  (when-let [scn (get-scene-from-panel panel)]
    (get-layout-from-scene scn)))


;;; Drawing Routine for the REPL

(defn draw-layout
  "Draws given layout on a canvas. Returns the frame and the scene (as
  map). The following options are allowed, their default values are
  given in parantheses:

    - visible (true)
    - dimension [600 600]
  "
  [layout
   & {:keys [visible dimension]
      :or   {visible   true,
             dimension [600 600]}}]
  (let [frame          (JFrame. "conexp-clj Lattice"),
        lattice-editor (make-lattice-editor frame layout)]
    (doto frame
      (.add lattice-editor)
      (.setSize (Dimension. (first dimension) (second dimension)))
      (.setVisible visible))
    {:frame frame,
     :scene (get-scene-from-panel lattice-editor)}))

(defn draw-lattice
  "Draws lattice with given layout. Passes all other parameters to
  draw-layout."
  [lattice & args]
  (let [map       (apply hash-map args),
        layout-fn (get map :layout-fn standard-layout)]
    (apply draw-layout (layout-fn lattice) args)))

(defn draw-concept-lattice
  "Draws the concept lattice of a given context, passing all remaining
  args to draw-lattice."
  [ctx & args]
  (apply draw-lattice (concept-lattice ctx) args))

;;;

(defn draw-lattice-to-file
  "Exports layout of given lattice to the given file."
  [lattice file-name
   & {:keys [layout-fn dimension]
      :or   {layout-fn standard-layout,
             dimension [600 600]}}]
  (write-layout :svg (layout-fn lattice) file-name))

;;;

nil
