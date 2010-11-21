;; Copyright (c) Daniel Borchmann. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.contrib.draw.lattices
  (:use [conexp.base                       :only (ns-doc, defnk)]
        [conexp.layouts                    :only (standard-layout)]
        [conexp.layouts.util               :only (scale-layout)]
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
  (:import [javax.swing JFrame JPanel BoxLayout JScrollBar JScrollPane]
           [java.awt Dimension BorderLayout]))

(ns-doc
 "This namespace provides a lattice editor and a convenience function
 to draw lattices.")

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

(defnk draw-lattice
  "Draws given lattice with given layout-function on a canvas. Returns
  the frame and the scene (as map). The following options are allowed,
  their default values are given in parantheses:

    - layout-fn (standard-layout)
    - visible (true)
    - dimension [600 600]
  "
  [lattice
   :layout-fn standard-layout,
   :visible true,
   :dimension [600 600]]
  (let [frame          (JFrame. "conexp-clj Lattice"),
        lattice-editor (make-lattice-editor frame (layout-fn lattice))]
    (doto frame
      (.add lattice-editor)
      (.setSize (Dimension. (first dimension) (second dimension)))
      (.setVisible visible))
    {:frame frame,
     :scene (get-scene-from-panel lattice-editor)}))

(defnk draw-lattice-to-file             ;does not work
  ""
  [lattice file-name
   :layout-fn standard-layout,
   :dimension [600 600]]
  (do-swing-return ;prevent some nasty AWT deadlocks while experimenting
    (let [frame (JFrame. ""),
          panel (make-lattice-editor frame (layout-fn lattice)),
          scene (get-scene-from-panel panel)]
      (.setPreferredSize panel (Dimension. 600 600))
      (.add frame panel)
      (fit-scene-to-layout scene)
      (.pack frame)
      (save-image scene (java.io.File. ^String file-name) "png"))))

(alter-meta! #'draw-lattice-to-file assoc :private true)

;;;

nil
