;; Copyright (c) Daniel Borchmann. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.contrib.draw.lattices
  (:use [conexp.base                       :only (ns-doc, defnk)]
        [conexp.layouts                    :only (*standard-layout-function*)]
        [conexp.layouts.util               :only (scale-layout)]
        [conexp.contrib.draw.scenes        :only (scene-canvas,
                                                  add-scrollbars)]
        [conexp.contrib.draw.scene-layouts :only (draw-on-scene,
                                                  get-layout-from-scene)]
        [conexp.contrib.draw.control util
                                     parameters
                                     freese
                                     file-exporter
                                     force-layout
                                     snapshots
                                     zoom-move])
  (:import [javax.swing JFrame JPanel BoxLayout JScrollBar JScrollPane]
           [java.awt Canvas Dimension BorderLayout]))

(ns-doc
 "This namespace provides a lattice editor and a convenience function
 to draw lattices.")

;;; Lattice Editor

(let [scenes (ref {})]

  (defn make-lattice-editor
    "Creates a lattice editor with initial layout."
    [frame layout]
    (let [layout (scale-layout [0 0] [100 100] layout),

          ^JPanel main-panel (JPanel. (BorderLayout.)),

          scn    (draw-on-scene layout),
          canvas (scene-canvas scn),

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
