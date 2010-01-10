(ns conexp.graphics.draw
  (:use [conexp.util :only (update-ns-meta!)]
	[conexp.layout :only (*standard-layout*)]
	[conexp.layout.util :only (scale-layout)]
	[conexp.graphics.nodes-and-connections :only (*default-node-radius*)]
	[conexp.graphics.base :only (draw-on-canvas)])
  (:import [javax.swing JFrame JPanel]
	   [java.awt Dimension]
	   [no.geosoft.cc.graphics ZoomInteraction]))

(update-ns-meta! conexp.graphics.draw
  :doc "This namespace provides a lattice editor and a convenience function to draw lattices.")


;;;

;; change node radius
;; toogle zoom <-> move

(defn make-lattice-editor
  "Creates a lattice editor for lattice with initial layout."
  [lattice layout]
  (let [main-panel (JPanel.),
	;; add layout manager

	scn (draw-on-scene [0.0 0.0] [400.0 400.0]
			   (scale-layout [0.0 0.0] [400.0 400.0]
					 (layout lattice))),
	canvas (.. scn getWindow getCanvas)

	]
    canvas))


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
