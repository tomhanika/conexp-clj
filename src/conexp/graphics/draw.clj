(ns conexp.graphics.draw
  (:use conexp.layout
	conexp.layout.util
	conexp.graphics.base)
  (:import [javax.swing JFrame]
	   [java.awt Dimension]))


;;; extend this to get a full "lattice editor"
(defn draw-lattice
  "Draws given lattice with given layout on a canvas and returns
  it. Uses *standard-layout* if no layout is given."
  ([lattice]
     (draw-lattice lattice *standard-layout*))
  ([lattice layout]
     (doto (JFrame. "conexp-clj Lattice")
       (.add (draw-on-canvas [0.0 0.0] [400.0 400.0]
			     (scale-layout [-200.0 -200.0] [600.0 600.0] (layout lattice))))
       (.setSize (Dimension. 400 400))
       (.setVisible true))))


;;;

nil
