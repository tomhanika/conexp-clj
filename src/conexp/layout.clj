(ns conexp.layout
  (:use [clojure.contrib.ns-utils :only (immigrate)]
	[conexp.graphics.base :only (draw-on-canvas)]
	conexp.layout.base)
  (:import [javax.swing JFrame]))

(immigrate 'conexp.layout.basic-layouts)

(def *standard-layout* simple-layered-layout)

(defn draw-lattice
  "Draws given lattice with given layout on a canvas and returns
  it. Uses *standard-layout* if no layout is given."
  ([lattice]
     (draw-lattice lattice *standard-layout*))
  ([lattice layout]
     (doto (JFrame. "conexp-clj Lattice")
       (.add (draw-on-canvas [0.0 0.0] [100.0 100.0]
			     (scale-layout [0.0 0.0] [100.0 100.0] (layout lattice))))
       (.setVisible true))))

nil
