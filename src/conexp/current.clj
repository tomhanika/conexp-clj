(ns conexp.current
  (:require conexp))

;;;

(comment "For Testing"

(require 'conexp.layout.force)
(defn simple-layered-force-layout [lat]
  (conexp.layout.force/force-layout (conexp/simple-layered-layout lat)))
(def lat (conexp/concept-lattice (conexp/rand-context #{1 2 3 4 5 6} 0.4)))
(conexp/draw-lattice lat)

)

;;;

nil
