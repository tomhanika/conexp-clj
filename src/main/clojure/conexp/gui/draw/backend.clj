;; Copyright ⓒ the conexp-clj developers; all rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.gui.draw.backend
  "Selectable rendering backend for lattice diagrams.

  A Backend knows how to create a scene and populate it with nodes, connections
  and grid points, and how to install the standard node-move interaction.  The
  scene and node objects it produces implement the AbstractScene / AbstractNode
  protocols, so all the drawing code built on top of them is backend-agnostic.

  The Swing no.geosoft G-canvas (`g-backend`) is the default; binding `*backend*`
  to another Backend implementation lets the same drawing code target a
  different renderer (e.g. a web canvas)."
  (:require [conexp.gui.draw.scenes :as scenes]
            [conexp.gui.draw.nodes-and-connections :as nc]))

;;;

(defprotocol Backend
  "Constructs the concrete scene and node objects for a rendering backend."
  (new-scene [backend]
    "Creates and returns a fresh, empty scene.")
  (add-node [backend scene x y name labels valuation]
    "Adds a node at world coordinate [x y] to scene and returns it. labels is a
    pair [upper-label lower-label].")
  (add-connection [backend scene lower upper]
    "Connects the lower and upper nodes on scene and returns the connection.")
  (add-grid-point [backend scene x y]
    "Adds a grid point at [x y] to scene.")
  (install-move-interaction [backend scene]
    "Installs the standard node-move interaction on scene."))

;;; The default no.geosoft G-canvas backend.

(def g-backend
  "The default no.geosoft G-canvas backend."
  (reify Backend
    (new-scene [_]
      (scenes/make-scene (scenes/make-window)))
    (add-node [_ scene x y name labels valuation]
      (nc/add-node scene x y name labels valuation))
    (add-connection [_ scene lower upper]
      (nc/connect-nodes scene lower upper))
    (add-grid-point [_ scene x y]
      (nc/grid-point scene x y))
    (install-move-interaction [_ scene]
      (scenes/start-interaction scene nc/move-interaction))))

(def ^:dynamic *backend*
  "The rendering backend used to construct lattice diagrams.  Defaults to the
  no.geosoft G-canvas; rebind to target another renderer."
  g-backend)

;;; Building a diagram with the current backend.

(defn add-nodes-with-connections
  "Adds to scene the nodes placed by node-coordinate-map and connected via the
  pairs in node-connections, using the current `*backend*`.  Returns the map
  from layout node to created scene node."
  [scene node-coordinate-map node-connections annotation valuation]
  (let [node-map (persistent!
                  (reduce (fn [m [node [x y]]]
                            (assoc! m node (add-node *backend* scene x y node
                                                     (annotation node)
                                                     (valuation node))))
                          (transient {})
                          node-coordinate-map))]
    (doseq [[node-1 node-2] node-connections]
      (add-connection *backend* scene (node-map node-1) (node-map node-2)))
    node-map))

;;;

nil
