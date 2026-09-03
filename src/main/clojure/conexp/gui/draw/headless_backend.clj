;; Copyright ⓒ the conexp-clj developers; all rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.gui.draw.headless-backend
  "A minimal, Swing-free rendering backend that records a lattice diagram as
  plain data instead of drawing it on a canvas.

  Its purpose is twofold: it demonstrates that the drawing code is genuinely
  decoupled from the no.geosoft/Swing backend -- a diagram can be constructed
  and queried entirely through the Backend / AbstractScene / AbstractNode
  protocols without instantiating a single GUI object -- and it provides a
  headless target for capturing computed diagrams as data (e.g. server-side).

  The DataScene / DataNode below implement AbstractScene / AbstractNode with no
  toolkit dependency; only the model state and topology are kept, rendering ops
  are no-ops."
  (:require [conexp.gui.draw.scenes :as scenes]
            [conexp.gui.draw.nodes-and-connections :as nc]
            [conexp.gui.draw.backend :as backend]))

;;; A node or connection: just its model-state ref.

(deftype DataNode [data]
  nc/AbstractNode
  (-node-data [_] data)
  (-redraw-node [_] nil))

;;; A scene: a data ref (hooks/layout), the drawn children, and a world extent.

(deftype DataScene [data children extent]
  scenes/AbstractScene
  (-scene-data [_] data)
  (-redraw [_] nil)
  (-height [_] (nth @extent 3))
  (-width [_] (nth @extent 2))
  (-canvas [_] nil)
  (-start-interaction [_ _] nil)
  (-zoom-factors [_] [1.0 1.0])
  (-save-image [_ _ _] nil)
  (-show-labels [_ _] nil)
  (-add-scrollbars [_ _ _] nil)
  (-device->world [_ x y] [x y])
  (-world->device [_ x y] [x y])
  (-remove-all [_] (reset! children []))
  (-children [_] (seq @children))
  (-set-world-extent [_ x0 y0 w h] (reset! extent [x0 y0 w h]))
  (-unzoom [_] nil))

;;; The backend.

(def headless-backend
  "A Swing-free Backend that builds the diagram as data."
  (reify backend/Backend
    (new-scene [_]
      (let [scene (DataScene. (ref {}) (atom []) (atom [0 0 1 1]))]
        (scenes/add-scene-hook scene :image-changed)
        scene))
    (add-node [_ scene x y name _labels _valuation]
      (let [node (DataNode. (ref {:type :node,
                                  :position [(double x) (double y)],
                                  :radius nc/default-node-radius,
                                  :name name}))]
        (swap! (.children ^DataScene scene) conj node)
        node))
    (add-connection [_ scene lower upper]
      (let [conn (DataNode. (ref {:type :connection,
                                  :lower lower,
                                  :upper upper,
                                  :name (str (nc/get-name lower) " -> " (nc/get-name upper))}))]
        (swap! (.children ^DataScene scene) conj conn)
        (dosync
         (alter (nc/-node-data lower) update-in [:upper] conj conn)
         (alter (nc/-node-data upper) update-in [:lower] conj conn))
        conn))
    (add-grid-point [_ _scene _x _y] nil)
    (install-move-interaction [_ _scene] nil)))

;;;

nil
