;; Copyright ⓒ the conexp-clj developers; all rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.contrib.draw.scene-layouts
  "Basic namespace for drawing lattice."
  (:use [conexp.base :only (illegal-argument, reduce!)]
        [conexp.layouts.base :only (positions, connections,
                                    nodes, update-positions, annotation)]
        [conexp.layouts.util :only (enclosing-rectangle)]
        conexp.contrib.draw.nodes-and-connections
        conexp.contrib.draw.scenes
        conexp.contrib.gui.util)
  (:import [javax.swing JFrame JButton JPanel JLabel]
           [java.awt Dimension BorderLayout Color]
           [no.geosoft.cc.graphics GWindow GScene GStyle]))

;;; get diagram from scene

(defn get-diagram-from-scene
  "Returns nodes and lines of a scene."
  [^GScene scene]
  (seq (.getChildren scene)))

;;; node and line iterators

(defmacro do-nodes
  "Do whatever with every node on the scene. Redraws the scene
  afterwards."
  [[node scene] & body]
  `(do
     (doseq [~node (filter node? (get-diagram-from-scene ~scene))]
       ~@body)
     (redraw-scene ~scene)))

(defmacro do-lines
  "Do whatever with every connection on the scene. Redraws the scene
  afterwards."
  [[line scene] & body]
  `(do
     (doseq [~line (filter connection? (get-diagram-from-scene ~scene))]
       ~@body)
     (redraw-scene)))

;;; manipulate layout of scene

(defn get-layout-from-scene
  "Returns layout from a scene."
  [scn]
  (update-positions (get-data-from-scene scn :layout)
                    (reduce! (fn [hash node]
                              (assoc! hash (get-name node) (position node)))
                             {}
                             (filter node? (get-diagram-from-scene scn)))))

(defn fit-scene-to-layout
  "Adjusts scene such that layout fits on it. Uses stored layout if
  none is given. Calls :image-changed hook."
  ([^GScene scene]
     (fit-scene-to-layout scene (get-layout-from-scene scene)))
  ([^GScene scene, layout]
     (let [[x_min y_min x_max y_max] (enclosing-rectangle (vals (positions layout))),
           width  (- x_max x_min),
           height (- y_max y_min),
           width  (if (zero? width)
                    1
                    width),
           height (if (zero? height)
                    1
                    height)]
       (.setWorldExtent scene
                        (double (- x_min (* 0.05 width)))
                        (double (- y_min (* 0.05 height)))
                        (double (* 1.10 width))
                        (double (* 1.10 height)))
       (.unzoom scene)
       (call-scene-hook scene :image-changed))))

(defn update-layout-of-scene
  "Updates layout according to new layout. The underlying lattice must
  not be changed."
  [^GScene scene, layout]
  (let [pos (positions layout)]
    (do-nodes [node scene]
      (let [[x y] (pos (get-name node))]
        (move-node-unchecked-to node x y)))))

(defn set-layout-of-scene
  "Sets given layout as current layout of scene."
  [^GScene scene, layout]
  (doto scene
    (.removeAll)
    (add-nodes-with-connections (positions layout) (connections layout) (annotation layout))
    (add-data-to-scene :layout layout)))


;;; draw nodes with coordinates and connections on a scene

(defn ^GScene draw-on-scene
  "Draws given layout on a GScene and returns it."
  [layout]
  (let [wnd (make-window),
        scn (make-scene wnd)]
    (doto scn
      (set-layout-of-scene layout)
      (fit-scene-to-layout layout))
    (doto wnd
      (.startInteraction (move-interaction scn)))
    scn))


;;;

nil
