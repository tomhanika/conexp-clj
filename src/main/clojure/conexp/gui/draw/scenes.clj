;; Copyright ⓒ the conexp-clj developers; all rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.gui.draw.scenes
  "Namespace for scene abstraction.

  A scene is an interactive drawing surface.  Its behaviour is captured by the
  AbstractScene protocol below, which is the seam at which the concrete
  rendering backend is plugged in.  The Swing no.geosoft G-canvas (GScene) is
  currently the only implementation; an alternative renderer (e.g. a web
  canvas) only has to implement AbstractScene to reuse the whole scene layer
  (data storage, hooks, layout handling) built on top of it.

  The public functions in this namespace are backend-agnostic and dispatch
  through the protocol; only make-window / make-scene and the GScene extension
  know about the concrete G-canvas backend."
  (:use [conexp.base :only (illegal-argument, def-)])
  (:use [seesaw.core :only (listen)])
  (:import [java.awt Color]
           [java.io File]
           [java.awt.image BufferedImage]
           [javax.imageio ImageIO]
           [javax.swing JScrollBar]
           [no.geosoft.cc.graphics GWindow GScene GStyle GWorldExtent GCanvas]))

;;; Backend-agnostic scene protocol

(defprotocol AbstractScene
  "Operations a concrete rendering backend must provide so the generic scene
  layer (data storage, hooks, layouts) can be built on top of it. All public
  functions in this namespace dispatch through these methods."
  (-scene-data [scene]
    "Returns the mutable ref holding the scene's non-visual state (hooks,
    stored layout, ...).")
  (-redraw [scene]
    "Redraws the current viewport of the scene.")
  (-height [scene]
    "Returns the world-coordinate height of the scene.")
  (-width [scene]
    "Returns the world-coordinate width of the scene.")
  (-canvas [scene]
    "Returns the underlying rendering canvas component.")
  (-start-interaction [scene interaction]
    "Starts interaction, a function from the scene to a backend interaction
    handler.")
  (-zoom-factors [scene]
    "Returns [height-factor width-factor] relative to the initial extent.")
  (-save-image [scene file format]
    "Saves the rendered scene to file in the given image format.")
  (-show-labels [scene toggle]
    "Turns visibility of the scene's labels on/off.")
  (-add-scrollbars [scene horizontal vertical]
    "Installs the given scrollbars for the scene.")
  (-device->world [scene x y]
    "Transforms a device coordinate pair to a world coordinate pair.")
  (-world->device [scene x y]
    "Transforms a world coordinate pair to a device coordinate pair.")
  (-remove-all [scene]
    "Removes all drawn objects (nodes/connections/grid) from the scene.")
  (-children [scene]
    "Returns the drawn objects currently on the scene.")
  (-set-world-extent [scene x0 y0 width height]
    "Sets the world-coordinate extent of the scene.")
  (-unzoom [scene]
    "Resets the view to the full world extent."))

;;; The Swing no.geosoft G-canvas backend

(extend-type GScene
  AbstractScene
  (-scene-data [scn] (.getUserData scn))
  (-redraw [scn] (.zoom scn 1.0))
  (-height [scn] (.getHeight (.getWorldExtent scn)))
  (-width [scn] (.getWidth (.getWorldExtent scn)))
  (-canvas [scn] (.. scn getWindow getCanvas))
  (-start-interaction [scn interaction]
    (.. scn getWindow (startInteraction (interaction scn))))
  (-zoom-factors [scn]
    (let [^GWorldExtent current-world-extent (.getWorldExtent scn),
          ^GWorldExtent initial-world-extent (.getInitialWorldExtent scn)]
      [(/ (.getHeight current-world-extent) (.getHeight initial-world-extent)),
       (/ (.getWidth current-world-extent) (.getWidth initial-world-extent))]))
  (-save-image [scn ^File file ^String format]
    (let [cnv (-canvas scn),
          img (BufferedImage. (.getWidth cnv) (.getHeight cnv)
                              BufferedImage/TYPE_INT_RGB)]
      (.paintComponent cnv (.createGraphics img))
      (when-not (ImageIO/write img format file)
        (illegal-argument "Format " format " not supported for saving images."))))
  (-show-labels [scn toggle]
    (.setVisibility scn (if toggle
                          GScene/ANNOTATION_VISIBLE
                          GScene/ANNOTATION_INVISIBLE)))
  (-add-scrollbars [scn ^JScrollBar horizontal-scrollbar ^JScrollBar vertical-scrollbar]
    (.installScrollHandler scn horizontal-scrollbar vertical-scrollbar))
  (-device->world [scn x y]
    (let [ptn (.deviceToWorld (.getTransformer scn) x y)]
      [(aget ptn 0) (aget ptn 1)]))
  (-world->device [scn x y]
    (let [ptn (.worldToDevice (.getTransformer scn) x y)]
      [(aget ptn 0) (aget ptn 1)]))
  (-remove-all [scn] (.removeAll scn))
  (-children [scn] (seq (.getChildren scn)))
  (-set-world-extent [scn x0 y0 width height]
    (.setWorldExtent scn (double x0) (double y0) (double width) (double height)))
  (-unzoom [scn] (.unzoom scn)))

;;; setting custom data

(defn add-data-to-scene
  "Adds given data under keyword to scene."
  [scn, key, data]
  (dosync
   (alter (-scene-data scn) assoc key data)))

(defn update-data-for-scene
  "Updates data item associated with keys in scene."
  [scn, keys, data]
  (dosync
   (alter (-scene-data scn) assoc-in keys data)))

(defn remove-data-from-scene
  "Removes all data associated with key from scene."
  [scn, key]
  (dosync
   (alter (-scene-data scn) disj key)))

(defn get-data-from-scene
  "Returns data associated with key from scene."
  [scn, key]
  (-> scn -scene-data deref (get key)))

(defn redraw-scene
  "Redraws current viewport of scene."
  [scn]
  (-redraw scn))

(defn scene-height
  "Returns the height of the given scene."
  [scn]
  (-height scn))

(defn scene-width
  "Returns the width of the given scene."
  [scn]
  (-width scn))

(defn ^GCanvas scene-canvas
  "Returns the canvas associated with a scene."
  [scn]
  (-canvas scn))


;; hooks

(defn- get-scene-hooks
  "Returns the hooks with their corresponding callbacks for scene."
  [scn]
  (get-data-from-scene scn :hooks))

(defn- set-scene-hooks
  "Sets hash-map of hooks to callbacks as scene hooks."
  [scn, hooks]
  (add-data-to-scene scn :hooks hooks))

(defn add-scene-hook
  "Adds hook for scene."
  [scn, hook]
  (when (not (contains? (get-scene-hooks scn) hook))
    (update-data-for-scene scn [:hooks hook] [])))

(defn set-scene-callback
  "Sets given functions as callbacks for hook on scene."
  [scn hook functions]
  (when (not (contains? (get-scene-hooks scn) hook))
    (add-scene-hook scn hook))
  (set-scene-hooks scn (assoc (get-scene-hooks scn) hook functions)))

(defn add-scene-callback
  "Adds given function as additional callback for hook."
  [scn hook function]
  (set-scene-callback scn hook
                      (conj (get (get-scene-hooks scn) hook)
                            function)))

(defn remove-scene-callback
  "Removes a given function from a given hook."
  [scn hook function]
  (set-scene-callback scn hook
                      (remove #{function}
                              (get (get-scene-hooks scn) hook))))

(defn call-scene-hook
  "Calls all callbacks of hook with given arguments. Every hook is
  called in a thread-safe manner."
  [scn hook & args]
  (when (not (contains? (get-scene-hooks scn) hook))
    (illegal-argument "Hook " hook " cannot be called for scene."))
  (doseq [callback (get (get-scene-hooks scn) hook)]
    (apply callback args)))

;; scene constructor

(defn- initialize-scene
  "Initializies given scene."
  [^GScene scn]
  (.setUserData scn (ref {}))
  scn)

(def- default-scene-style
  "Default GScene style."
  (doto (GStyle.)
    (.setBackgroundColor Color/WHITE)
    (.setAntialiased true)))

(defn ^GWindow make-window
  "Creates default window."
  []
  (GWindow. Color/WHITE))

(defn ^GScene make-scene
  "Makes scene on given window."
  [window]
  (let [^GScene scn (GScene. window)]
    (doto scn
      (initialize-scene)
      (.shouldZoomOnResize true)
      (.shouldWorldExtentFitViewport false)
      (.setStyle default-scene-style)
      (add-scene-hook :image-changed))
    (listen (scene-canvas scn) :component-resized
            (fn [_]
              (call-scene-hook scn :image-changed)))
    scn))

;; methods on scenes

(defn start-interaction
  "Starts a given interaction for scene. interaction must be a
  function from a scene to a GInteraction object."
  [scn interaction]
  (-start-interaction scn interaction))

(defn get-zoom-factors
  "Returns zoom factors for height and width of given scene."
  [scn]
  (-zoom-factors scn))

(defn save-image
  "Saves image on scene scn in given file with given format."
  [scn, ^File file, ^String format]
  (-save-image scn file format))

(defn show-labels
  "Turns visibility of labels on scene on and off."
  [scn, toggle]
  (-show-labels scn toggle))

(defn add-scrollbars
  "Adds given scrollbars for scene."
  [scn, ^JScrollBar horizontal-scrollbar, ^JScrollBar vertical-scrollbar]
  (-add-scrollbars scn horizontal-scrollbar vertical-scrollbar))

;;; coordinate transformers

(defn device-to-world
  "Transforms a device coordinate pair [x y] to a world coordinate pair for the given scene."
  [scn x y]
  (-device->world scn x y))

(defn world-to-device
  "Transforms a world coordinate pair [x y] of the given scene to a device coordinate pair."
  [scn x y]
  (-world->device scn x y))

(defn origin
  "Returns the origin of scn."
  [scn]
  (world-to-device scn 0 0))

;;; scene manipulation

(defn remove-all-from-scene
  "Removes all drawn objects from the scene."
  [scn]
  (-remove-all scn))

(defn scene-children
  "Returns the drawn objects currently on the scene."
  [scn]
  (-children scn))

(defn set-world-extent
  "Sets the world-coordinate extent of scn to origin [x0 y0] with the given
  width and height."
  [scn x0 y0 width height]
  (-set-world-extent scn x0 y0 width height))

(defn unzoom-scene
  "Resets the scene view to the full world extent."
  [scn]
  (-unzoom scn))

;;;

nil
