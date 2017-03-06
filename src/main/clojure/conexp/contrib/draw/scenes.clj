;; Copyright ⓒ the conexp-clj developers; all rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.contrib.draw.scenes
  "Namespace for scene abstraction."
  (:use [conexp.base :only (illegal-argument, def-)])
  (:use [seesaw.core :only (listen)])
  (:import [java.awt Color]
           [java.io File]
           [java.awt.image BufferedImage]
           [javax.imageio ImageIO]
           [javax.swing JScrollBar]
           [no.geosoft.cc.graphics GWindow GScene GStyle GWorldExtent GCanvas]))

;; setting custom data

(defn- initialize-scene
  "Initializies given scene."
  [^GScene scn]
  (.setUserData scn (ref {}))
  scn)

(defn add-data-to-scene
  "Adds given data under keyword to scene."
  [^GScene scn, key, data]
  (dosync
   (alter (.getUserData scn) assoc key data)))

(defn update-data-for-scene
  "Updates data item associated with keys in scene."
  [^GScene scn, keys, data]
  (dosync
   (alter (.getUserData scn) assoc-in keys data)))

(defn remove-data-from-scene
  "Removes all data associated with key from scene."
  [^GScene scn, key]
  (dosync
   (alter (.getUserData scn) disj key)))

(defn get-data-from-scene
  "Returns data associated with key from scene."
  [^GScene scn, key]
  (-> scn .getUserData deref (get key)))

(defn redraw-scene
  "Redraws current viewport of scene."
  [^GScene scn]
  (.zoom scn 1.0))

(defn scene-height
  "Returns the height of the given scene."
  [^GScene scn]
  (.getHeight (.getWorldExtent scn)))

(defn scene-width
  "Returns the width of the given scene."
  [^GScene scn]
  (.getWidth (.getWorldExtent scn)))

(defn ^GCanvas scene-canvas
  "Returns the canvas associated with a scene."
  [^GScene scn]
  (.. scn getWindow getCanvas))


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
  [^GScene scn interaction]
  (.. scn getWindow (startInteraction (interaction scn))))

(defn get-zoom-factors
  "Returns zoom factors for height and width of given scene."
  [^GScene scn]
  (let [^GWorldExtent current-world-extent (.getWorldExtent scn),
        ^GWorldExtent initial-world-extent (.getInitialWorldExtent scn)]
    [(/ (.getHeight current-world-extent) (.getHeight initial-world-extent)),
     (/ (.getWidth current-world-extent) (.getWidth initial-world-extent))]))

(defn save-image
  "Saves image on scene scn in given file with given format."
  [^GScene scn, ^File file, ^String format]
  (let [cnv (scene-canvas scn),
        img (BufferedImage. (.getWidth cnv) (.getHeight cnv)
                            BufferedImage/TYPE_INT_RGB)]
    (.paintComponent cnv (.createGraphics img))
    (when-not (ImageIO/write img format file)
      (illegal-argument "Format " format " not supported for saving images."))))

(defn show-labels
  "Turns visibility of labels on scene on and off."
  [^GScene scn, toggle]
  (.setVisibility scn (if toggle
                        GScene/ANNOTATION_VISIBLE
                        GScene/ANNOTATION_INVISIBLE)))

(defn add-scrollbars
  "Adds given scrollbars for scene."
  [^GScene scene, ^JScrollBar horizontal-scrollbar, ^JScrollBar vertical-scrollbar]
  (.installScrollHandler scene horizontal-scrollbar vertical-scrollbar))

;;; coordinate transformers

(defn device-to-world
  "Transforms a device coordinate pair [x y] to a world coordinate pair for the given scene."
  [^GScene scn x y]
  (let [trf (.getTransformer scn)
        ptn (.deviceToWorld trf x y)]
    [(aget ptn 0) (aget ptn 1)]))

(defn world-to-device
  "Transforms a world coordinate pair [x y] of the given scene to a device coordinate pair."
  [^GScene scn x y]
  (let [trf (.getTransformer scn)
        ptn (.worldToDevice trf x y)]
    [(aget ptn 0) (aget ptn 1)]))

(defn origin
  "Returns the origin of scn."
  [scn]
  (world-to-device scn 0 0))

;;;

nil
