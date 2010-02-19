;; Copyright (c) Daniel Borchmann. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.graphics.scenes
  (:use conexp.base
	conexp.graphics.util)
  (:import [java.awt Color]
	   [no.geosoft.cc.graphics GWindow GScene GStyle GWorldExtent]))

(update-ns-meta! conexp.graphics.scenes
 :doc "Namespace for scene abstraction.")


;;; scenes

(defvar- *default-scene-style* (doto (GStyle.)
				 (.setBackgroundColor Color/WHITE)
				 (.setAntialiased true))
  "Default GScene style.")

(defn make-window
  "Creates default window."
  []
  (GWindow. Color/WHITE))

(defn- initialize-scene
  "Initializies given scene."
  [#^GScene scn]
  (.setUserData scn {:hooks {}})
  scn)

(defn make-scene
  "Makes scene on given window."
  [window]
  (let [#^GScene scn (GScene. window)]
    (doto scn
      (initialize-scene)
      (.shouldZoomOnResize true)
      (.shouldWorldExtentFitViewport false)
      (.setStyle *default-scene-style*))
    scn))

(defn redraw-scene
  "Redraws current viewport of scene."
  [#^GScene scn]
  (.zoom scn 1.0))

(defn- get-scene-hooks
  "Returns the hooks with their corresponding callbacks for scene."
  [#^GScene scn]
  (:hooks (.getUserData scn)))

(defn- set-scene-hooks
  "Sets hash-map of hooks to callbacks as scene hooks."
  [#^GScene scn, hooks]
  (.setUserData scn (assoc (.getUserData scn) :hooks hooks)))

(defn add-hook
  "Adds hook for scene."
  [#^GScene scn, hook]
  (when (not (contains? (get-scene-hooks scn) hook))
    (.setUserData scn (assoc-in (.getUserData scn) [:hooks hook] []))))

(defn set-callback-for-hook
  "Sets given functions as callbacks for hook on scene."
  [scn hook functions]
  (when (not (contains? (get-scene-hooks scn) hook))
    (add-hook scn hook))
  (set-scene-hooks scn (assoc (get-scene-hooks scn) hook functions)))

(defn add-callback-for-hook
  "Adds given function as additional callback for hook."
  [scn hook function]
  (set-callback-for-hook scn hook
			 (conj (get (get-scene-hooks scn) hook)
			       function)))

(defn call-hook-with
  "Calls all callbacks of hook with given arguments."
  [scn hook & args]
  (when (not (contains? (get-scene-hooks scn) hook))
    (illegal-argument "Hook " hook " cannot be called for scene."))
  (doseq [callback (get (get-scene-hooks scn) hook)]
    (apply callback args)))

(defn start-interaction
  "Starts a given interaction for scene. interaction must be a
  function from a scene to a GInteraction object."
  [#^GScene scn interaction]
  (.. scn getWindow (startInteraction (interaction scn))))

(defn get-zoom-factors
  "Returns zoom factors for height and width of given scene."
  [#^GScene scn]
  (let [#^GWorldExtent current-world-extent (.getWorldExtent scn),
	#^GWorldExtent initial-world-extent (.getInitialWorldExtent scn)]
    [(/ (.getHeight current-world-extent) (.getHeight initial-world-extent)),
     (/ (.getWidth current-world-extent) (.getWidth initial-world-extent))]))

;;;

nil
