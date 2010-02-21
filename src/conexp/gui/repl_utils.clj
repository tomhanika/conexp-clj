(ns conexp.gui.repl-utils
  (:require [conexp.gui.util :as util])
  (:use conexp))

;;;

(defn- get-main-frame
  "Returns current main-frame or nil."
  []
  (eval 'conexp/*main-frame*))

;; Tabs

(defn add-tab
  "Adds given panel to tabpane of current frame."
  [panel name]
  (util/add-tab (get-main-frame) panel name))

(defn get-tabs
  "Returns hashmap of indices to tab contents."
  []
  (util/get-tabs (get-main-frame)))


;; Stuff

(defn start-lattice-editor
  "Starts lattice editor with given lattice."
  [lattice]
  (add-tab (make-lattice-editor (get-main-frame) lattice *standard-layout-function*)
	   "Lattice"))

;;;

nil
