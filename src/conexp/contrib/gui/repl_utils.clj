;; Copyright (c) Daniel Borchmann. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.contrib.gui.repl-utils
  (:require [conexp.contrib.gui.util :as util])
  (:use conexp.base
	conexp.layout
	conexp.contrib.draw.lattice-drawer))

;;;

(defvar *main-frame* nil
  "Main frame we are operatin in.")

(defn- get-main-frame
  "Returns current main-frame or nil."
  []
  (when (nil? *main-frame*)
    (illegal-state "Function should be called in the GUI REPL only."))
  *main-frame*)

;;; Tabs

(defn add-tab
  "Adds given panel to tabpane of current frame."
  [panel name]
  (util/add-tab (get-main-frame) panel name))

(defn get-tabs
  "Returns hashmap of indices to tab contents."
  []
  (util/get-tabs (get-main-frame)))

(defn current-tab
  "Returns the currently selected tab."
  []
  (util/current-tab (get-main-frame)))

;; remove-tab


;;; Stuff

(defn start-lattice-editor
  "Starts lattice editor with given lattice."
  [lattice]
  (add-tab (make-lattice-editor (get-main-frame)
				(*standard-layout-function* lattice))
	   "Lattice"))

;;;

nil
