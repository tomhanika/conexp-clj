;; Copyright ⓒ the conexp-clj developers; all rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.contrib.gui.repl-utils
  (:require [conexp.contrib.gui.util :as util]
            [conexp.contrib.gui.editors.context-editor.context-editor :as context-editor])
  (:use conexp.base
        conexp.layouts
        conexp.contrib.draw.lattices))

;;;

(def ^{:dynamic true} *main-frame*
  "Main frame we are operating in."
  nil)

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
  "Returns a vector of tabs, where the index in the vector is the
  index of the corresponding tab."
  []
  (util/get-tabs (get-main-frame)))

(defn get-tab
  "Returns the tab given by its number, starting from 0."
  [n]
  (nth (get-tabs) n))

(defn current-tab
  "Returns the currently selected tab."
  []
  (util/current-tab (get-main-frame)))

(defn remove-tab
  "Removes the given tab, or the current one if none is given."
  ([]
     (remove-tab (current-tab)))
  ([tab]
     (util/remove-tab (get-main-frame) tab)))


;;; Contexts

(defn get-context-from-tab
  "Returns the context in the given tab (or the current tab if none is
  given) if there exists one."
  ([]
     (get-context-from-tab (current-tab)))
  ([tab]
     (context-editor/get-context-from-panel tab)))

(defn show-context-in-tab
  "Opens a new tab with the given context."
  [context]
  (add-tab (context-editor/make-context-editor context)
           "Context"))

(defn set-context-in-tab
  "Sets the context in the given tab (or the current tab) to the given
  context."
  ([tab context]
     (context-editor/set-context-in-panel tab context))
  ([context]
     (set-context-in-tab (current-tab) context)))


;;; Lattices

(defn show-lattice-in-tab
  "Starts lattice editor with given lattice."
  [lattice]
  (add-tab (make-lattice-editor (get-main-frame)
                                (standard-layout lattice))
           "Lattice"))

;;;

nil
