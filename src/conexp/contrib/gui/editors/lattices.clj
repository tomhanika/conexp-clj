;; Copyright (c) Daniel Borchmann. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.contrib.gui.editors.lattices
  (:use conexp.base
	conexp.fca.contexts
	conexp.fca.lattices
	conexp.io
	conexp.layout
	[conexp.layout.base :only (lattice)]
	conexp.graphics.draw
	conexp.contrib.gui.util
	conexp.contrib.gui.plugins.base)
  (:use clojure.contrib.swing-utils)
  (:import [java.io File]))

(ns-doc
 "Provides lattice-editor, a plugin for lattices for the standard GUI
 of conexp-clj.")

;;; The Plugin

(declare load-lattice-editor unload-lattice-editor)

(define-plugin lattice-editor
  "Lattice editor plugin."
  :load-hook   #(load-lattice-editor %),
  :unload-hook #(unload-lattice-editor %))

;;; The Actions

(defn- load-lattice-and-go
  "Loads lattice with given loader and adds a new tab with with a
  lattice-editor from the result of tranformer."
  [frame loader transformer]
  (when-let [#^File file (choose-open-file frame)]
    (let [thing (loader (.getPath file))]
      (add-tab frame
	       (make-lattice-editor frame
				    (transformer thing))
	       "Lattice"))))

(defn- save-layout
  "Tries to store the result of applying transformer to the currently
  selected layout into the file the users selects."
  [frame transformer write format]
  (let [layout (get-layout-from-panel (current-tab frame))]
    (if (nil? layout)
      (illegal-argument "Current tab does not contain a lattice editor.")
      (write format
	     (transformer layout)
	     (.getPath (choose-save-file frame))))))

(defn- edit-standard-context
  "Opens a context-editor with the standard context of the lattice
  displayed in the current tab of frame."
  [frame]
  (unsupported-operation "Not yet implemented."))

;;; The Hooks

(defvar- *lattice-menu*
  {:name "Lattice",
   :content [{:name "Load Lattice",
	      :handler #(load-lattice-and-go % read-lattice *standard-layout-function*)}
	     {:name "Load Lattice from Context"
	      :handler #(load-lattice-and-go % read-context
					     (comp *standard-layout-function* concept-lattice))}
	     {:name "Load Layout"
	      :handler #(load-lattice-and-go % read-layout identity)}
	     {}
	     {:name "Save Lattice",
	      :content [{:name "Format conexp-clj simple",
			 :handler #(save-layout % lattice write-lattice :simple)}]}
	     {:name "Save Layout",
	      :content [{:name "Format conexp-clj simple",
			 :handler #(save-layout % identity write-layout :simple)}]}
	     {}
	     {:name "Edit Standard Context",
	      :handler edit-standard-context}]}
  "Menu for lattice editor.")

(let [menu-hash (ref {})]

  (defn- load-lattice-editor
    "Loads the lattice-editor plugin in frame."
    [frame]
    (dosync
     (alter menu-hash
	    assoc frame (add-menus frame [*lattice-menu*]))))

  (defn- unload-lattice-editor
    "Unloads the lattice-editor plugin from frame."
    [frame]
    (dosync
     (let [menu (get @menu-hash frame)]
       (remove-menus frame [menu])
       (alter menu-hash dissoc frame))))

  nil)

;;; The End

nil
