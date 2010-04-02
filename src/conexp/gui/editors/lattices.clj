;; Copyright (c) Daniel Borchmann. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.gui.editors.lattices
  (:use conexp.base
	conexp.fca.contexts
	conexp.fca.lattices
	conexp.io
	conexp.layout
	conexp.graphics.draw
	conexp.gui.util
	conexp.gui.plugins.base)
  (:use clojure.contrib.swing-utils)
  (:import [java.io File]))

(update-ns-meta! conexp.gui.editors.lattices
  :doc "Provides lattice-editor, a plugin for lattices for the standard GUI of conexp-clj.")

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
  (do-swing-and-wait
   (when-let [#^File file (choose-open-file frame)]
     (let [thing (loader (.getPath file))]
       (add-tab frame
		(make-lattice-editor frame
				     (transformer thing))
		"Lattice")))))

(defn- load-lattice-and-draw
  "Asks the user for a file to load a lattice from and displays it in
  the lattice editor."
  [frame menu-item]
  (load-lattice-and-go frame read-lattice *standard-layout-function*))

(defn- load-layout-and-draw
  "Asks the user for a file to load a layout from and displays it."
  [frame menu-item]
  (load-lattice-and-go frame read-layout identity))

(defn- load-context-and-draw
  "Asks the user for a file to load a context from and displays the
  corresponding concept lattice."
  [frame menu-item]
  (load-lattice-and-go frame read-context (comp *standard-layout-function* concept-lattice)))

;;; The Hooks

(defvar- *lattice-menu*
  {:name "Lattice",
   :content [{:name "Load Lattice",
	      :handler load-lattice-and-draw}
	     {:name "Load Layout"
	      :handler load-layout-and-draw}
	     {:name "Load Context"
	      :handler load-context-and-draw}
	     {}
	     {:name "Save Lattice"}
	     {:name "Save Layout"}
	     {}
	     {:name "Edit Standard Context"}]}
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
