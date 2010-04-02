;; Copyright (c) Daniel Borchmann. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.gui.editors.lattices
  (:use conexp.base
	conexp.gui.util
	conexp.gui.plugins.base))

(update-ns-meta! conexp.gui.editors.lattices
  :doc "Provides lattice-editor, a plugin for lattices for the standard GUI of conexp-clj.")

;;; The Plugin

(declare load-lattice-editor unload-lattice-editor)

(define-plugin lattice-editor
  "Lattice editor plugin."
  :load-hook   #(load-lattice-editor %),
  :unload-hook #(unload-lattice-editor %))

;;; The Hooks

(defvar- *lattice-menu*
  {:name "Lattice",
   :content [{:name "Load Lattice"}
	     {:name "Load Layout"}
	     {:name "Load Context"}
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
