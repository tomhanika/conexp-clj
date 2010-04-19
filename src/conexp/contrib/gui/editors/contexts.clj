;; Copyright (c) Daniel Borchmann. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.contrib.gui.editors.contexts
  (:use conexp.base
	conexp.fca.contexts
	conexp.fca.lattices
        conexp.io
	conexp.layout
	[conexp.layout.base :only (lattice)]
	conexp.graphics.draw
	conexp.contrib.gui.util
        conexp.contrib.gui.editors.util
        conexp.contrib.gui.editors.context-editor
	conexp.contrib.gui.plugins.base)
  (:use clojure.contrib.swing-utils
	clojure.contrib.io
    [clojure.contrib.string :only (replace-str title-case)])
  (:import [java.io File]))

(update-ns-meta! conexp.contrib.gui.editors.contexts
  :doc "Provides context-editor, a plugin for contexts for the standard GUI of conexp-clj.")

;;; The Plugin

(declare load-context-editor unload-context-editor)

(define-plugin context-editor
  "Context editor plugin."
  :load-hook   #(load-context-editor %),
  :unload-hook #(unload-context-editor %))

;;; The Actions

(defn- load-context-and-go
  "Loads context with given loader and adds a new tab with a context-editor."
  [frame loader]
  (when-let [#^File file (choose-open-file frame)]
    (let [ path (.getPath file)
           thing (loader path)]
      (add-tab frame
	       (make-context-editor thing)
	       (str "Context " path)))))

(defn- save-context-and-go
  "Saves context with given writer."
  [frame writer]
  (when-let [ thing (get-context-from-panel (current-tab frame))]
    (when-let [#^File file (choose-save-file frame)]
      (let [ path (.getPath file) ]
        (writer thing path)))))
  


;;; The Hooks

(defvar- *context-menu*
  {:name "Context",
  :content [ {:name "Load Context",
	      :handler (fn [x] (load-context-and-go x read-context)) }
              {:name "Save Context",
              :content (vec (map (fn [x] {:name (str 
                                                  (replace-str ":" "" (str x)) 
                                                  " format"),
                                   :handler (fn [f] (save-context-and-go f
                                           (fn [c p] (write-context x c p))))})
                              (list-context-input-formats))) }]}
  "Menu for context editor.")

(let [menu-hash (ref {})]

  (defn- load-context-editor
    "Loads the context-editor plugin in frame."
    [frame]
    (dosync
     (alter menu-hash
	    assoc frame (add-menus frame [*context-menu*]))))

  (defn- unload-context-editor
    "Unloads the context-editor plugin from frame."
    [frame]
    (dosync
     (let [menu (get @menu-hash frame)]
       (remove-menus frame [menu])
       (alter menu-hash dissoc frame))))

  nil)

;;; The End

nil
