;; Copyright (c) Daniel Borchmann. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

;; This file has been written by Immanuel Albrecht, with modifications by DB

(ns conexp.contrib.gui.editors.contexts
  (:use conexp.base
	conexp.fca.contexts
	conexp.fca.lattices
        conexp.io
	conexp.layout
	[conexp.layout.base :only (lattice)]
	conexp.contrib.draw.lattices
	conexp.contrib.gui.util
        conexp.contrib.gui.editors.context-editor.context-editor
	conexp.contrib.gui.plugins.base)
  (:use clojure.contrib.swing-utils
	[clojure.contrib.io :exclude (spit)]
        [clojure.contrib.string :only (replace-str)])
  (:import [java.io File]))

(ns-doc
 "Provides context-editor, a plugin for contexts for the standard GUI
  of conexp-clj.")

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
    (let [path (.getPath file),
          thing (loader path)]
      (add-tab frame
	       (make-context-editor thing)
	       (str "Context " path)))))

(defn- clone-context-and-go
  "Loads context with given loader and adds a new tab with a context-editor."
  [frame]
  (add-tab frame
    (clone-context-view-from-panel (current-tab frame))
    (str (current-tab-title frame) "*")))
  

(defn- random-context-and-go
  "Creates a random context and adds a new tab with a context-editor."
  [frame]
  (let [thing (rand-context #{"a" "b" "c" "d" "e" "f"} #{1 2 3 4 5 6} 0.4)]
    (add-tab frame
             (make-context-editor thing)
             "Context")))

(defn- second-op-context-and-go
  "Show the current second operand context in a new tab."
  [frame]
  (let [thing (get-current-second-operand-context)]
    (add-tab frame
             (make-context-editor thing)
             "Context")))

(defn- save-context-and-go
  "Saves context with given writer."
  [frame writer]
  (when-let [thing (get-context-from-panel (current-tab frame))]
    (when-let [#^File file (choose-save-file frame)]
      (let [path (.getPath file)]
        (writer thing path)))))
  
(defn- show-lattice-and-go
  "Shows concept lattice of current tab."
  [frame]
  (let [thing (get-context-from-panel (current-tab frame))]
    (add-tab frame
             (make-lattice-editor frame
                                  (*standard-layout-function* (concept-lattice thing)))
             "Concept-Lattice")))

;;; The Hooks

(defvar- *context-menu*
  {:name "Context",
   :content [{:name "Load Context",
	      :handler #(load-context-and-go % read-context)},
             {:name "Random Context",
              :handler random-context-and-go},
             {:name "Second Operand Context",
              :handler second-op-context-and-go},
             {}
             {:name "Save Context",
              :content (vec (map (fn [format]
                                   {:name (str (replace-str ":" "" (str format))
                                               " format"),
                                    :handler (fn [frame]
                                               (save-context-and-go frame
                                                                    (fn [ctx path]
                                                                      (write-context format ctx path))))})
                                 (list-context-formats)))},
             {:name "Clone Current Context View"
              :handler clone-context-and-go},
             {}
             {:name "Show Concept Lattice",
              :handler show-lattice-and-go}]}
  "Menu for context editor.")

(let [menu-hash (atom {})]

  (defn- load-context-editor
    "Loads the context-editor plugin in frame."
    [frame]
    (swap! menu-hash
           assoc frame (add-menus frame [*context-menu*])))

  (defn- unload-context-editor
    "Unloads the context-editor plugin from frame."
    [frame]
    (let [menu (get @menu-hash frame)]
      (remove-menus frame [menu])
      (swap! menu-hash dissoc frame)))

  nil)

;;; The End

nil
