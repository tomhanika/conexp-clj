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
        conexp.layouts
        [conexp.layouts.base :only (lattice)]
        conexp.contrib.draw.lattices
        conexp.contrib.gui.util
        conexp.contrib.gui.editors.context-editor.context-editor
        conexp.contrib.gui.plugins.base)
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
  (when-let [^File file (choose-open-file frame)]
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
      (when-let [^File file (choose-save-file frame)]
        (write format
               (transformer layout)
               (.getPath file))))))

(defn- edit-standard-context
  "Opens a context-editor with the standard context of the lattice
  displayed in the current tab of frame."
  [frame]
  (let [layout (get-layout-from-panel (current-tab frame))]
    (if (nil? layout)
      (illegal-argument "Current tab does not contain a lattice editor.")
      (add-tab frame (make-context-editor (standard-context (lattice layout)))
        "Standard-Context"))))


;;; The Hooks

(defvar- lattice-menu
  {:name "Lattice",
   :content [{:name "Load Lattice",
              :handler #(load-lattice-and-go % read-lattice standard-layout)}
             {:name "Load Lattice from Context"
              :handler #(load-lattice-and-go % read-context
                                             (comp standard-layout concept-lattice))}
             {:name "Load Layout"
              :handler #(load-lattice-and-go % read-layout identity)}
             {}
             {:name "Save Lattice",
              :content (map (fn [format]
                              (hash-map :name (str "Format " (name format)),
                                        :handler #(save-layout % lattice write-lattice format)))
                            (list-lattice-output-formats))},
             {:name "Save Layout",
              :content (map (fn [format]
                              (hash-map :name (str "Format " (name format)),
                                        :handler #(save-layout % identity write-layout format)))
                            (list-layout-output-formats))}
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
            assoc frame (add-menus frame [lattice-menu]))))

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
