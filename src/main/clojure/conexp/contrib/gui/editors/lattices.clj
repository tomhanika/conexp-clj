;; Copyright ⓒ the conexp-clj developers; all rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.contrib.gui.editors.lattices
  "Provides lattice-editor, a plugin for lattices for the standard GUI of conexp-clj."
  (:use [conexp.base :exclude (select)]
        conexp.fca.contexts
        conexp.fca.lattices
        conexp.io.contexts
        conexp.io.lattices
        conexp.io.layouts
        conexp.layouts
        [conexp.layouts.base :only (lattice)]
        conexp.contrib.draw.lattices
        conexp.contrib.gui.util
        conexp.contrib.gui.editors.context-editor.context-editor
        conexp.contrib.gui.plugins.base)
  (:use seesaw.core)
  (:import [java.io File]))

;;; The Plugin

(declare load-lattice-editor unload-lattice-editor)

(define-plugin lattice-editor
  "Lattice editor plugin."
  :load-hook   #(load-lattice-editor %)
  :unload-hook #(unload-lattice-editor %))

;;; The Actions

(defn- load-lattice-and-go
  "Loads lattice with given loader and adds a new tab with with a
  lattice-editor from the result of tranformer."
  [frame loader transformer]
  (with-swing-error-msg frame "Error"
    (when-let [^File file (choose-open-file frame)]
      (let [thing (loader (.getPath file))]
        (add-tab frame
                 (make-lattice-editor frame
                                      (transformer thing))
                 "Lattice")))))

(defn- save-layout
  "Tries to store the result of applying transformer to the currently
  selected layout into the file the users selects."
  [frame transformer write format]
  (with-swing-error-msg frame "Error"
    (let [layout (get-layout-from-panel (current-tab frame))]
      (if (nil? layout)
        (illegal-argument "Current tab does not contain a lattice editor.")
        (when-let [^File file (choose-save-file frame)]
          (write format
                 (transformer layout)
                 (.getPath file)))))))

(defn- edit-standard-context
  "Opens a context-editor with the standard context of the lattice
  displayed in the current tab of frame."
  [frame]
  (with-swing-error-msg frame "Error"
    (let [layout (get-layout-from-panel (current-tab frame))]
      (if (nil? layout)
        (illegal-argument "Current tab does not contain a lattice editor.")
        (add-tab frame (make-context-editor (standard-context (lattice layout)))
                 "Standard-Context")))))


;;; The Hooks

(defn- lattice-menu
  "Returns the menu of the lattice editor plugin"
  [frame]
  (menu :text "Lattice",
        :items [(menu-item :text "Load Lattice",
                           :listen [:action (fn [_]
                                              (load-lattice-and-go frame
                                                                   read-lattice standard-layout))])
                (menu-item :text "Load Lattice from Context"
                           :listen [:action (fn [_]
                                              (load-lattice-and-go frame
                                                                   read-context
                                                                   (comp standard-layout
                                                                         concept-lattice)))])
                (menu-item :text "Load Layout"
                           :listen [:action (fn [_]
                                              (load-lattice-and-go frame
                                                                   read-layout
                                                                   identity))])
                :separator
                (menu :text "Save Lattice",
                      :items (map (fn [format]
                                    (menu-item :text (str "Format " (name format)),
                                               :listen [:action (fn [_]
                                                                  (save-layout frame
                                                                               lattice
                                                                               write-lattice
                                                                               format))]))
                                  (list-lattice-output-formats))),
                (menu :text "Save Layout",
                      :items (map (fn [format]
                                    (menu-item :text (str "Format " (name format)),
                                               :listen [:action (fn [_]
                                                                  (save-layout frame
                                                                               identity
                                                                               write-layout
                                                                               format))]))
                                  (list-layout-output-formats)))
                :separator
                (menu-item :text "Edit Standard Context",
                           :listen [:action (fn [_]
                                              (edit-standard-context frame))])]))

(let [menu-hash (ref {})]

  (defn- load-lattice-editor
    "Loads the lattice-editor plugin in frame."
    [frame]
    (dosync
     (alter menu-hash
            assoc frame (add-menus frame [(lattice-menu frame)]))))

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
