;; Copyright ⓒ the conexp-clj developers; all rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.contrib.gui.plugins
  (:use [conexp.base :exclude (select)]
        conexp.contrib.gui.util
        conexp.contrib.gui.plugins.base
        conexp.contrib.gui.plugins.browse)
  (:use seesaw.core))


;;; Plugin Managers for Frames

(let [plugin-managers (atom {})]      ;memory leak possible!

  (defn- save-pm-for-frame
    "Saves given plugin manager pm for frame for later retrival."
    [frame pm]
    (swap! plugin-managers assoc frame pm))

  (defn get-plugin-manager
    "Returns the plugin manager of a given frame."
    [frame]
    (get @plugin-managers frame))

  nil)

(defn add-plugin-manager
  "Adds a plugin-manager and a corresponding menu to frame."
  ;; unfinished
  [frame]
  (let [plugin-manager (make-plugin-manager frame),
        plugin-menu    (menu :text "Plugins"
                             :items [(menu-item :text "Show registered plugins"
                                                :enabled? false)
                                     (menu-item :text "Show loaded plugins"
                                                :enabled? false)
                                     :separator
                                     (menu-item :text "Load plugin"
                                                :enabled? false)
                                     (menu-item :text "Unload plugin"
                                                :enabled? false)
                                     (menu-item :text "Register new plugin"
                                                :enabled? false)
                                     (menu-item :text "Unregister plugin"
                                                :enabled? false)
                                     :separator
                                     (menu-item :text "Browse plugins"
                                                :enabled? false)])]
    (save-pm-for-frame frame plugin-manager)
    (add-menus frame [plugin-menu])))

;;;

nil
