;; Copyright (c) Daniel Borchmann. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.gui.plugins
  (:use conexp.util
	conexp.gui.util
	conexp.gui.plugins.base
	conexp.gui.plugins.browse))


;;; Plugin Managers for Frames

(let [*plugin-managers* (atom {})]
  (defn- save-pm-for-frame
    "Saves given plugin manager pm for frame for later retrival."
    [frame pm]
    (swap! *plugin-managers* assoc frame pm))

  (defn get-plugin-manager
    "Returns the plugin manager of a given frame."
    [frame]
    (get @*plugin-managers* frame))

  nil)

(defn add-plugin-manager
  "Adds a plugin-manager and a corresponding menu to frame."
  ;; unfinished
  [frame]
  (let [plugin-manager (make-plugin-manager frame),
	plugin-menu {:name "Plugins",
		     :content [{:name "Show registered plugins"}
			       {:name "Show loaded plugins"}
			       ---
			       {:name "Load plugin"}
			       {:name "Unload plugin"}
			       {:name "Register new plugin"}
			       {:name "Unregister plugin"}
			       ---
			       {:name "Browse plugins"}]}]
    (save-pm-for-frame frame plugin-manager)
    (add-menus frame [plugin-menu])))

;;;

nil
