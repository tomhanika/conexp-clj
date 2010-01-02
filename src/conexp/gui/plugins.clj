(ns conexp.gui.plugins
  (:use conexp.util
	conexp.gui.util
	conexp.gui.plugins.base
	conexp.gui.plugins.browse))


;;; Plugin Managers for Frames

;; a hack (do I like this?)
(let [*plugin-managers* (atom {})]
  (defn- save-pm-for-frame
    "Saves given plugin manager pm for frame for later retrival."
    [frame pm]
    (swap! *plugin-managers* assoc frame pm))

  (defn get-plugin-manager
    "Returns the plugin manager of a given frame."
    [frame]
    (get @*plugin-managers* frame)))

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
