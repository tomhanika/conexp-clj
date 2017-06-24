(ns conexp.gui.plugins
  (:require [conexp.base :refer :all]
            [conexp.gui.plugins.base :refer :all]
            [conexp.gui.plugins.browse :refer :all]
            [conexp.gui.util :refer :all]))

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
