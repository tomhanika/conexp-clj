(ns conexp.gui.plugins
  (:use conexp.util))

(defstruct plugin
  :load-hook
  :unload-hook)

(def load-hook (accessor plugin :load-hook))
(def unload-hook (accessor plugin :unload-hook))

(defmacro define-plugin
  "Defines a plugin. The syntax is of the form

  (define-plugin plugin
    :load-hook fn-called-when-plugin-is-loaded
    :unload-hook fn-called-when-plugin-is-unloaded)

  whereas the :unload-hook is optional. All functions take the frame
  the plugin-manager operates on as single argument"
  [name & hooks]
  (let [hooks (apply hash-map hooks)]
    (if (not (contains? hooks :load-hook))
      (illegal-argument "Plugin has to provide a load-hook."))
    `(def ~name (struct plugin ~(:load-hook hooks) ~(:unload-hook hooks)))))

(defstruct plugin-manager
  :base-frame
  :registered-plugins
  :loaded-plugins)

(def base-frame (accessor plugin-manager :base-frame))
(def registered-plugins (accessor plugin-manager :registered-plugins))
(def loaded-plugins (accessor plugin-manager :loaded-plugins))

(defn make-plugin-manager
  "Creates a plugin-manager for frame."
  [frame]
  (struct plugin-manager frame (ref #{}) (ref #{})))

(defn plugin-registered? [plugin-manager plugin]
  (contains? (registered-plugins plugin-manager) plugin))

(defn plugin-loaded?
  "Tests whether a plugin is loaded within a plugin manager or not."
  [plugin-manager plugin]
  (and (plugin-registered? plugin-manager plugin)
       (contains? (loaded-plugins plugin-manager) plugin)))

(defn register-plugin
  "Tests whether a plugin is registered within a plugin manager or not."
  [plugin-manager plugin]
  (dosync
   (alter (registered-plugins) conj plugin)))

(defn load-plugin
  "Loads a registered plugin from a plugin manager."
  [plugin-manager plugin]
  (when (not (plugin-registered? plugin-manager plugin))
    (illegal-argument "Plugin not registered and hence cannot be loaded."))
  (when (plugin-loaded? plugin-manager plugin)
    (illegal-argument "Plugin already be loaded."))
  (dosync
   (alter (loaded-plugins) conj plugin))
  ((load-hook plugin) (base-frame plugin-manager)))

(defn unload-plugin
  "Unloads a plugin from a plugin manager."
  [plugin-manager plugin]
  (when (not (plugin-registered? plugin-manager plugin))
    (illegal-argument "Plugin not registered and hence cannot be unloaded."))
  (when (not (plugin-loaded? plugin-manager plugin))
    (illegal-argument "Plugin not loaded and hence cannot be unloaded."))
  (let [unload-hook (unload-hook plugin)]
    (when (nil? unload-hook)
      (illegal-argument "Plugin didn't provide an unload-hook and can therefore not be unloaded."))
    (dosync
     (alter (loaded-plugins plugin-manager) disj plugin))
    (unload-hook (base-frame plugin-manager))))

(defn unregister-plugin
  "Unregisters a plugin from a plugin manager."
  [plugin-manager plugin]
  (when (plugin-loaded? plugin-manager plugin)
    (unload-plugin plugin-manager plugin))
  (dosync
   (alter (registered-plugins plugin-manager) disj plugin)))


;; TODO:
;; searching for plugins in files and directories
;; graphical plugin-browser (extra namespace)

nil
