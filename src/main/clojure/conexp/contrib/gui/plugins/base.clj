;; Copyright ⓒ the conexp-clj developers; all rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.contrib.gui.plugins.base
  "Implements plugin frameworks used for conexp-clj's standard GUI."
  (:use [conexp.base :only (illegal-argument)]))

;;; Basic Plugin Structure

(defstruct plugin
  :load-hook
  :unload-hook)

(def load-hook (accessor plugin :load-hook))
(def unload-hook (accessor plugin :unload-hook))

(defmacro define-plugin
  "Defines a plugin. The syntax is of the form

  (define-plugin plugin
    :load-hook fn-called-when-plugin-is-loaded,
    :unload-hook fn-called-when-plugin-is-unloaded)

  whereas the :unload-hook is optional. All functions take the frame
  the plugin-manager operates on as single argument"
  [name description & hooks]
  (let [hooks (apply hash-map hooks)]
    (if (not (contains? hooks :load-hook))
      (illegal-argument "Plugin has to provide a load-hook."))
    `(def ~name (with-meta
                  (struct plugin ~(:load-hook hooks) ~(:unload-hook hooks))
                  {:conexp-gui-plugin true,
                   :description ~description}))))


;;; Plugin Manager for managing plugins

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
  (contains? @(registered-plugins plugin-manager) plugin))

(defn plugin-loaded?
  "Tests whether a plugin is loaded within a plugin manager or not."
  [plugin-manager plugin]
  (and (plugin-registered? plugin-manager plugin)
       (contains? @(loaded-plugins plugin-manager) plugin)))

(defn register-plugin
  "Tests whether a plugin is registered within a plugin manager or not."
  [plugin-manager plugin]
  (dosync
   (alter (registered-plugins plugin-manager) conj plugin)))

(defn load-plugin
  "Loads a registered plugin from a plugin manager."
  [plugin-manager plugin]
  (when (not (plugin-registered? plugin-manager plugin))
    (register-plugin plugin-manager plugin))
  (when (plugin-loaded? plugin-manager plugin)
    (illegal-argument "Plugin already be loaded."))
  (dosync
   (alter (loaded-plugins plugin-manager) conj plugin))
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


;;; Searching for Plugins in Namespaces

(defn plugins-from-namespace
  "Returns all plugins defined in namespace ns."
  [ns]
  (filter (fn [var]
            (try
             (:conexp-gui-plugin (meta (deref var)))
             (catch Exception _ false)))
          (map second
               (ns-publics (find-ns ns)))))

(defn plugins-from-loaded-libs
  "Returns all plugins defined in all loaded libraries."
  []
  (for [ns (loaded-libs),
        plugins (plugins-from-namespace ns)]
    plugins))

(defn plugins-from-file
  "Returns all plugins defined in the file named by file-name."
  [^String file-name]
  (let [absolute-path (.getAbsolutePath (java.io.File. file-name))]
    (load-file absolute-path)
    (filter #(= (:file (meta %)) absolute-path)
            (plugins-from-loaded-libs))))

;;;

nil
