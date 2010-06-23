;; Copyright (c) Daniel Borchmann. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

;; This file has been written by Immanuel Albrecht, with modifications by DB

(ns conexp.contrib.gui.util.hookable
  (:use conexp.base
        conexp.contrib.gui.util))

;; hookable

(defrecord hookable [hooks])

(defn make-hookable
  "Creates an empty hookable object."
  []
  (hookable. (ref {})))

(defn hookable?
  "Tests whether the given object is hookable."
  [obj]
  (keyword-isa? obj ::hookable))

(defn add-hook
  "Adds a hook to the hooksmap.

  Parameters:
    ohookable   _hookable object
    name        _key for the hook
    function    _a function that will be assigned to the hook"
  [ohookable name function]
  (assert (hookable? ohookable))
  (let [hooks (:hooks ohookable)]
    (dosync (alter hooks conj {name (list function "-no-doc-str-")}))))

(defn set-hook
  "Sets a hook in the hooksmap, throws if this hook doesn't exist."
  [ohookable name function]
  (assert (hookable? ohookable))
  (let [hooks (:hooks ohookable)]
    (if (contains? @hooks name)
      (dosync (alter hooks 
                     (fn [h]
                       (let [doc-str (second (h name))
                             fun-str (list function doc-str)
                             new-h   (conj h {name fun-str})]
                         new-h))))
      (illegal-argument (str "set-hook " name " to " function " for "
                             ohookable " failed: hook undefined")))))

(defn call-hook
  "Calls a hook in the hooksmap, throws if this hook doesn't exist."
  [ohookable name & args]
  (assert (hookable? ohookable))
  (let [hooks (:hooks ohookable),
        hookmap @hooks]
    (if (contains? hookmap name)
      (apply (first (hookmap name)) args)
      (illegal-argument (str "call-hook " name " for "
                             ohookable " failed: hook undefined"
                             "\n\nmap:\n" hookmap)))))

(defn get-hook-function
  "Looks up a hook in the hooksmap and returns the associated function,
 throws if this hook doesn't exist."
  [ohookable name]
  (assert (hookable? ohookable))
  (let [hooks (:hooks ohookable),
        hookmap @hooks]
    (if (contains? hookmap name)
      (first (hookmap name))
      (illegal-argument (str "get-hook-function " name " for "
                             ohookable " failed: hook undefined"
                             "\n\nmap:\n" hookmap)))))

(defn doc-hook
  "Looks up a hook in the hooksmap and returns its doc-str,
   returns :not-found if this hook doesn't exist."
  [ohookable name]
  (assert (hookable? ohookable))
  (let [hooks (:hooks ohookable),
        hookmap @hooks]
    (if (contains? hookmap name)
      (second (hookmap name))
      :not-found)))

;;;

nil
