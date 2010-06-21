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

(defn-typecheck add-hook ::hookable
  "Adds a hook to the hooksmap.

  Parameters:
    ohookable   _hookable object
    name        _key for the hook
    function    _a function that will be assigned to the hook
    doc-str     _a documentation string for the hook"
  [ohookable name function doc-str]
  (let [hooks (:hooks ohookable)]
    (dosync (alter hooks conj {name (list function doc-str)}))))

(defn-typecheck set-hook ::hookable
  "Sets a hook in the hooksmap, throws if this hook doesn't exist."
  [ohookable name function]
  (let [hooks (:hooks ohookable)]
    (if (contains? @hooks name)
      (dosync (alter hooks 
                     (fn [h]
                       (let [ doc-str (second (h name))
                              fun-str (list function doc-str)
                              new-h (conj h {name fun-str})]
                         new-h ))))
      (illegal-argument (str "set-hook " name " to " function " for "
                          ohookable " failed: hook undefined")))))

(defn-typecheck call-hook ::hookable
  "Calls a hook in the hooksmap, throws if this hook doesn't exist."
  [ohookable name & args]
  (let [hooks (:hooks ohookable),
        hookmap @hooks]
    (if (contains? hookmap name)
      (apply (first (hookmap name)) args)
      (illegal-argument (str "call-hook " name " for "
                             ohookable " failed: hook undefined"
                             "\n\nmap:\n" hookmap)))))

(defn-typecheck get-hook-function ::hookable
  "Looks up a hook in the hooksmap and returns the associated function,
 throws if this hook doesn't exist."
  [ohookable name]
  (let [hooks (:hooks ohookable),
        hookmap @hooks]
    (if (contains? hookmap name)
      (first (hookmap name))
      (illegal-argument (str "get-hook-function " name " for "
                             ohookable " failed: hook undefined"
                             "\n\nmap:\n" hookmap)))))

(defn-typecheck doc-hook ::hookable
  "Looks up a hook in the hooksmap and returns its doc-str,
   returns :not-found if this hook doesn't exist."
  [ohookable name]
  (let [hooks (:hooks ohookable),
        hookmap @hooks]
    (if (contains? hookmap name)
      (second (hookmap name))
      :not-found)))

(defn make-hookable
  "Creates an empty hookable object."
  []
  (hookable. (ref {})))

(defn hookable?
  "Tests whether the given object is hookable."
  [obj]
  (isa? (class-to-keyword (type obj)) ::hookable))

;;;

nil
