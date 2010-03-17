;; Copyright (c) Daniel Borchmann. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.util.hookable
  (:use conexp.util))

;; hookable

(deftype hookable [hooks])

(inherit-multimethod add-hook ::hookable
  "Adds a hook to the hooksmap.

  Parameters:
    ohookable   _hookable object
    name        _key for the hook
    function    _a function that will be assigned to the hook")

(defmethod add-hook ::hookable
  [ohookable name function]
  (let [ hooks (:hooks ohookable) ]
    (dosync-wait (commute hooks conj {name function}))))

(inherit-multimethod set-hook ::hookable
  "Sets a hook in the hooksmap, throws if this hook doesn't exist.

  Parameters:
    ohookable   _hookable object
    name        _key for the hook
    function    _a function that will be assigned to the hook")

(defmethod set-hook ::hookable
  [ohookable name function]
  (let [ hooks (:hooks ohookable) ]
    (if (contains? @hooks name)
      (dosync-wait (commute hooks conj {name function}))
      (illegal-argument (str "set-hook " name " to " function " for "
                          ohookable " failed: hook undefined")))))

(inherit-multimethod call-hook ::hookable
  "Calls a hook in the hooksmap, throws if this hook doesn't exist.

  Parameters:
    ohookable   _hookable object
    name        _key for the hook
    & args      _arguments passed to the hook function")

(defmethod call-hook ::hookable
  [ohookable name & args]
  (let [ hooks (:hooks ohookable)
         hookmap @hooks]
    (if (contains? hookmap name)
      (apply (hookmap name) args)
      (illegal-argument (str "call-hook " name " for "
                          ohookable " failed: hook undefined")))))

(defn make-hookable
  "Creates an empty hookable object."
  [] (hookable (ref {})))

(defn hookable?
  "Tests whether the given object is hookable."
  [obj] (isa? (type obj) ::hookable))

