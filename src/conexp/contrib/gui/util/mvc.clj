;; Copyright (c) Daniel Borchmann. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.contrib.gui.util.mvc
  (:use conexp.util))

(update-ns-meta! conexp.contrib.gui.util.mvc
  :doc "Provides an implementation of a very simple
  model-view-controller object.")

;;;

(defn make-mvc
  "Creates an model-view-controller object, where obj will be the
  corresponding model."
  [obj]
  {:model (ref obj), :views (ref #{})})

(defn add-view!
  "Adds a callback (i.e. a view) to a given model-view-controller
  object. The callback must be a function of three arguments: mvc, the
  old value of the model and the new one. Returns an integer which
  denotes this callback and can be used to remove it."
  [mvc callback]
  (let [key (inc (apply max -1 @(:views mvc)))]
    (dosync
     (alter (:views mvc) conj key)
     (add-watch (:model mvc) key
                (fn [_ _ old new]
                  (callback mvc old new))))
    key))

(defn remove-view!
  "Removes callback by given key from the model-view-controller object
  mvc."
  [mvc key]
  (when-not (contains? (:views mvc) key)
    (illegal-argument "Cannot remove a view which has not been added."))
  (dosync
   (alter (:views mvc) disj key)
   (remove-watch (:model mvc) key))
  key)

(defn apply-to-model!
  "Applies given function f to the model of the given
  model-view-controller object mvc with given args as remaining
  arguments. This is the controller part of mvc."
  [mvc f & args]
  (dosync (apply alter (:model mvc) f args)))

;;;

nil
