;; Copyright (c) Daniel Borchmann. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

;; This file has been written by Immanuel Albrecht, with modifications by DB

(ns conexp.contrib.gui.util.one-to-many
  (:use conexp.contrib.gui.util))

;; one-to-many abstraction

(defrecord one-to-many [one many])

(defn make-one-to-many
  "Creates a one-to-many object.
   Parameters:
     one     _the single point object
     & many  _optional hash-set of connected objects"
  [one & many]
  (one-to-many. one (if (nil? many) #{} (hash-set many))))

(defn add
  "Returns a new one-to-many object that consists of otm
   and has some more elements."
  [otm & els]
  (assert (keyword-isa? otm one-to-many))
  (one-to-many. (:one otm) (apply conj (:many otm) els)))

(defn del
  "Returns a new one-to-many object that consists of otm
   and has some less elements."
  [otm & els]
  (assert (keyword-isa? otm one-to-many))
  (one-to-many. (:one otm) (apply disj (:many otm) els)))

(defn call-one
  "Calls the given function with the one-part of the given one-many relation
  as first parameter."
  [otm f & parms]
  (assert (keyword-isa? otm one-to-many))
  (apply f (:one otm) parms))

(defn call-many
  "Calls the given function several times with each many-part of the given 
  one-many relation as first parameter for one."
  [otm f & parms]
  (assert (keyword-isa? otm one-to-many))
  (doseq [m (:many otm)] (apply f m parms)))

(defn call-first
  "Calls the given function with the given parameter for the first of
  the many."
  [otm f & parms]
  (assert (keyword-isa? otm one-to-many))
  (let [m (:many otm)]
    (when-not (empty? m)
      (apply f (first m) parms))))

;;;

nil
