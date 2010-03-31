;; Copyright (c) Daniel Borchmann. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.util.one-to-many
  (:use conexp.util.multimethods))

;; one-to-many abstraction

(deftype one-to-many [one many])


(defn make-one-to-many
  "Creates a one-to-many object.
   Parameters:
     one     _the single point object
     & many  _optional hash-set of connected objects"
  [one & many] (one-to-many one (if (nil? many) #{} (hash-set many))))


(inherit-multimethod add ::one-to-many
  "Returns a new one-to-many object that consists of otm
   and has some more elements.
  
  Parameters:
    otm   _one-to-many object
    & els _new elements")

(defmethod add ::one-to-many
  [otm & els]
  (one-to-many (:one otm) (apply conj (:many otm) els)))

(inherit-multimethod del ::one-to-many
  "Returns a new one-to-many object that consists of otm
   and has some less elements.
  
  Parameters:
    otm   _one-to-many object
    & els _dismissed elements")

(defmethod del ::one-to-many
  [otm & els]
  (one-to-many (:one otm) (apply disj (:many otm) els)))
  

(inherit-multimethod call-one ::one-to-many
  "Calls the given function with the one-part of the given one-many relation
  as first parameter.
  
  Parameters:
    otm     _one-to-many object
    f       _function
    & parms _function parameters")

(defmethod call-one ::one-to-many
  [otm f & parms]
  (apply f  (:one otm) parms))

(inherit-multimethod call-many ::one-to-many
  "Calls the given function several times with each many-part of the given 
  one-many relation as first parameter for one.
  
  Parameters:
    otm     _one-to-many object
    f       _function
    & parms _function parameters")

(defmethod call-many ::one-to-many
  [otm f & parms]
  (doseq [m (:many otm)] (apply f m parms)))


(inherit-multimethod call-first ::one-to-many
  "Calls the given function with the given parameter for the first of the many.
  
  Parameters:
    otm     _one-to-many object
    f       _function
    & parms _function parameters")

(defmethod call-first ::one-to-many
  [otm f & parms]
  (let [m (:many otm)]
    (when-not (empty? m)
      (apply f (first m) parms))))
