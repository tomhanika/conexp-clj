;; Copyright (c) Daniel Borchmann. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.util.one-to-many
  (:use conexp.util))

;; one-to-many abstraction

(deftype one-to-many [one many])


(defn make-one-to-many
  "Creates a one-to-many object.
   Parameters:
     one     _the single point object
     & many  _optional list of connected objects"
  [one & many] (one-to-many one many))
