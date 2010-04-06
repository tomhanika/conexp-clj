;; Copyright (c) Daniel Borchmann. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.main
  (:require conexp.base
	    conexp.fca
	    conexp.io
	    conexp.layout
	    conexp.graphics
	    conexp.gui)
  (:use [clojure.contrib.ns-utils :only (immigrate)]))

;;;

(def *conexp-namespaces* '[conexp.base
			   conexp.fca
			   conexp.io
			   conexp.layout
			   conexp.graphics
			   conexp.gui])

(dorun (map immigrate *conexp-namespaces*))

(update-ns-meta! conexp.main
  :doc "Main namespace for conexp-clj. Immigrates all needed namespaces.")

(def *conexp-version* {:major 0,
		       :minor 0,
		       :patch 2
		       :qualifier "pre-alpha"})

(defn conexp-version
  "Returns the version of conexp as a string."
  []
  (let [{:keys [major minor patch qualifier]} *conexp-version*]
    (str major "." minor "." patch "-" qualifier)))

(defn has-version?
  "Compares given version of conexp and returns true if and only if
  the current version of conexp is higher or equal than the given one"
  [{my-major :major, my-minor :minor, my-patch :patch}]
  (let [{:keys [major, minor, patch]} *conexp-version*]
    (or (and (< my-major major))
	(and (= my-major major)
	     (< my-minor minor))
	(and (= my-major major)
	     (= my-minor minor)
	     (< my-patch patch)))))

;;;

nil
