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
	    conexp.layout)
  (:use [clojure.contrib.ns-utils :only (immigrate)]))

(conexp.base/ns-doc
 "Main namespace for conexp-clj. Immigrates all needed namespaces.")

;;;

(def *conexp-namespaces* '[conexp.base
			   conexp.fca
			   conexp.io
			   conexp.layout])

(dorun (map immigrate *conexp-namespaces*))

;;;

(defvar- *internal-version-string*
  (.trim #=(slurp "VERSION")))

(def *conexp-version* (let [[_ major minor patch qualifier] (re-find #"(\d+).(\d+).(\d+)-(\w+)" *internal-version-string*)]
                        {:major major,
                         :minor minor,
                         :patch patch,
                         :qualifier qualifier}))

(defn- conexp-built-version
  "Returns the date of the conexp build, retrieved from the name of
  the conexp-clj jar file. Returns \"source\" if there is none in the
  classpath."
  []
  (if-let [[_ date time] (re-find #"conexp-clj-.*-(\d+).(\d+).jar"
                                  (System/getProperty "java.class.path"))]
    (str date "." time)
    "source"))

(defn conexp-version
  "Returns the version of conexp as a string."
  []
  (let [{:keys [major minor patch qualifier]} *conexp-version*]
    (str major "." minor "." patch "-" qualifier "-" (conexp-built-version))))

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

(defn test-conexp
  "Runs tests for conexp."
  []
  (require 'conexp.tests :reload-all)
  (require 'clojure.test)
  (clojure.test/run-tests 'conexp.tests))

;;;


nil

