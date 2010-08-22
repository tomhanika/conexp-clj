;; Copyright (c) Daniel Borchmann. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.contrib.doc
  (:use conexp.main)
  (:use [clojure.contrib.io :only (with-out-writer)]
	[clojure.contrib.string :only (split)]))


;;; API Documentation

(defn- public-api
  "Returns a map of public functions of namespaces to their
  corresponding documentation."
  [ns]
  (let [ns (find-ns ns)]
    (into {}
	  (map (fn [[function var]]
		 [function (str (:arglists (meta var))
				"\n\n"
				(:doc (meta var)))])
	       (filter (fn [[f var]]
			 (and (= (:ns (meta var)) ns)
			      (not (Character/isUpperCase ^Character (first (str f))))))
		       (ns-map ns))))))


;;; Documentation Coverage

(defn conexp-fns-needing-doc
  "Returns function in public conexp-clj api not having documentation."
  []
  (for [ns *conexp-namespaces*
	[f _] (public-api ns)
	:when (not (:doc (meta (resolve (symbol (str ns) (str f))))))]
    (symbol (str ns) (str f))))


;;; Test Coverage

(defn- defining-namespace
  "Returns the namespace in which name in namespace ns was orignally defined."
  [ns name]
  (let [defining-file (:file (meta (resolve (symbol (str ns) (str name)))))]
    (apply str
	   (interpose "."
		      (split #"/" (apply str
					 (drop-last 4 defining-file)))))))

(defn- test-exists?
  "Checks whether for symbol f in namespace conexp.ns there exists a
  symbol test-f in conexp.tests.ns."
  [ns f]
  (let [test-ns (symbol (str "conexp.tests." (second (split #"\." 2 (str ns)))))]
    (try
     (require test-ns)
     (contains? (public-api test-ns) (symbol (str "test-" f)))
     (catch java.io.FileNotFoundException _
       false))))

(defn conexp-fns-needing-tests
  "Returns all functions in the public API of conexp, where no
  corresponding test in conexp.tests exists."
  []
  (for [ns *conexp-namespaces*
	[f _] (public-api ns)
	:let [def-ns (defining-namespace ns f)]
	:when (and (.startsWith ^String def-ns "conexp")
		   (not (test-exists? (defining-namespace ns f) f)))]
    (symbol def-ns (str f))))

;;;

nil
