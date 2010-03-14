;; Copyright (c) Daniel Borchmann. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.io.util
  (:use conexp.base)
  (:use [clojure.contrib.io :only (reader)]))

;;;

(defn get-line []
  (read-line))

(defn get-lines [n]
  (doall (take n (repeatedly #(get-line)))))

(defmacro with-in-reader [file & body]
  `(with-open [input# (reader ~file)]
     (binding [*in* input#]
       ~@body)))

;;; Format dispatch framework macro

(defmacro define-format-dispatch
  "Defines for name the functions write-name, read-name,
  add-name-input-format, get-known-name-input-formats and
  find-name-input-format. You can then add new formats with
  add-name-input-format and read-name will automatically dispatch in
  the format determined from its only argument."
  [name]
  (let [add   (symbol (str "add-" name "-input-format")),
	get   (symbol (str "get-known-" name "-input-formats")),
	find  (symbol (str "find-" name "-input-format")),
	write (symbol (str "write-" name)),
	read  (symbol (str "read-" name))]
  `(do
     (let [known-context-input-formats# (ref {})]
       (defn- ~add [name# predicate#]
	 (dosync
	  (alter known-context-input-formats# assoc name# predicate#)))

       (defn- ~get []
	 (keys @known-context-input-formats#))

       (defn- ~find [file#]
	 (first
	  (for [[name# predicate#] @known-context-input-formats#
		:when (with-open [in-rdr# (reader file#)]
			(predicate# in-rdr#))]
	    name#)))

       nil)

     (defmulti ~write (fn [format# ctx# file#] format#))
     (defmethod ~write :default [format# _# _#]
       (illegal-argument "Format " format# " for " ~name " output is not known."))

     (defmulti ~read ~find)
     (defmethod ~read :default [file#]
       (illegal-argument "Cannot determine format of " ~name " in " file#))

     nil)))


;;;

nil
