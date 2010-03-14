;; Copyright (c) Daniel Borchmann. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.io.lattices
  (:use conexp
	conexp.io.util)
  (:use clojure.contrib.io))

;;; Method Declaration

(defmulti write-lattice (fn [format ctx file] format))

(let [known-lattice-input-formats (ref {})]
  (defn- add-lattice-input-format [name predicate]
    (dosync
     (alter known-lattice-input-formats assoc name predicate)))

  (defn- get-known-lattice-input-formats []
    (keys @known-lattice-input-formats))

  (defn- find-lattice-input-format [file]
    (first
     (for [[name predicate] @known-lattice-input-formats
	   :when (with-open [in-rdr (reader file)]
		   (predicate in-rdr))]
       name))))

(defmulti read-lattice find-lattice-input-format)

(defmethod write-lattice :default [format _ _]
  (illegal-argument "Format " format " for lattice output is not known."))

(defmethod read-lattice :default [file]
  (illegal-argument "Cannot determine format of lattice in " file))

;;;
;;; Formats
;;;


;;;

nil
