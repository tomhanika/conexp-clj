;; Copyright (c) Daniel Borchmann. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.tests.io.util
  (:use conexp.base
        conexp.io)
  (:import [java.io File]))

;;;

(defn tmpfile
  "Returns a temporary and unique File object."
  []
  (File/createTempFile "conexp-clj-" ".tmp"))

(defn out-in
  "Returns object, treates as type, read in from a file where it has
  previously written to. format is used for output."
  [object type format]
  (let [writer (resolve (symbol "conexp.io" (str "write-" type))),
        reader (resolve (symbol "conexp.io" (str "read-" type)))]
    (when (or (nil? writer) (nil? reader))
      (illegal-argument "out-in called with invalid type " type "."))
    (let [tmp (. (tmpfile) getAbsolutePath)]
      (@writer format object tmp)
      (@reader tmp))))

(defn out-in-out-in-test
  "Checks for object of type type whether it passes out-in-out-in,
  where format is used for output."
  [object type format]
  (let [obj-1 (out-in object type format),
        obj-2 (out-in obj-1 type format)]
    (= obj-1 obj-2)))

;;;

nil
