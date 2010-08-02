;; Copyright (c) Daniel Borchmann. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.io.layouts
  (:use conexp.base
	conexp.io.util
	conexp.layout.base)
  (:import [java.io PushbackReader]))

(ns-doc "Implements IO for layouts.")

;;; Input format dispatch

(define-format-dispatch "layout")
(set-default-layout-format! :simple)

;;; Formats

;; Simple conexp-clj Format for Layout

(add-layout-input-format :simple
			 (fn [rdr]
			   (= "conexp-clj simple" (.readLine rdr))))

(define-layout-output-format :simple
  [layout file]
  (with-out-writer file
    (println "conexp-clj simple")
    (prn {:layout [(positions layout)
		   (connections layout)]})))

(define-layout-input-format :simple
  [file]
  (with-in-reader file
    (let [_        (get-line),
	  hash-map (binding [*in* (PushbackReader. *in*)]
		     (read)),
	  layout   (:layout hash-map)]
      (when-not layout
	(illegal-argument "File " file " does not contain a layout."))
      (apply make-layout layout))))

;;; ConExp Layout format

'TODO

;;;

nil
