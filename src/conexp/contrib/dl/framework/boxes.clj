;; Copyright (c) Daniel Borchmann. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.contrib.dl.boxes
  (:use conexp
	conexp.contrib.dl.base))

;;; TBox definitions

(deftype TBox [language definitions])

(defn tbox-language
  "Returns language for which tbox is a tbox."
  [tbox]
  (:language tbox))

(defn tbox-definitions
  "Returns the definitions in a tbox."
  [tbox]
  (:definitions tbox))

(defmethod print-method ::TBox [tbox out]
  (.write out "TBox"))

;;; normalizing TBoxes

;;; acessing used role name, primitive and defined concepts

;;;

nil
