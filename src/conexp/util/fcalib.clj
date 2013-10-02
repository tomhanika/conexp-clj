;; Copyright (c) Daniel Borchmann. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.util.fcalib
  (:use conexp.base)
  (:import [de.tudresden.inf.tcs.fcaapi FCAImplication]
           [de.tudresden.inf.tcs.fcalib Implication]
           ;; for now, we use conexp-ng's implementation, until the corresponding code is
           ;; available in fcalib
           [fcatools.conexpng.model FormalContext]))

;;

(defn fcalib-to-context [conexp-clj-context]
  (not-yet-implemented))

(defn fcalib-from-context [^FormalContext fcalib-context]
  (not-yet-implemented))

(defn fcalib-to-implication [conexp-clj-implication]
  (not-yet-implemented))

(defn fcalib-from-implication [^FCAImplication fcalib-implication]
  (not-yet-implemented))

(defn fcalib-to-implication-set [conexp-clj-set-of-implications]
  (not-yet-implemented))

;;

nil
