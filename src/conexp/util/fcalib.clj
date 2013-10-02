;; Copyright (c) Daniel Borchmann. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.util.fcalib
  (:use conexp.base
        conexp.fca.implications)
  (:import [de.tudresden.inf.tcs.fcaapi FCAImplication]
           [de.tudresden.inf.tcs.fcalib Implication]
           ;; for now, we use conexp-ng's implementation, until the corresponding code is
           ;; available in fcalib
           [fcatools.conexpng.model FormalContext]))

;;

(defn to-fcalib-context [conexp-clj-context]
  (not-yet-implemented))

(defn from-fcalib-context [^FormalContext fcalib-context]
  (not-yet-implemented))

(defn to-fcalib-implication [conexp-clj-implication]
  "Convert implications as used in conexp-clj to implications as used by fcalib."
  (Implication. (premise conexp-clj-implication)
                (conclusion conexp-clj-implication)))

(defn from-fcalib-implication [^FCAImplication fcalib-implication]
  "Convert implications as used in fcalib to implications as used in conexp-clj."
  (make-implication (.getPremise fcalib-implication)
                    (.getConclusion fcalib-implication)))

(defn to-fcalib-implication-set [conexp-clj-set-of-implications]
  (not-yet-implemented))

;;

nil
