;; Copyright ⓒ the conexp-clj developers; all rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.layouts
  (:require [conexp.layouts
             [common :refer :all]
             [layered :refer :all]]))

;;;

(def standard-layout
  "Standard layout function. Call on a lattice to get a layout."
  simple-layered-layout)

(defn inf-additive-layout
  "Returns an infimum additive layout for lattice."
  [lattice]
  (to-inf-additive-layout (standard-layout lattice)))

;;;

nil
