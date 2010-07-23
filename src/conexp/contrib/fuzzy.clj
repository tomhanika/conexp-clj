;; Copyright (c) Daniel Borchmann. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.contrib.fuzzy
  (:use conexp.main)
  (:use [clojure.contrib.ns-utils :only (immigrate)])
  (:require conexp.contrib.fuzzy.sets
            conexp.contrib.fuzzy.logics
            conexp.contrib.fuzzy.fca))

(ns-doc "Fuzzy Logics for Formal Concept Analysis")

;;;

(immigrate 'conexp.contrib.fuzzy.sets
           'conexp.contrib.fuzzy.logics
           'conexp.contrib.fuzzy.fca)

;;;

nil

