;; Copyright (c) Daniel Borchmann. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.fca.rule-exploration
  (:use conexp.base
	conexp.fca.implications))

(defn apply-to-implication [alpha impl]
  (make-implication (map alpha (premise impl))
		    (map alpha (conclusion impl))))

(defn position [x vec]
  (loop [pos 0
	 vec vec]
    (cond
      (empty? vec)
      nil
      (= x (first vec))
      pos
      :else
      (recur (inc pos) (rest vec)))))

(defn permutation
  ; hack only, slow and bad
  [source target]
  (fn [x]
    (nth target (position x source))))

nil
