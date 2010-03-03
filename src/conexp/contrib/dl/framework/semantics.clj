;; Copyright (c) Daniel Borchmann. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.contrib.dl.framework.semantics
  (:use conexp
	conexp.contrib.dl.framework.syntax
	conexp.contrib.dl.framework.models
	conexp.contrib.dl.framework.boxes))

;;;

(defn next-interpretation
  "Defines a new interpretation on the defined concepts of tbox in
  model through interpretation."
  [model tbox interpretation]
  (let [extended-model (extend-model model interpretation)]
    (loop [defs  (seq (tbox-definitions tbox)),
	   new-i {}]
      (if (empty? defs)
	new-i
	(recur (rest defs)
	       (let [def (first defs)]
		 (conj new-i [(definition-target def)
			      (interpret extended-model (definition-expression def))])))))))

(defn fixed-point
  "Apply f to data until (= old-data new-data)."
  [f data]
  (let [runner (fn runner [old-data]
		 (let [new-data (f old-data)]
		   (if (= new-data old-data)
		     new-data
		     (recur new-data))))]
    (runner data)))

(defn constant-interpretation
  "Return interpretation on the defined concepts of tbox, constantly
  returning value."
  [tbox value]
  (hashmap-by-function (constantly value)
		       (defined-concepts tbox)))

(defn gfp-model
  "Returns the gfp-model of tbox in model."
  [tbox model]
  (fixed-point (fn [i]
		 (next-interpretation model tbox i))
	       (constant-interpretation tbox (model-base-set model))))

(defn lfp-model
  "Returns the lfp-model of tbox in model."
  [tbox model]
  (fixed-point (fn [i]
		 (next-interpretation model tbox i))
	       (constant-interpretation tbox #{})))

;;;

nil
