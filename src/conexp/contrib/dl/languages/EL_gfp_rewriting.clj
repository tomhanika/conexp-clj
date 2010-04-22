;; Copyright (c) Daniel Borchmann. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.contrib.dl.languages.EL-gfp-rewriting
  (:use conexp.main
	conexp.contrib.dl.framework.syntax
	conexp.contrib.dl.languages.EL-gfp))

(update-ns-meta! conexp.contrib.dl.languages.EL-gfp-rewriting
  :doc "Provides functions for EL-gfp term rewriting.")

;;;

(defn- arguments*
  "Returns the arguments of the given DL expression as set, if it is
  not atomic. If it is, returns the singleton set of the dl-expression
  itself."
  [dl-expression]
  (if (atomic? dl-expression)
    (set [dl-expression])
    (set (arguments dl-expression))))

(defn- implication-kernel
  "Given a set of concepts returns a minimal subset of concepts which,
  when closed under the given implications, yields a superset of the
  original set given."
  [set-of-concepts set-of-implications]
  (let [implication-closure (clop-by-implications set-of-implications)]
    (loop [to-consider (seq set-of-concepts),
	   concepts set-of-concepts]
      (if (empty? to-consider)
	concepts
	(let [next-concept (first to-consider)]
	  (recur (rest to-consider)
		 (if (contains? (implication-closure (disj concepts next-concept))
				next-concept)
		   (disj concepts next-concept)
		   concepts)))))))

(defn abbreviate-subsumption
  "Takes a subsumption whose subsumee and subsumer are in normal form
  and returns a subsumption where from the subsumer every term already
  present in the subsumee is removed."
  [subsumption background-knowledge]
  (let [language (expression-language (subsumee subsumption)),
	premise-args (arguments* (subsumee subsumption)),
	conclusion-args (difference (arguments* (subsumer subsumption))
                                    premise-args)]
    (make-subsumption (make-dl-expression language (cons 'and (implication-kernel premise-args background-knowledge)))
		      (make-dl-expression language (cons 'and (implication-kernel conclusion-args background-knowledge))))))

;;;

nil

