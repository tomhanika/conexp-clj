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
        conexp.contrib.dl.framework.boxes
        conexp.contrib.dl.languages.description-graphs)
  (:use [clojure.walk :only (walk)]))

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

(defn- abbreviate-expression
  "Abbreviates expression with given knowledge."
  [expression knowledge]
  (cond
   (tbox-target-pair? expression)
   (let [[tbox target] (expression-term expression)]
     (make-dl-expression (expression-language expression)
                         [(make-tbox (expression-language expression)
                                     (into {} (for [[sym def] (tbox-definition-map tbox)]
                                                [sym (make-dl-definition (definition-target def)
                                                                         (abbreviate-expression (definition-expression def)
                                                                                                knowledge))])))
                          target])),

   (atomic? expression) expression,

   (= 'and (operator expression))
   (let [shorter (implication-kernel (arguments* expression) knowledge)]
     (make-dl-expression-nc (expression-language expression)
                            (cons 'and (map #(expression-term (abbreviate-expression % knowledge)) shorter)))),

   (= 'exists (operator expression))
   (let [args (arguments expression)]
     (make-dl-expression-nc (expression-language expression)
                            (list 'exists (expression-term (nth args 0))
                                  (expression-term (abbreviate-expression (nth args 1)
                                                                          knowledge))))),

   :else (illegal-argument "Abbreviate-expression can only handle EL expressions.")))


(defn abbreviate-subsumption
  "Takes a subsumption whose subsumee and subsumer are in normal form
  and returns a subsumption where from the subsumer every term already
  present in the subsumee is removed."
  [subsumption background-knowledge]
  (let [language (expression-language (subsumee subsumption)),
	premise-args (arguments* (subsumee subsumption)),
	conclusion-args (difference (arguments* (subsumer subsumption))
                                    premise-args)]
    (make-subsumption (abbreviate-expression (make-dl-expression language (cons 'and premise-args)) background-knowledge)
                      (abbreviate-expression (make-dl-expression language (cons 'and conclusion-args)) background-knowledge))))

;;; EL and EL-gfp normalization

(defn- minimal-elements
  "For a given sequence sqn of elements and an order relation
  more-specific?, return all minimal elements of sqn wrt
  more-specific?."
  [sqn more-specific?]
  (loop [collected (),
         left sqn]
    (if (empty? left)
      (reverse collected)
      (let [next (first left)]
        (recur (conj collected next)
               (remove #(more-specific? next %) (rest left)))))))

(defn- more-specific?
  "Returns true iff exp-1 is more specific than exp-2."
  [exp-1 exp-2]
  (let [atom?   #(not (seq? %)),
        and?    #(and (seq? %) (= 'and (first %))),
        exists? #(and (seq? %) (= 'exists (first %)))]
    (cond
     (= exp-1 exp-2) true,

     (and (atom? exp-1) (atom? exp-2)) false,

     (and (and? exp-1)
          (not (and? exp-2)))
     (or (some #(more-specific? % exp-2) (rest exp-1))
         false),

     (and (not (and? exp-1))
          (and? exp-2))
     (more-specific? (list 'and exp-1) exp-2),

     (and (and? exp-1) (and? exp-2))
     (every? #(more-specific? exp-1 %) (rest exp-2)),

     (and (exists? exp-1) (exists? exp-2))
     (and (= (second exp-1) (second exp-2))
          (more-specific? (nth exp-1 2) (nth exp-2 2))),

     :else false)))

(defn normalize-EL-term
  "Normalizes a given EL term."
  [term]
  (let [transform (fn transform [sexp]
                    (cond
                     (and (seq? sexp)
                          (= (first sexp) 'exists))
                     (list (first sexp) (second sexp) (transform (nth sexp 2))),

                     (and (seq? sexp)
                          (= (first sexp) 'and))
                     (walk transform
                           #(list* (first sexp) (minimal-elements % more-specific?))
                           (rest sexp)),

                     :else sexp))]
    (transform term)))

(defn normalize-EL-gfp-term
  "Normalizes a given EL-gfp term. tbox must not contains embedded TBoxes."
  [[tbox target] ]
  (assert (tbox? tbox))
  [(make-tbox (tbox-language tbox)
              (into {} (for [[sym def] (tbox-definition-map tbox)]
                         [sym (make-dl-definition (tbox-language tbox)
                                                  (definition-target def)
                                                  (normalize-EL-term (expression-term (definition-expression def))))]))),
   target])

;;;

nil

