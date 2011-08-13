;; Copyright (c) Daniel Borchmann. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.contrib.dl.framework.semantics
  (:use conexp.main
        conexp.contrib.dl.framework.syntax
        conexp.contrib.dl.framework.boxes))

(ns-doc "Defines basic notions for semantics of description logics.")

;;; interpretation definition

(defrecord Interpretation [language base-set function])

(defn interpretation-base-set
  "Returns base set of a given interpretation."
  [^Interpretation interpretation]
  (.base-set interpretation))

(defn interpretation-language
  "Returns language of the given interpretation."
  [^Interpretation interpretation]
  (.language interpretation))

(defn interpretation-function
  "Returns the interpretation function of given interpretation."
  [^Interpretation interpretation]
  (.function interpretation))

(defn make-interpretation
  "Returns an interpretation for a given DL language on the given base set."
  [language base-set interpretation-function]
  (Interpretation. language base-set interpretation-function))

(defmethod print-method Interpretation [interpretation out]
  (let [^String output (with-out-str (print (list 'Interpretation
                                                  (interpretation-language interpretation)
                                                  (interpretation-base-set interpretation)
                                                  (interpretation-function interpretation))))]
    (.write ^java.io.Writer out (.trim output))))

;;; Interpretations

(defmulti compile-expression
  "Compiles an expression to a function mapping a model to the extent
  of this expression in the model."
  (fn [dl-expression]
    (cond
     (compound? dl-expression)  (operator dl-expression),
     :else                      ::base-case)))

(defmethod compile-expression :default [dl-expression]
  (illegal-argument "Dont know how to interpret " (print-str dl-expression)))

(defn interpret
  "Interprets given expression in given interpretation and returns the
  corresponding extent."
  [interpretation dl-expression]
  ((compile-expression (if (dl-expression? dl-expression)
                        dl-expression
                        (make-dl-expression (interpretation-language interpretation)
                                            dl-expression)))
   interpretation))

(defmethod compile-expression ::base-case [dl-expression]
  (fn [interpretation]
    (let [result ((interpretation-function interpretation) (expression-term dl-expression))]
      (if (nil? result)
        (let [base-semantics (get-method compile-expression
                                         [(language-name (expression-language dl-expression)) ::base-semantics]),
              default        (get-method compile-expression
                                         :default)]
          (when (= base-semantics default)
            (throw (IllegalStateException. (str "Cannot interpret " (print-str dl-expression) "."))))
          ((base-semantics dl-expression) interpretation))
        result))))


;;; base semantics (i.e. what to do if nothing else applies)

(defmacro define-base-semantics
  "Define how to interpret an expression which is neither compound nor
  a primitive concept, i.e. TBox-ABox pairs and the like."
  [language [interpretation dl-expression] & body]
  `(defmethod compile-expression [(language-name ~language) ::base-semantics] [~dl-expression]
     (fn [~interpretation]
       ~@body)))


;;; defining new constructors

(defmacro define-constructor
  "Defines a new constructor for description logics. Captures the
  variables «interpretation» and «dl-exp» for representing the
  interpretation and the dl-expression used."
  [name & body]
  `(do
     (defmethod compile-expression '~name [~'dl-exp]
       (fn [~'interpretation]
         ~@body))
     (add-common-constructor! '~name)))

(define-constructor and
  (reduce intersection (interpretation-base-set interpretation)
          (map #(interpret interpretation %) (arguments dl-exp))))

(define-constructor or
  (reduce union #{}
          (map #(interpret interpretation %) (arguments dl-exp))))

(define-constructor not
  (difference (interpretation-base-set interpretation)
              (interpret interpretation (first (arguments dl-exp)))))

(define-constructor exists
  (let [r-I (interpret interpretation (first (arguments dl-exp))),
        C-I (interpret interpretation (second (arguments dl-exp)))]
    (set-of x [[x y] r-I
               :when (contains? C-I y)])))

(define-constructor forall
  (let [r-I (interpret interpretation (first (arguments dl-exp))),
        C-I (interpret interpretation (second (arguments dl-exp)))]
    ;; interpreting Delta\(exists r. not C)
    (difference (interpretation-base-set interpretation)
                (set-of x [[x y] r-I
                           :when (not (contains? C-I y))]))))

(define-constructor inverse
  (let [r-I (interpret interpretation (first (arguments dl-exp)))]
    (set-of [y x] [[x y] r-I])))

(define-constructor nominal
  (let [individuals (map expression-term (arguments dl-exp))]
    (intersection (interpretation-base-set interpretation)
                  (set individuals))))

(define-constructor top
  (assert (empty? (arguments dl-exp))
          "Top concept constructor does not take any arguments.")
  (interpretation-base-set interpretation))

(define-constructor bottom
  (assert (empty? (arguments dl-exp))
          "Bottom concept constructor does not take any arguments.")
  #{})


;;; interpretation syntax

(defmacro interpretation
  "Defines an interpretation for language on base-set: interpretations
  maps atomic expressions to their extents."
  [language base-set & interpretations]
  `(let [interpretation-map# '~(apply hash-map interpretations),
         defined-symbols#    (keys interpretation-map#),
         undefined-symbols#  (difference (union (concept-names ~language)
                                                (role-names ~language))
                                         (set defined-symbols#))]
     (when (not (empty? undefined-symbols#))
       (illegal-argument "Definition of model is incomplete. The symbols "
                         undefined-symbols# " are missing."))
     (make-interpretation ~language (set '~base-set) interpretation-map#)))

(add-dl-syntax! 'interpretation)

(defmacro define-interpretation
  "Globally defines an interpretation with name for language on
  base-set: interpretations maps atomic expressions to their extents."
  [name language base-set & interpretations]
  `(def ~name (interpretation ~language ~base-set ~@interpretations)))


;;; most specific concepts

(defmulti most-specific-concept
  "Computes the model based most specific concept of a set of objects
  in a given interpretation."
  (fn [model dl-exp]
    (language-name (interpretation-language model))))

(defmethod most-specific-concept :default [model _]
  (illegal-argument "Language "
                    (print-str (interpretation-language model))
                    " does not provide msc."))

(defmacro define-msc
  "Defines model based most specific concepts for a language, a model
  and a set of objects. Must return a dl-expression."
  [language [model objects] & body]
  `(defmethod most-specific-concept (language-name ~language)
     [~model ~objects]
     ~@body))

(defn model-closure
  "Return the most specific concept of the interpretation of dl-exp in
  model."
  [model dl-exp]
  (most-specific-concept model (interpret model dl-exp)))

;;;

(defn extend-interpretation
  "Extends interpretation by the given interpretation function i. i
  should return nil if it doesn't change a value of model's original
  interpretion, where then the original interpretation is used."
  [interpretation i]
  (make-interpretation (interpretation-language interpretation)
                       (interpretation-base-set interpretation)
                       (fn [A]
                         (or (i A)
                             ((interpretation-function interpretation) A)))))

(defn holds-in-interpretation?
  "Returns true iff subsumption holds in given interpretation."
  [interpretation subsumption]
  (subset? (interpret interpretation (subsumee subsumption))
           (interpret interpretation (subsumer subsumption))))

(defnk hash-map->interpretation
  "Given concepts as a hash-map from symbols to sets and roles as a
  hash-map from symbols to sets of pairs returns a model containing
  the hash-maps as interpretation. If parameter :base-lang is given
  the description logic used in this model will be an extension of the
  parameter value."
  [concepts roles :base-lang nil]
  (let [concept-names (keys concepts),
        role-names    (keys roles),
        language      (make-dl (gensym)
                               concept-names
                               role-names
                               []
                               :extends base-lang),
        base-set      (union (set-of x [[conc extension] concepts,
                                        x extension])
                             (set-of x [[role extension] roles,
                                        pair extension,
                                        x pair]))]
    (make-interpretation language base-set (merge concepts roles))))


;;; TBox interpretations

(defn- fixed-point
  "Apply f to data until (= old-data new-data)."
  [f data]
  (let [runner (fn runner [old-data]
                 (let [new-data (f old-data)]
                   (if (= new-data old-data)
                     new-data
                     (recur new-data))))]
    (runner data)))

(defn- next-tbox-interpretation
  "Defines a new interpretation function on the defined concepts of
  tbox in interpretation through an interpretation i of the defined
  concepts of tbox."
  [interpretation tbox i]
  (let [new-interpretation (extend-interpretation interpretation i)]
    (reduce! (fn [map [sym sym-def]]
               (assoc! map sym (interpret new-interpretation
                                          (definition-expression sym-def))))
             {}
             (tbox-definition-map tbox))))

(defn- constant-tbox-interpretation
  "Returns an interpretation function on the defined concepts of tbox,
  constantly returning value."
  [tbox value]
  (map-by-fn (constantly value)
             (defined-concepts tbox)))

(defn gfp-model
  "Returns the gfp-model of tbox in interpretation."
  [tbox interpretation]
  (extend-interpretation
   interpretation
   (fixed-point (fn [i]
                  (next-tbox-interpretation interpretation tbox i))
                (constant-tbox-interpretation tbox
                                              (interpretation-base-set interpretation)))))

(defn lfp-model
  "Returns the lfp-model of tbox in interpretation."
  [tbox interpretation]
  (extend-interpretation
   interpretation
   (fixed-point (fn [i]
                  (next-tbox-interpretation interpretation tbox i))
                (constant-tbox-interpretation tbox #{}))))

;;;

nil
