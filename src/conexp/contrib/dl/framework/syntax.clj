;; Copyright (c) Daniel Borchmann. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.contrib.dl.framework.syntax
  (:use conexp.main)
  (:use [clojure.walk :only (walk)]))

(ns-doc
 "Provides basic syntax definitions for DL expressions and the like.")

;;;

(defvar ^{:dynamic true} *print-with-dl-type* false
  "If true, prints DL data structures with type tag.")

;;;

(defrecord DL [name concept-names role-names constructors])

(defn language-name
  "Returns the name of the given language."
  [^DL language]
  (.name language))

(defn concept-names
  "Returns the concept names of the given language."
  [^DL language]
  (.concept-names language))

(defn role-names
  "Returns the role names of the given language."
  [^DL language]
  (.role-names language))

(defn signature
  "Returns the signature of the given language, i.e. the pair of role
  names and concept names."
  [language]
  [(role-names language), (concept-names language)])

(defn constructors
  "Returns all allowed constructors of the given language."
  [^DL language]
  (.constructors language))

(defmethod print-method DL [dl out]
  (.write ^java.io.Writer out (print-str (list 'DL (name (language-name dl))))))

(defn make-language
  "Creates a DL from concept-names, role-names and constructors."
  [name concept-names role-names constructors]
  (DL. (keyword "conexp.contrib.dl.framework" (str name))
       (set concept-names)
       (set role-names)
       (set constructors)))

(defn restrict-language
  "Restricts the given DL to the given concept- and role-names."
  [^DL dl concept-names role-names]
  (DL. (.name dl)
       (set concept-names)
       (set role-names)
       (constructors dl)))

;;;

(deftype DL-expression [language sexp hash-cache]
  Object
  (equals [this other]
    (generic-equals [this other] DL-expression [language sexp]))
  (hashCode [this]
    (if-let [cached-value @hash-cache]
      cached-value
      (let [result (hash-combine-hash DL-expression language sexp)]
        (reset! hash-cache result)
        result))))

(defn make-dl-expression-nc
  "Creates a DL expression without any checks on already present DL
  expression. Use with care."
  [language dl-sexp]
  (DL-expression. language dl-sexp (atom nil)))

(defn expression-term
  "Returns the s-exp describing this expression."
  [^DL-expression dl-expression]
  (.sexp dl-expression))

(defn expression-language
  "Returns the language of this expression."
  [^DL-expression dl-expression]
  (.language dl-expression))

(defmethod print-method DL-expression [dl-exp out]
  (let [^String output (with-out-str
                          (if *print-with-dl-type*
                            (print (list 'DL-expr (expression-term dl-exp)))
                            (print (expression-term dl-exp))))]
    (.write ^java.io.Writer out (.trim output))))

;;;

(defn dl-expression?
  "Returns true iff thing is a DL expression. If dl is given, checks
  for thing to be a dl-expression in dl."
  ([thing]
     (instance? DL-expression thing))
  ([dl thing]
     (and (dl-expression? thing)
          (= dl (expression-language thing)))))

(defn- dl-sexp->term
  "Ensures no dl-expression objects in the syntax expression given."
  [expr]
  (cond
   (dl-expression? expr) (expression-term expr),
   (sequential? expr)    (walk dl-sexp->term identity expr),
   :else                 expr))

(defn make-dl-expression
  "Takes a DL and a s-exp describing a concept description and returns
  a DL-expression."
  [language dl-sexp]
  (make-dl-expression-nc language (dl-sexp->term dl-sexp)))

;;;

(let [dl-creators (atom #{})]
  (defn add-dl-syntax!
    "Adds a new keyword for with-dl."
    [symbol]
    (swap! dl-creators conj symbol))

  (defn- get-dl-syntax
    "Returns all symbols to be recognized by with-dl."
    []
    @dl-creators)

  nil)

(defmacro with-dl
  "Lets one write dl-expression without repeatedly naming the dl one
  is working with. Recognized keywords for dl-expression can be added
  with add-dl-syntax!.

  Note: This implementation is very simple. Don't try to shadow
  symbols which are recognized as syntax with local binding. This will
  not work."
  [dl & body]
  (let [symbols (get-dl-syntax)]
    (letfn [(insert-dl [form]
                       (cond
                        (and (seq? form)
                             (not (empty? form))
                             (contains? symbols (first form)))
                        (list* (first form) dl (walk insert-dl identity (rest form))),

                        (or (sequential? form)
                            (set? form)
                            (map? form))
                        (walk insert-dl identity form),

                        :else form))]
      (cons 'do (insert-dl body)))))

;; `(macrolet ~(vec (for [sym (get-dl-syntax)]
;;                    `(~sym [& args#] `(~'~sym ~'~dl ~@args#))))
;;    ~@body))

;;;

(let [common-constructors (atom #{})]

  (defn get-common-constructors
    "Returns all registered common constructors."
    []
    @common-constructors)

  (defn add-common-constructor!
    "Adds given symbol as a common constructor."
    [sym]
    (swap! common-constructors conj sym))

  nil)

(defmacro dl-expression
  "Allows input of DL s-expression without quoting. The following quoting rules apply:

    - function calls are not quoted (sequences starting with a symbol
      not being in (get-common-constructors).
    - capital letters are quoted (appearing outside of a function call)
    - symbols in (get-common-constructors) being the first element of a sequence are quoted."
  [language expression]
  (let [transform-symbol (fn [symbol]
                           (if (Character/isUpperCase ^Character (first (str symbol)))
                             (list 'quote symbol)
                             symbol)),
        transform (fn transform [sexp]
                    (cond
                     (seq? sexp)        (cond
                                         (empty? sexp) sexp,
                                         (contains? (get-common-constructors) (first sexp))
                                         (list* 'list (list 'quote (first sexp)) (walk transform identity (rest sexp))),
                                         :else sexp),
                     (sequential? sexp) (walk transform identity sexp),
                     (symbol? sexp)     (transform-symbol sexp),
                     :else              sexp))]
    `(make-dl-expression ~language ~(transform expression))))

(add-dl-syntax! 'dl-expression)

(defnk make-dl
  "Constructs a description logic from the given arguments."
  [name concepts roles constr :extends nil]
  (when-let [invalid (first (filter #(not (Character/isUpperCase ^Character (first (str %))))
                                    (concat concepts roles)))]
    (illegal-argument "Invalid Concept or Role name \"" invalid "\". "
                      "Concept and role names must start with a capital letter. (sorry for that)"))

  (when (not (empty? (intersection (set concepts) (set roles))))
    (illegal-argument "Concept and role names must be disjoint."))

  (let [base-lang     extends,

        disjoint-into (fn [sqn other-sqn]
                        (when-let [x (first (filter (fn [a] (some #(= % a) sqn)) other-sqn))]
                          (illegal-argument "Item «" x "» already defined in base language."))
                        (into sqn other-sqn)),

        concepts      (disjoint-into concepts (and base-lang (concept-names base-lang)))
        roles         (disjoint-into roles    (and base-lang (role-names base-lang)))
        constr        (disjoint-into constr   (and base-lang (constructors base-lang))),

        language      (make-language name concepts roles constr)]

    (when base-lang
      (derive (language-name language) (language-name base-lang)))

    language))

(defmacro define-dl
  "Defines a DL."
  [name concept-names role-names constructors & options]
  `(let [dl# (make-dl '~name '~concept-names '~role-names '~constructors ~@options)]
     (def ~name dl#)
     dl#))

;;;

(defn compound?
  "Returns true iff given expression is a compound expression."
  [dl-expression]
  (let [expr (expression-term dl-expression)]
    (seq? expr)))

(defn atomic?
  "Returns true iff given expression is an atomic expression."
  [dl-expression]
  (not (compound? dl-expression)))

(defn primitive?
  "Returns true iff given expression consists of a concept name or a
  role name only."
  [dl-expression]
  (and (atomic? dl-expression)
       (or (contains? (concept-names (expression-language dl-expression))
                      (expression-term dl-expression))
           (contains? (role-names (expression-language dl-expression))
                      (expression-term dl-expression)))))

(defn operator
  "Returns the operator of the expression."
  [dl-expression]
  (when-not (compound? dl-expression)
    (illegal-argument "Given expression is atomic and has no operator."))
  (first (expression-term dl-expression)))

(defn arguments
  "Returns the operator arguments of the expression."
  [dl-expression]
  (when-not (compound? dl-expression)
    (illegal-argument "Given expression is atomic and has no arguments."))
  (map #(if-not (dl-expression? %)
          (make-dl-expression-nc (expression-language dl-expression) %)
          %)
       (rest (expression-term dl-expression))))

;;;

(defn symbols-in-expression
  "Returns all symbols used in expressions."
  [dl-expression]
  (let [collector (fn collector [expr]
                    (cond
                     (seq? expr) (vec (reduce concat (map collector (rest expr)))),
                     (dl-expression? expr) (collector (expression-term expr)),
                     :else [expr]))]
    (set (collector (expression-term dl-expression)))))

(defn role-names-in-expression
  "Returns all role names used in the given expression."
  [dl-expression]
  (intersection (role-names (expression-language dl-expression))
                (symbols-in-expression dl-expression)))

(defn concept-names-in-expression
  "Returns all concept names used in the given expression."
  [dl-expression]
  (intersection (concept-names (expression-language dl-expression))
                (symbols-in-expression dl-expression)))

(defn free-symbols-in-expression
  "Returns all free symbols in the given expression."
  [dl-expression]
  (difference (symbols-in-expression dl-expression)
              (union (role-names-in-expression dl-expression)
                     (concept-names-in-expression dl-expression))))

(defn- substitute-syntax
  "Substitues in sexp-1 every occurence of a key in names by its value."
  [sexp-1 names]
  (cond
   (some #{sexp-1} (keys names)) (let [new (names sexp-1)]
                                   (if (dl-expression? new)
                                     (expression-term new)
                                     new)),
   (sequential? sexp-1) (walk #(substitute-syntax % names) identity sexp-1),
   :else sexp-1))

(defn substitute
  "Substitutes in the first dl-expression all occurences of keys in
  names by their values, returning the resulting expression."
  [dl-expr names]
  (make-dl-expression-nc (expression-language dl-expr)
                         (substitute-syntax (expression-term dl-expr) names)))

;;; Definitions

(defrecord DL-definition [target dl-expression])

(defn definition-target
  "Returns target of this definition."
  [^DL-definition definition]
  (.target definition))

(defn definition-expression
  "Returns expression of this definition."
  [^DL-definition definition]
  (.dl-expression definition))

(defmethod print-method DL-definition [definition out]
  (let [^String s (with-out-str
                    (print (definition-target definition))
                    (print " := ")
                    (print (definition-expression definition)))]
    (.write ^java.io.Writer out s)))

(defn make-dl-definition
  "Creates and returns a DL definition."
  ([target definition-expression]
     (when-not (dl-expression? definition-expression)
       (illegal-argument "make-dl-expression requires a valid dl-expression as second arguments."))
     (DL-definition. target definition-expression))
  ([language target definition-sexp]
     (DL-definition. target (make-dl-expression language definition-sexp))))

;;; Subsumptions

(defrecord DL-subsumption [subsumee subsumer])

(defn subsumee
  "Returns the subsumee of the given subsumption."
  [^DL-subsumption subsumption]
  (.subsumee subsumption))

(defmethod premise DL-subsumption [susu]
  #{(subsumee susu)})

(defn subsumer
  "Returns the subsumer of the given subsumption."
  [^DL-subsumption subsumption]
  (.subsumer subsumption))

(defmethod conclusion DL-subsumption [susu]
  #{(subsumer susu)})

(defn make-subsumption
  "Creates and returns a subsumption."
  [C D]
  (when-not (and (dl-expression? C) (dl-expression? D))
    (illegal-argument "Arguments to make-subsumption must be DL-expressions."))
  (DL-subsumption. C D))

(defmethod print-method DL-subsumption [susu out]
  (let [^String output (with-out-str
                          (print (list (subsumee susu)
                                       '==>
                                       (subsumer susu))))]
    (.write ^java.io.Writer out (.trim output))))

(defmacro subsumption
  "Defines a subsumption."
  [DL sexp-for-subsumee sexp-for-subsumer]
  `(make-subsumption (dl-expression ~DL ~sexp-for-subsumee)
                     (dl-expression ~DL ~sexp-for-subsumer)))

(add-dl-syntax! 'subsumption)

;;;

nil
