;; Copyright (c) Daniel Borchmann. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.contrib.java
  (:use conexp.base)
  (:use clojure.java.io)
  (:require conexp.main
            clojure.pprint))

;;;

(defn- to-valid-Java-name
  "Convert old-name to a valid Java variable name."
  [old-name]
  (let [new-name (-> (str old-name)
                     (.replaceAll "-" "_")
                     (.replaceAll " " "_")
                     (.replaceAll "!" "_f")   ;assume ! only at end
                     (.replaceAll "\\?" "_p") ;assume ? only at end
                     (.replaceAll "<" "_lt_")
                     (.replaceAll ">" "_gt_")
                     (.replaceAll "=" "_eq_"))]
    (symbol new-name)))

(defn- conexp-functions
  "Returns a hash-map of function names to vars of ns. The function
  names are converted by to-valid-Java-name."
  [ns]
  (let [public-map (ns-publics ns)]
    (reduce! (fn [map [sym, ^clojure.lang.Var var]]
               (if-not (.isMacro var)
                 (conj! map [(to-valid-Java-name sym) var])
                 map))
             {}
             public-map)))

(defn- dissect-arglist
  [arglist]
  (let [[a b] (split-with #(not= '& %) arglist)]
    [a (rest b)]))

(defn- function-signatures
  "Returns sequence of function signatures, each being suitable for
  gen-class."
  [new-name, ^clojure.lang.Var var]
  (let [arglists (:arglists (meta var)),
        return   (or (:tag (meta var)) 'Object),
        tag      (fn [x]
                   (let [tag (:tag (meta x))]
                     (if-not tag
                       'Object
                       (case tag
                         Lattice conexp.fca.lattices.Lattice
                         Context conexp.fca.contexts.Context
                         Association-Rule conexp.fca.association_rules.Association-Rule
                         tag))))]
    (if-not arglists
      nil
      (for [arglist arglists]
        (let [arglist-split (dissect-arglist arglist)]
          [new-name
           (if (empty? (second arglist-split))
             (vec (map tag (first arglist-split)))
             (conj (vec (map tag (first arglist-split)))
                   "[Ljava.lang.Object;"))
           return])))))

(defn- generate-definition
  "Generates function definition with name new-name, calling orig-name
  with supplied arguments. Prepends prefix before new-name"
  [prefix new-name orig-name & arglists]
  `(defn ~(symbol (str prefix new-name))
     ~@(for [args arglists]
         (let [arglist-split (dissect-arglist args)]
           `(~args
             ~(if (not-empty (second arglist-split))
                `(apply ~(symbol orig-name) ~@(first arglist-split) ~@(second arglist-split))
                `(~(symbol orig-name) ~@(first arglist-split))))))))

;;;

(defn generate-java-interface
  "Given a name of a file generates the code for the Java interface in
  that file. After this has been compiled it can be used to call
  conexp-clj functions from Java."
  [orig-ns new-ns file-name]
  (let [methods (mapcat #(apply function-signatures %)
                        (conexp-functions orig-ns)),
        methods (mapcat #(list (symbol "^{:static true}") %) methods)]
    (with-open [out (writer file-name)]
      (binding [*print-meta* true
                *out* out]
        (clojure.pprint/pprint
         `(do
            (ns ~new-ns
              (:require conexp.main)
              (:gen-class
               :prefix ~'conexp-clj-
               :methods ~methods))

            (import 'conexp.fca.contexts.Context)
            (import 'conexp.fca.lattices.Lattice)
            (import 'conexp.fca.association_rules.Association-Rule)

            ~@(for [[new-name, ^clojure.lang.Var var] (conexp-functions orig-ns)]
                (let [orig-name (symbol (str (.ns var)) (str (.sym var))),
                      arglists  (:arglists (meta var))]
                  (apply generate-definition
                         "conexp-clj-"
                         new-name
                         orig-name
                         arglists)))))))
    (compile new-ns)))

;;;

(generate-java-interface 'conexp.main
                         'conexp.contrib.java.Main
                         "src/conexp/contrib/java/Main.clj")

;;;

nil
