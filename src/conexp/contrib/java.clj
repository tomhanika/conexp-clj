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
                     (.replaceAll "!" "")
                     (.replaceAll "<" "_lt_")
                     (.replaceAll ">" "_gt_")
                     (.replaceAll "=" "_eq_"))]
    (symbol
     (if (.endsWith new-name "?")
       (if-not (.startsWith new-name "has")
         (str "is_" (subs new-name 0 (dec (count new-name))))
         (subs new-name 0 (dec (count new-name))))
       new-name))))

(defvar- conexp-functions
  (let [public-map (ns-publics 'conexp.main)]
    (reduce! (fn [map [sym, ^clojure.lang.Var var]]
               (if-not (.isMacro var)
                 (conj! map [(to-valid-Java-name sym) var])
                 map))
             {}
             public-map))
  "All functions exported by conexp-clj, as a set of Vars.")

(defn- dissect-arglist
  [arglist]
  (let [[a b] (split-with #(not= '& %) arglist)]
    [a (rest b)]))

(defn- function-signatures
  "Returns sequence of function signatures, each being suitable for
  gen-class."
  [new-name, ^clojure.lang.Var var]
  (let [arglists (:arglists (meta var)),
        return   (or (:tag (meta var)) 'Object)]
    (if-not arglists
      nil
      (for [arglist arglists]
        (let [arglist-split (dissect-arglist arglist)]
          [new-name
           (if (empty? (second arglist-split))
             (vec (map (constantly 'Object)
                       (first arglist-split)))
             (conj (vec (map (constantly 'Object)
                             (first arglist-split)))
                   'objects))
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
  [file-name]
  (with-open [out (writer file-name)]
    (binding [*out* out]
      (clojure.pprint/pprint
       `(do
          (ns ~'conexp.contrib.JavaInterface
            (:require conexp.main)
            (:gen-class
             :prefix ~'conexp-clj-
             :methods ~(map #(with-meta % {:static true})
                            (mapcat #(apply function-signatures %)
                                    conexp-functions))))

          (import 'conexp.fca.contexts.Context)
          (import 'conexp.fca.lattices.Lattice)
          (import 'conexp.fca.association_rules.Association-Rule)

          ~@(for [[new-name, ^clojure.lang.Var var] conexp-functions]
              (let [orig-name (symbol (str (.ns var)) (str (.sym var))),
                    arglists  (:arglists (meta var))]
                (apply generate-definition
                       "conexp-clj-"
                       new-name
                       orig-name
                       arglists))))))))

;;;

nil
