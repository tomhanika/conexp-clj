;; Copyright ⓒ the conexp-clj developers; all rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.fca.many-valued-contexts
  "Many-Valued-Contexts and some functions for scaling."
  (:require [conexp.base :refer :all]
            [conexp.fca.contexts :refer :all]))

;;;

(deftype Many-Valued-Context [objects attributes incidence]
  Object
  (equals [this other]
    (generic-equals [this other] Many-Valued-Context [objects attributes incidence]))
  (hashCode [this]
    (hash-combine-hash Many-Valued-Context objects attributes incidence))
  ;;
  Context
  (objects [this] objects)
  (attributes [this] attributes)
  (incidence [this] incidence))

(defn mv-context-to-string
  "Returns a string representing the given many-valued context mv-ctx
  as a value-table."
  ([mv-ctx]
     (mv-context-to-string mv-ctx sort-by-first sort-by-first))
  ([mv-ctx order-on-objects order-on-attributes]
     (let [objs         (sort order-on-objects (objects mv-ctx)),
           atts         (sort order-on-attributes (attributes mv-ctx)),
           inz          (incidence mv-ctx),

           str          #(if (nil? %) "nil" (str %))

           max-obj-len  (reduce #(max %1 (count (str %2))) 0 objs)
           max-att-lens (loop [lens   (transient (map-by-fn #(count (str %)) atts)),
                               values (seq inz)]
                          (if values
                            (let [[[_ m] w] (first values),
                                  len (count (str w))]
                              (recur (if (> len (lens m))
                                       (assoc! lens m len)
                                       lens)
                                     (next values)))
                            (persistent! lens)))]
       (with-str-out
         (ensure-length "" max-obj-len " ") " |" (for [att atts]
                                                   [(ensure-length (str att) (max-att-lens att) " ") " "])
         "\n"
         (ensure-length "" max-obj-len "-") "-+" (for [att atts]
                                                   (ensure-length "" (inc (max-att-lens att)) "-"))
         "\n"
         (for [obj objs]
           [(ensure-length (str obj) max-obj-len)
            " |"
            (for [att atts]
              [(ensure-length (str (inz [obj att])) (max-att-lens att))
               " "])
            "\n"])))))

(defmethod print-method Many-Valued-Context [mv-ctx out]
  (.write ^java.io.Writer out ^String (mv-context-to-string mv-ctx)))

;;;

(defmulti make-mv-context
  "Constructs a many-valued context from a set of objects, a set of
  attributes and an incidence relation, given as set of triples [g m w]
  or as a function from two arguments g and m to values w."
  {:arglists '([objects attributes incidence])}
  (fn [& args] (vec (map clojure-type args))))

(defmethod make-mv-context [Object Object clojure-coll]
  [objs atts inz]
  (let [objs (to-set objs),
        atts (to-set atts)]
    (Many-Valued-Context. objs
                          atts
                          (if (map? inz)
                            (do
                              (when-not (= (cross-product objs atts) (set (keys inz)))
                                (illegal-argument "Incidence map for many-value-context must be total "
                                                  "and must not contain additional keys."))
                              inz)
                            (loop [hash  (transient {}),
                                   items inz]
                              (if (empty? items)
                                (persistent! hash)
                                (let [[g m w] (first items)]
                                  (recur (if (and (contains? objs g)
                                                  (contains? atts m))
                                           (assoc! hash [g m] w)
                                           hash)
                                         (rest items)))))))))

(defmethod make-mv-context [Object Object clojure-fn]
  [objs atts inz-fn]
  (let [objs (to-set objs),
        atts (to-set atts)]
    (Many-Valued-Context. objs
                          atts
                          (map-by-fn (fn [[g m]]
                                       (inz-fn g m))
                                     (cross-product objs atts)))))

(defmethod make-mv-context :default [objs atts inz]
  (illegal-argument "No method defined for types "
                    (clojure-type objs) ", "
                    (clojure-type atts) ", "
                    (clojure-type vals) ", "
                    (clojure-type inz) "."))

(defn make-mv-context-from-matrix
  "Creates a many-valued context from a given matrix of
  values. objects and attributes may either be given as numbers
  representing the corresponding number of objects and attributes
  respectively, or as collections. The number of entries in values
  must match the number of objects times the number of attributes."
  [objects attributes values]
  (let [objects    (ensure-seq objects),
        attributes (ensure-seq attributes),
        m          (count objects),
        n          (count attributes)]
    (assert (= (* m n) (count values)))
    (let [entries (into {} (for [i (range m), j (range n)]
                             [[(nth objects i) (nth attributes j)] (nth values (+ (* n i) j))]))]
      (make-mv-context objects attributes (fn [a b] (entries [a b]))))))

(defn make-mv-context-nc
  "Just creates a many-valued context from a set of objects, a set of
  attributes and a hashmap from pairs of objects and attributes to
  values. Does no checking, use with care."
  [objects attributes incidence]
  (assert (map? incidence))
  (Many-Valued-Context. (to-set objects) (to-set attributes) incidence))

;;;

(defn values-of-attribute
  "For a given many-valued context mv-ctx and a given attribute m,
  returns the set of all values of m in mv-ctx."
  [mv-ctx m]
  (when-not (contains? (attributes mv-ctx) m)
    (illegal-argument "Given element is not an attribute of the given many-valued context."))
  (let [inz (incidence mv-ctx)]
    (set-of (inz [g m]) [g (objects mv-ctx)])))

(defn values-of-object
  "For a given many-valued context mv-ctx and a given object g,
  returns the set of all values of g in mv-ctx."
  [mv-ctx g]
  (when-not (contains? (objects mv-ctx) g)
    (illegal-argument "Given element is not an object of the given many-valued context."))
  (let [inz (incidence mv-ctx)]
    (set-of (inz [g m]) [m (attributes mv-ctx)])))

;;;

(defn scale-mv-context
  "Scales given many-valued context mv-ctx with given scales. scales must be a map from attributes m
  to contexts K, where all possible values of m in mv-ctx are among the objects in K. If a scale for
  an attribute is given, the default scale is used, where default should be a function returning a
  scale for the supplied attribute. If no default scale is given, an error is thrown if an attribute
  is missing."
  ([mv-ctx scales]
     (scale-mv-context mv-ctx scales #(illegal-argument "No scale given for attribute " % ".")))
  ([mv-ctx scales default]
     (assert (map? scales))
     (let [scale  (fn [m]
                    (if (contains? scales m)
                      (scales m)
                      (default m)))
           inz    (incidence mv-ctx),
           objs   (objects mv-ctx),
           atts   (set-of [m n] [m (attributes mv-ctx)
                                 n (attributes (scale m))])]
       (make-context-nc objs
                        atts
                        (fn [[g [m n]]]
                          (let [w (inz [g m])]
                            ((incidence (scale m)) [w n])))))))

(defn nominal-scale
  "Returns the nominal scale on the set base."
  ([values]
     (nominal-scale values values))
  ([values others]
     (make-context values others =)))

(defn ordinal-scale
  "Returns the ordinal scale on the set values, optionally given an
  order relation <=."
  ([values]
   (ordinal-scale values <=))
  ([values <=]
   (ordinal-scale values values <=))
  ([values others <=]
   (let [atts (map #(vector '<= %) others),
         inz  (fn [g [_ m]]
                (<= g m))]
     (make-context values atts inz))))

(defn interordinal-scale
  "Returns the interordinal scale on the set base, optionally given
  two order relations <= and >=."
  ([values]
   (interordinal-scale values <= >=))
  ([values <= >=]
   (interordinal-scale values values <= >=))
  ([values others <= >=]
   (let [objs    values,

         atts-<= (map #(vector '<= %) others),
         atts->= (map #(vector '>= %) others),

         inz     (fn [g [test m]]
                   (if (= test '<=)
                     (<= g m)
                     (>= g m)))]
     (make-context values (union atts-<= atts->=) inz))))

(defn biordinal-scale
  "Returns the biordinal scale on the sequence values, optionally given
  two order relations <= and >=. Note that values (and others) must be
  ordered (e.g. vector or list), because otherwise the result will be
  arbitrary."
  ([values n]
     (biordinal-scale values values n <= >=))
  ([values others n <= >=]
     (let [first-objs (take n values),
           rest-objs  (drop n values),

           first-atts (map #(vector '<= %) (take n others)),
           rest-atts  (map #(vector '>= %) (drop n others)),

           inz        (fn [g [test m]]
                        (if (= test '<=)
                          (<= g m)
                          (>= g m)))]
       (make-context values (union first-atts rest-atts) inz))))

(defn dichotomic-scale
  "Returns the dichotimic scale on the set values. Note that base must
  have exactly two arguments."
  [values]
  (assert (= 2 (count values)))
  (nominal-scale values))

(defn interval-scale
  "Returns the interval scale on the set values.
  Note that values must be ordered (e.g. vector or list), because otherwise the result
  will be arbitrary.  Also note that the intervales will be left-open."
  ([values]
    (interval-scale values values < >=))
  ([values others]
    (interval-scale values others < >=))
  ([values others < >=]
     (assert (sequential? others)
             "Interval values must be ordered to obtain a reasonable result.")
     (let [pairs      (partition 2 1 others)
           atts       (map #(vector '∈ (vec  %)) pairs)
           inz        (fn [g [_ [a b]]]
                        (and (< g b)
                             (>= g a)))]
       (make-context values atts inz))))

;;;

(defmacro scale-mv-context-with
  "Scales the given many-valued context ctx with the given scales. These are of the form

    [att_1 att_2 ...] scale,

  where att_i is an attribute of the given context and scale determines a call to a known
  scale. The variable «values» will be bound to the corresponding values of each attribute
  and may be used when constructing the scale. For example, you may use this macro with

    (scale-mv-context-with ctx
      [a b c]  (nominal-scale values)
      [d]      (ordinal-scale values <=)
      (nominal-scale values))

  where the last entry (without any associated attribute) is the default scale.  Note that
  attributes of ctx always have to be given in a sequence, even if there is only one."
  [ctx & scales]
  (let [default      (if (odd? (count scales))
                       (last scales)
                       nil),
        scales       (partition 2 (if default (butlast scales) scales)),
        given-atts   (mapcat first scales)]
    (when (not= given-atts (distinct given-atts))
      (illegal-argument "Doubly given attribute."))
    `(do
       ~(when-not default
          `(when-not (= (attributes ~ctx) '~(set given-atts))
             (illegal-argument "Given scales to scale-context do not "
                               "yield the attribute set of the given context.")))
       (scale-mv-context ~ctx
                         ~(into {}
                                (for [[atts scale] scales,
                                      att atts]
                                  `['~att (let [~'values (values-of-attribute ~ctx '~att)]
                                            ~scale)]))
                         (memoize (fn [x#]
                                    (let [~'values (values-of-attribute ~ctx x#)]
                                      ~default)))))))

;;;

nil
