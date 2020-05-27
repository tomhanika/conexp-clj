;; Copyright ⓒ the conexp-clj developers; all rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.fca.smeasure
  (:require [conexp.base :refer :all]
            [conexp.fca.contexts :refer :all]
            [conexp.fca.concept-transform :refer :all]
            [conexp.fca.cover :refer [generate-concept-cover]]
            [clojure.math.combinatorics :as comb]))

(defprotocol Smeasure
  (context [sm] "Returns the original context that is measured.")
  (scale   [sm] "Returns the scale that measures the context.")
  (measure [sm] "Returns the scale measure map that associates objects of context with objects of scale."))

(deftype ScaleMeasure [context scale measure]
  Object
  (equals [this other]
    (and (= (class this) (class other))
         (= (.context this) (.context ^ScaleMeasure other))
         (every? #(= ((.measure this) %)
                     ((.measure ^ScaleMeasure other) %))
                 (objects context))))
  (hashCode [this]
    (hash-combine-hash ScaleMeasure context scale measure))
  ;;
  Smeasure
  (context [this] context)
  (scale [this] scale)
  (measure [this] measure))

;;; visualization terminal

(defn ^String smeasure-to-string
  "Prints smeasures in a human readable form."
  [sm]
  (let [context (context sm)
        scale   (scale sm)
        mapping (measure sm)
				groups  (group-by #(mapping %) (objects context))
				;;
        ctx-incident? (incidence context)
        sca-incident? (incidence scale)
        ;;
        max-att-ctx (reduce #(max %1 (count (str %2))) 0 (attributes context))
        max-obj-ctx (reduce #(max %1 (count (str %2))) 0 (objects context))
        max-att-sca (reduce #(max %1 (count (str %2))) 0 (attributes scale))
        max-obj-sca (reduce #(max %1 (count (str %2))) 0 (objects scale))
				;;
				seg-line [(ensure-length "" max-obj-ctx "-") "-+"
                  (for [att (attributes context)]
                    (ensure-length "" (inc (count (print-str att))) "-"))
                  "     "
				          (ensure-length "" max-obj-sca "-") "-+"
                  (for [att (attributes scale)]
                    (ensure-length "" (inc (count (print-str att))) "-"))
                  "\n"]]
    (with-str-out
			;; header
      (ensure-length "" max-obj-ctx " ") " |" (for [att (attributes context)]
                                                   [(print-str att) " "])
			"     "
      (ensure-length "" max-obj-ctx " ") " |" (for [att (attributes scale)]
                                                   [(print-str att) " "]) "\n"
			(for [[k group] groups]
				;; first line in group
        [seg-line
         (ensure-length (print-str (first group)) max-obj-ctx)
         " |"
         (for [att (attributes context)]
              [(ensure-length (if (ctx-incident? [(first group) att]) "x" ".")
                              (count (print-str att)))
               " "])
				 " ⟶   "
         (ensure-length (print-str (mapping (first group))) max-obj-sca)
         " |"
         (for [att (attributes scale)]
              [(ensure-length (if (sca-incident? [(mapping(first group)) att]) 
                                  "x" ".")
                              (count (print-str att)))
               " "])
         "\n"
				 ;; remaining lines
				 (for [obj (drop 1 group)]
              [(ensure-length (print-str obj) max-obj-ctx) " |"
               (for [att (attributes context)]
                    [(ensure-length (if (ctx-incident? [obj att]) "x" ".")
                                    (count (print-str att)))
                     " "])
							 "     "
               (ensure-length "" max-obj-ctx " ") " |"
               (for [att (attributes scale)]
                    [(ensure-length "" (count (print-str att)) " ")
                     " "])
               "\n"])]))))

(defn print-smeasure
  "Prints the result of applying smeasure-to-string to the given
   smeasure."
  [sm]
  (print (smeasure-to-string sm)))

(defmethod print-method ScaleMeasure [sm out]
  (.write ^java.io.Writer out
          ^String (smeasure-to-string sm)))

;;;

(defn- pre-image-measure
  "Returns the pre-image map of a scale measures function sigma."
  [sm]
  (let [m (measure sm)]
    (if (map? m)
      (apply (partial merge-with into) {}
             (for [[k v] m] {v #{k}}))
      (let [mapified (into {}
                           (for [obj (objects (context sm))]
                             [obj ((measure sm) obj)]))]
        (apply (partial merge-with into) {}
             (for [[k v] mapified] {v #{k}}))))))

(defn original-extents
  "Returns the pre-image of all extents whichs image is closed in the
  scale."
  [sm]
  (let [scale-extents (extents (scale sm))
        pre-image (pre-image-measure sm)]
    (map #(set (reduce into (map pre-image %)))
            scale-extents)))


(defn valid-scale-measure?
  "Checks if the input is a valid scale measure."
  [sm]
  (let [pre-extents (original-extents sm)]
    (every? #(extent? (context sm) %)
            pre-extents)))

(defn smeasure?
  "Checks if the input is a valid scale measure."
  [sm]
  (and (instance? ScaleMeasure sm)
       (valid-scale-measure? sm)))

(defn make-smeasure
  "Returns a scale-measure object of the input is a valid scale measure."
  [ctx scale m]
  (let [sm (ScaleMeasure. ctx scale m)]
    (assert (valid-scale-measure? sm) "The Input is no valid Scale Measure")
    sm))

(defn make-smeasure-nc
  "Generates a scale measure object without checking the validity."
  [ctx scale m]
  (ScaleMeasure. ctx scale m))

(defn make-id-smeasure
  "Generates a scale-measure with the identity map and the context as scale."
  [ctx]
  (make-smeasure-nc ctx ctx identity))

(defn remove-attributes-sm
  "Removes 'attr attributes from the scale."
  [sm attr]
  (let [s (scale sm)
        new-scale (make-context (objects s)
                              (difference (attributes s) attr)
                              (incidence s))]
    (make-smeasure-nc (context sm) new-scale (measure sm))))


(defn cluster-attributes-all
  "Clusters 'attr attributes in the scale context.
  For example the attributes #{1 2 3} become #{[1 2] 3} in the scale.
  The new incidence is build such that (g, [attr]) if (g,m) for all m in attr."
  [sm attr]
  (let [ctx (context sm)
        s (scale sm)]
    (make-smeasure-nc (context sm)
                      (make-context (objects s)
                                    (conj (difference (set (attributes s)) attr)
                                          attr)
                                    (fn [a b]
                                      (if (set? b)
                                        (every? #((incidence s) [a %])
                                              b)
                                        ((incidence s) [a b]))))
                      (measure sm))))

(defn- valid-cluster
  "This function is a predicate factory for valid scale measure clustering."
  [scale original]
  (let [get-exts (fn [cover] (set (map #(get-in cover [% :extent]) (keys cover))))
        ext (get-exts original)]
    (fn [clustered-scale pre-image]
      (let [ext-new (get-exts (transform-bv-cover scale clustered-scale original))]
        (subset? (set (map pre-image ext-new)) ext)))))

(defn cluster-attributes-ex
  "Clusters 'attr attributes in the scale context.
  For example the attributes #{1 2 3} become #{[1 2] 3} in the scale.
  The new incidence is build such that (g, [attr]) if (g,m) for some m
  in attr.  If the 'attr cluster does not form a valid scale measure,
  a sequence of valid supersets of lowest cardinality is returned."
  [sm attr]
  (let [s (scale sm)
        apply-cluster (fn [at] (make-context (objects s)
                                           (conj (difference (attributes s) at) at)
                                           (fn [a b]
                                             (if (set? b)
                                               (some #((incidence s) [a %])
                                                     b)
                                               ((incidence s) [a b])))))
        original (generate-concept-cover (concepts s))
        comp-scale-image identity
        scale-pre-image identity
        valid-cluster? (valid-cluster s original)]
    (loop [i 0]
      (let [candidates (comb/combinations (seq (difference (attributes s) attr)) i)
            valids (filter #(valid-cluster? (apply-cluster (into attr %)) scale-pre-image) candidates)]
        (if (empty? valids)
          (recur (inc i))
          (if (empty? (first valids))
            (let [new-scale (apply-cluster attr)]
              (make-smeasure-nc (context sm)
                                (make-context (objects new-scale)
                                              (attributes new-scale)
                                              (incidence-relation new-scale))
                                (comp comp-scale-image (measure sm))))
            (map #(into attr %) valids)))))))

(defn cluster-objects-all
  "Clusters 'obj objects in the scale context.
  For example the attributes #{1 2 3} become #{[1 2] 3} in the scale.
  The new incidence is build such that ([obj],m) if (g,m) for all g
  in obj.  If the 'obj cluster does not form a valid scale measure,
  a sequence of valid supersets of lowest cardinality is returned."
  [sm obj]
  (let [s (scale sm)
        apply-cluster (fn [o] (make-context (conj (difference (objects s) o) o)
                                           (attributes s)
                                           (fn [a b]
                                             (if (set? a)
                                               (every? #((incidence s) [% b])
                                                     a)
                                               ((incidence s) [a b])))))
        original (generate-concept-cover (concepts s))
        comp-scale-image (fn [g] (if (get obj g) obj g))
        scale-pre-image (fn [o] (fn [oset] (reduce #(if (= o %2) (into %1 %2) (conj %1 %2)) #{} oset)))
        valid-cluster? (valid-cluster s original)]
    (loop [i 0]
      (let [candidates (comb/combinations (seq (difference (objects s) obj)) i)
            valids (filter #(valid-cluster? (apply-cluster (into obj %)) (scale-pre-image (into obj %))) candidates)]
        (if (empty? valids)
          (recur (inc i))
          (if (empty? (first valids))
            (let [new-scale (apply-cluster obj)]
              (make-smeasure-nc (context sm)
                                (make-context (objects new-scale)
                                              (attributes new-scale)
                                              (incidence-relation new-scale))
                                (comp comp-scale-image (measure sm))))
            (map #(into obj %) valids)))))))


(defn cluster-objects-ex
  "Clusters 'obj objects in the scale context.
  For example the attributes #{1 2 3} become #{[1 2] 3} in the scale.
  The new incidence is build such that ([obj],m) if (g,m) for some g
  in obj.  If the 'obj cluster does not form a valid scale measure,
  a sequence of valid supersets of lowest cardinality is returned."
  [sm obj]
  (let [s (scale sm)
        apply-cluster (fn [o] (make-context (conj (difference (objects s) o) o)
                                           (attributes s)
                                           (fn [a b]
                                             (if (set? a)
                                               (some #((incidence s) [% b])
                                                     a)
                                               ((incidence s) [a b])))))
        original (generate-concept-cover (concepts s))
        comp-scale-image (fn [g] (if (get obj g) obj g))
        scale-pre-image (fn [o] (fn [oset] (reduce #(if (= o %2) (into %1 %2) (conj %1 %2)) #{} oset)))
        valid-cluster? (valid-cluster s original)]
    (loop [i 0]
      (let [candidates (comb/combinations (seq (difference (objects s) obj)) i)
            valids (filter #(valid-cluster? (apply-cluster (into obj %)) (scale-pre-image (into obj %))) candidates)]
        (if (empty? valids)
          (recur (inc i))
          (if (empty? (first valids))
            (let [new-scale (apply-cluster obj)]
              (make-smeasure-nc (context sm)
                                (make-context (objects new-scale)
                                              (attributes new-scale)
                                              (incidence-relation new-scale))
                                (comp comp-scale-image (measure sm))))
            (map #(into obj %) valids)))))))

(defn rename-scale-objects
  "Renames objects in the scale. Input the renaming as function on the
  set of objects or as key value pairs."
  ([sm rename-fn]
   (make-smeasure-nc (context sm)
                       (rename-objects (scale sm) rename-fn)
                       (comp rename-fn (measure sm))))
  ([sm key val & keyvals]
   (let [rename-map (apply hash-map key val keyvals)
         rename-fn  (fn [o] (or (get rename-map o) o))]
     (rename-scale-objects sm rename-fn))))

(defn rename-scale-attributes
  "Renames attribute in the scale. Input the renaming as function on the
  set of attributes or as key value pairs."
  ([sm rename-fn]
   (make-smeasure-nc (context sm)
                     (rename-attributes (scale sm) rename-fn)
                     (measure sm)))
  ([sm key val & keyvals]
   (let [rename-map (apply hash-map key val keyvals)
         rename-fn  (fn [a] (or (get rename-map a) a))]
     (rename-scale-attributes sm rename-fn))))

;; (defn cluster-attributes-ex [sm attr]
;;   (let [ctx (context sm)
;;         s (scale sm)]
;;     (make-smeasure-nc (context sm)
;;                       (make-context (objects s)
;;                                     (conj (difference (set (attributes s)) attr)
;;                                           attr)
;;                                     (fn [a b]
;;                                       (if (set? b)
;;                                         (some #((incidence s) [a %])
;;                                               b)
;;                                         ((incidence s) [a b]))))
;;                       identity)))


;; (defn cluster-objects-ex [sm obj]
;;   (let [ctx (context sm)
;;         s (scale sm)]
;;     (make-smeasure-nc (context sm)
;;                       (make-context (conj (difference (set (objects s)) obj)
;;                                           obj)
;;                                     (attributes s)
;;                                     (fn [a b]
;;                                       (if (set? a)
;;                                         (some #((incidence s) [% b])
;;                                               a)
;;                                         ((incidence s) [a b]))))
;;                       (fn [a] (if (get obj a) obj a)))))

;; (defn cluster-objects-all-nc [sm obj]
;;   (let [ctx (context sm)
;;         s (scale sm)]
;;     (make-smeasure-nc (context sm)
;;                       (make-context (conj (difference (set (objects s)) obj)
;;                                           obj)
;;                                     (attributes s)
;;                                     (fn [a b]
;;                                       (if (set? a)
;;                                         (every? #((incidence s) [% b])
;;                                               a)
;;                                         ((incidence s) [a b]))))
;;                       (fn [a] (if (get obj a) obj a)))))

;; (defn- build-obj-all-cluster [obj exts]
;;   (let [new-cluster (reduce into obj
;;                                 (filter #(and (not (empty? (intersection obj %)))
;;                                               (proper-subset? (intersection obj %) obj))
;;                                         exts))]
;;         (if (= obj new-cluster)
;;           obj
;;           (build-obj-all-cluster new-cluster exts))))

;; (defn cluster-objects-all [sm obj]
;;   (let [cluster (build-obj-all-cluster obj (extents (scale sm)))]
;;     (println cluster)
;;     (assert
;;      (not (some #(= (attributes (scale sm)) (object-derivation (scale sm) #{%})) cluster))
;;      "No valid Object Clustering possible.")
;;     (cluster-objects-all-nc sm cluster)))

