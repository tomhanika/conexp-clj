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

(defn- pre-image-measure 
  "Returns the pre-image map of a scale measures function."
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
(defn original-extents [sm]
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

(defn make-smeasure [ctx scale m]
  (let [sm (ScaleMeasure. ctx scale m)]
    (assert (valid-scale-measure? sm) "The Input is no valid Scale Measure")
    sm))

(defn make-smeasure-nc [ctx scale m]
  (ScaleMeasure. ctx scale m))

(defn make-id-smeasure [ctx]
  (make-smeasure-nc ctx ctx identity))

(defn remove-attributes-sm [sm attr]
  (let [s (scale sm)
        new-scale (make-context (objects s) 
                              (disj (attributes s))
                              (incidence s))]
    (make-smeasure-nc (context sm) new-scale (measure sm))))


(defn cluster-attributes-all [sm attr]
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

(defn- valid-cluster [scale original]
  (let [get-exts (fn [cover] (set (map #(get-in cover [% :extent]) (keys cover))))
        ext (get-exts original)]
    (fn [clustered-scale] 
      (let [ext-new (get-exts (transform-bv-cover scale clustered-scale original))]
        (subset? ext-new ext)))))

(defn cluster-attributes-ex [sm attr]
  (let [s (scale sm)
        apply-cluster (fn [at] (make-context (objects s) 
                                           (conj (difference (attributes s) attr) at) 
                                       (fn [a b] 
                                         (if (set? b) 
                                           (some #((incidence s) [a %]) 
                                                 b) 
                                           ((incidence s) [a b])))))
        original (generate-concept-cover (concepts s))
        valid-cluster? (valid-cluster s original)]
    (loop [i 1]
      (let [candidates (comb/combinations (seq (difference (attributes s) attr)) i)
            valids (filter #(valid-cluster? (apply-cluster (into attr %))) candidates)]
        (if (empty? valids)
          (recur (inc i))
          (map #(into attr %) valids))))))

(defn cluster-objects-all [sm obj]
  (let [s (scale sm)
        apply-cluster (fn [o] (make-context (conj (difference (objects s) obj) o) 
                                           (attributes s)
                                           (fn [a b] 
                                             (if (set? a) 
                                               (every? #((incidence s) [% b]) 
                                                     a) 
                                               ((incidence s) [a b])))))
        original (generate-concept-cover (concepts s))
        valid-cluster? (valid-cluster s original)]
    (loop [i 0]
      (let [candidates (comb/combinations (seq (difference (objects s) obj)) i)
            valids (filter #(valid-cluster? (apply-cluster (into obj %))) candidates)]
        (if (empty? valids)
          (recur (inc i))
          (map #(into obj %) valids))))))


(defn cluster-objects-ex [sm obj]
  (let [s (scale sm)
        apply-cluster (fn [o] (make-context (conj (difference (objects s) obj) o) 
                                           (attributes s)
                                           (fn [a b] 
                                             (if (set? a) 
                                               (some #((incidence s) [% b]) 
                                                     a) 
                                               ((incidence s) [a b])))))
        original (generate-concept-cover (concepts s))
        valid-cluster? (valid-cluster s original)]
    (loop [i 0]
      (let [candidates (comb/combinations (seq (difference (objects s) obj)) i)
            valids (filter #(valid-cluster? (apply-cluster (into obj %))) candidates)]
        (if (empty? valids)
          (recur (inc i))
          (map #(into obj %) valids))))))

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

