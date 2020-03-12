;; Copyright ⓒ the conexp-clj developers; all rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.fca.smeasure
  (:require [conexp.base :refer :all] 
            [conexp.fca.contexts :refer :all]))

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



(defn make-smeasure-nc [ctx scale m]
  (ScaleMeasure. ctx scale m))

(defn make-id-smeasure [ctx]
  (make-smeasure-nc ctx ctx identity))

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

(defn cluster-attributes-ex [sm attr]
  (let [ctx (context sm)
        s (scale sm)]
    (make-smeasure-nc (context sm) 
                      (make-context (objects s) 
                                    (conj (difference (set (attributes s)) attr)
                                          attr) 
                                    (fn [a b] 
                                      (if (set? b) 
                                        (some #((incidence s) [a %]) 
                                              b) 
                                        ((incidence s) [a b])))) 
                      identity)))

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
                      identity)))
