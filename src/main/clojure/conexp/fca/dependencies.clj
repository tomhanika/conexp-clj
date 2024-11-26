;; Copyright ⓒ the conexp-clj developers; all rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.fca.dependencies
  "Dependencies for Formal Concept Analysis, based on
   'Dependencies of many-valued attributes', 1987, Rudolf Wille
   http://www.opengrey.eu/item/display/10068/148205 "
  (:require [clojure.core.reducers :as r]
            [clojure.set :refer [intersection subset?]]
            [conexp.base :refer :all]
            [conexp.fca.implications :refer [premise conclusion
                                             stem-base-from-base
                                             make-implication]]
            [conexp.fca.contexts :refer :all]
            [conexp.fca.many-valued-contexts :refer :all]))

;;; validation methods

(defn dependent?
  "Checks if a dependency between attributes is valid. Attributes depend
   on another set of attributes if they can be expressed by them, i.e. 
   given a context (G,M,I), the set Y depends on the set X of attributes
   if every extent of the concepts of (G,M,I_{X u Y}) is an extent of
   a concet of (G,M,I_{X}).
   Scales must be given as map between attributes and contexts."
  [mv-ctx scales impl]
  (let [default (fn [a] (make-context-nc #{} #{} #{}))
        ctx-pc  (scale-mv-context 
                  mv-ctx 
                  (select-keys scales (concat (conclusion impl) 
                                              (premise impl)))
                  default)
        ctx-c  (scale-mv-context 
                  mv-ctx 
                  (select-keys scales (conclusion impl))
                  default)]
    (= (extents ctx-c)
       (extents ctx-pc))))

(defn weakly-dependent?
  "Checks if a weak dependency is valid. Given a context (G,M,I), a set of 
   attributes Y is weakly dependent on X if every extent of a concept in
   (G,M,I_{Y}) is also an extent of a concept of (G,M,I_{X}).
   Scales must be given as map between attributes and contexts."
  [mv-ctx scales impl]
  (let [default (fn [a] (make-context-nc #{} #{} #{}))
        ctx-p  (scale-mv-context 
                  mv-ctx 
                  (select-keys scales (premise impl))
                  default)
        ctx-c  (scale-mv-context 
                  mv-ctx 
                  (select-keys scales (conclusion impl))
                  default)]
    (subset? (set (extents ctx-p))
             (set (extents ctx-c)))))

(defn functional-dependent?
  "Checks if a functional dependency is valid. A set Y is functinally dependet
   on a set X of a mv-context if for some objects g and h
   x(g) = x(h) for all x in X implies y(g) = y(h) for all y in Y."
  [mv-ctx impl]
  (let [inz   (incidence mv-ctx)
        objs  (objects mv-ctx)
        prem  (vec (premise impl))
        concl (vec (conclusion impl))
        prem-inz  (mapv (fn[a] (mapv (fn [b] (get inz [a b])) prem)) objs)
        concl-inz (mapv (fn[a] (mapv (fn [b] (get inz [a b])) concl)) objs)]
    ; 'A' functional dependent on 'B' iff incidence 'A' has the same number of
    ; unique lines as the semi.prod of 'A' and 'B'
    (= (count (set prem-inz)) 
       (count (set (map #(concat %1 %2) prem-inz concl-inz))))))

;;; mv-ctx dependencies

(defn all-dependencies
  "Given the many-valued context returns the set of all functional 
   dependencies. Optionally scales may be given as a map between attributes
   and their scale context. If scales are given a third attribute may
   indicate :strong or :weak dependency with :strong being the default for
   normal dependency."
  ([mv-ctx]
    (let [powerset  (remove empty?  (subsets (attributes mv-ctx)))]
      (set 
        (filter identity
          (for [x powerset y powerset :when (empty? (intersection x y))]
            (let [impl (make-implication y x)]
              (if (functional-dependent? mv-ctx impl)
                  impl
                  nil)))))))
  ([mv-ctx scales]
    (all-dependencies mv-ctx scales :strong))
  ([mv-ctx scales type]
    (let [powerset (remove empty? (subsets (attributes mv-ctx)))]
      (set
        (filter identity
          (for [x powerset y powerset :when (empty? (intersection x y))]
            (let [impl (make-implication y x)]
              (case type
                :strong 
                  (if (dependent? mv-ctx scales impl) impl nil)
                :weak
                  (if (weakly-dependent? mv-ctx scales impl) impl nil)
                (illegal-argument 
                  (str "No dependency type " type "."))))))))))

(defn dependencies
  "Given the many-valued context and scales as map between attributes and their
   scale context returns the set of all non-redundant dependencies. The third
   argument may indicate :strong or :weak dependency with :strong being
   the default for normal dependeny."
  ([mv-ctx scales]
    (dependencies mv-ctx scales :strong))
  ([mv-ctx scales type]
    (stem-base-from-base (all-dependencies mv-ctx scales type))))

(defn functional-dependencies
  "Returns the set of all non-redundant functional dependencies."
  [mv-ctx]
  (stem-base-from-base (all-dependencies mv-ctx)))

;;; end

true
