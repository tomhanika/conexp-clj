;; Copyright ⓒ the conexp-clj developers; all rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.fca.incremental-ganter
  "Implements algorithms from
   'Attribute-incremental construction of the canonical
   implication basis' 2007; Obiedkiv & Duquenne
   https://link.springer.com/article/10.1007/s10472-007-9057-2 "
  (:require [conexp.base :refer :all]
            [conexp.fca.implications :refer :all]
            [conexp.fca.contexts :refer :all]
            [conexp.fca.closure-systems :refer :all]
            [clojure.set :refer [difference union subset? intersection]] ))

;;;

(defn- saturate
  "Given a premise A and implications (B -> C) returns the union of A and all
   elements {b | b in implications and B subset of A}.
   Corresponds with algorithm 2."
  [new-prem implications]
  (loop [new-closure (atom new-prem)
         unused-impl (atom implications)]
    (let [old-closure @new-closure]
      (doall (for [impl @unused-impl]
               (if (subset? (second impl) @new-closure)
                   (do (swap! new-closure union (last impl)) 
                       (swap! unused-impl difference #{impl})))))
      (if (= old-closure @new-closure)
          @new-closure
          (recur new-closure unused-impl)))))  

(defn- fuse 
  "Tries to build an implication from each element in extra-impl and adds them 
   to extra-elements. All implications in extra-elements will be sorted 
   according to order and appended to the elements vector.
   Corresponds with algorithm 10."
  [order basis elements extra-impl extra-elements]
  (doall (for [impl @extra-impl]
           (do (swap! extra-impl difference #{impl})
               (let [other-impl  (union @basis @extra-impl)
                     new-premise (saturate (second impl) other-impl)
                     new-impl    (list (first impl) new-premise (last impl))]
                 (if (proper-subset? new-premise (last impl))
                     (do (swap! basis conj new-impl)
                         (swap! extra-elements conj new-impl)))))))
  (swap! extra-elements #(sort-by (fn [a] (second a)) 
                                  (fn [a b] (lectic-< order a b))
                                  %)) 
  (swap! elements #(vec (concat % @extra-elements))))

(defn- process-modified-concept
  "Corresponds with algorithm 9."
  [y concept elements min-mod mod-concepts]
  (let [min-set (filter
                  #(subset? (second %) (second concept))
                  @min-mod)]
    (if (empty? min-set)
        (let [new-implication (list (first concept)
                                    (second concept)
                                    (conj (second concept) y))]
          (swap! min-mod conj new-implication)
          (swap! elements (fn [a] (vec (remove #{concept} a))))
          (swap! elements conj new-implication))
        (swap! elements (fn [a] (vec (remove #{concept} a))))))
  (swap! mod-concepts conj (list (first concept)
                                 (conj (second concept) y))))

(defn- process-modified-implication
  "Corresponds with algorithm 8."
  [y element elements min-mod non-min-mod]
  (let [implication (list (first element) 
                          (second element) 
                          (conj (last element) y))
        min-set     (filter
                      #(subset? (second %) (second implication))
                      @min-mod)]
    (if (empty? min-set)
        (do (swap! min-mod conj implication)
            (swap! elements (fn [a] (vec (remove #{element} a))))
            (swap! elements conj implication))
        (let [new-implication (list (first implication) 
                                    (conj (second implication) y) 
                                    (last implication))] 
          (swap! elements (fn [a] (vec (remove #{element} a))))
          (swap! non-min-mod conj new-implication)))))

(defn- process-stable-concept
  "Corresponds with algorithm 7."
  [ctx y concept elements current-attributes new-stable]
  (let [new-ext  (intersection (first concept) (attribute-derivation ctx #{y}))
        new-prem (conj (second concept) y)
        new-cons (set (filter
                        #(subset? new-ext (attribute-derivation ctx #{%}))
                        @current-attributes))]
    (if (= new-cons new-prem)
        (swap! elements conj (list new-ext new-prem))
        (if (= new-prem (saturate new-prem @new-stable))
            (do (swap! new-stable conj (list new-ext new-prem new-cons))
                (swap! elements conj (list new-ext new-prem new-cons)))))))

(defn- process-stable-implication
  "Corresponds with algorithm 6."
  [implication old-stable]
  (swap! old-stable conj implication))

(defn add-attribute
  "Given the following updates the implication base accordingly:
    ctx       whole context
    order     order of all attributes
    y         new attribute to be added
    elements  all concepts and implications so far in lexical order
    n         set of all computed elements so far

    Corresponds with algorithm 5."
  [ctx order y elements n]
  (let [old-stable   (atom #{})
        new-stable   (atom #{})
        min-mod      (atom #{})
        non-min-mod  (atom #{})
        mod-concepts (atom #{})]
    (doall
      (for [element @elements]
        (if (subset? (first element) (attribute-derivation ctx #{y}))
            (if (= 2 (count element))
                (process-modified-concept 
                  y element elements min-mod mod-concepts)
                (process-modified-implication 
                  y element elements min-mod non-min-mod))
            (if (= 2 (count element))
                (process-stable-concept ctx y element elements n new-stable)
                (process-stable-implication element old-stable)))))
    (fuse order
          (atom (union @old-stable @new-stable @min-mod))
          elements 
          non-min-mod 
          mod-concepts)))

(defn incremental-ganter
  "Computes the canonical base incrementally. Source:
   'Attribute-incremental construction of the canonical
   implication basis' 2007; Obiedkiv & Duquenne
   https://link.springer.com/article/10.1007/s10472-007-9057-2 
   Corresponds with algorithm 4."
  [ctx]
  (let [elements (atom [(list (objects ctx) #{})])
        order    (attributes ctx)
        n        (atom #{})]
    (doall 
      (for [y order]
        (do (swap! n conj y)
            (add-attribute ctx order y elements n))))
    (for [element @elements :when (= 3 (count element))] 
      (make-implication (second element) (last element)))))

(defalias incremental-stem-base incremental-ganter)
(defalias incremental-canonical-base incremental-ganter)
