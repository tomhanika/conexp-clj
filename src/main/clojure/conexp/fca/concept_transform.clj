;; Copyright ⓒ the conexp-clj developers; all rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.fca.concept-transform
  (:require [conexp.base :refer :all] 
            [conexp.fca.contexts :refer :all] 
            [conexp.fca.cover :refer :all]
            [clojure.set :refer [difference union subset? intersection]]))



;;; transformer algorithms using the cover structure

(defn transform-bv-cover
  "Transforms the concept lattice of ctx1 to that of ctx2 using the
  algorithm presented in 'Knowledge Cores in Large Formal Contexts'
  The methods input are the two contexts and the concept lattice of
  ctx1 as cover relation generated by cover.clj. For ctx1 and ctx2 it
  required that there exists a super context ctx such that ctx1 and
  ctx2 are induced subcontexts of ctx.

  This method is useful for larger contexts ctx1 and ctx2 which have
  many objects and attributes in common."
  [ctx1 ctx2 bv1]
  (let [;; update the set of attributes of the ctx and bv first
        shared-attributes (intersection (attributes ctx2) (attributes ctx1))
        remove-ctx1-attributes-bv (attribute-deletion-cover bv1 ctx1 (difference (attributes ctx1) shared-attributes))
        ctx2-only-attributes (difference (attributes ctx2) (attributes ctx1))
        attr-intermediate-ctx (make-context (objects ctx1) (attributes ctx2) 
                                            (fn [a b] (if (contains? (objects ctx2) a) 
                                                        ((incidence ctx2) [a b])
                                                        ((incidence ctx1) [a b]))))
        insert-ctx2-attributes-bv (attribute-insertion-cover remove-ctx1-attributes-bv attr-intermediate-ctx ctx2-only-attributes)
        ;; To update all objects, we update the attributes of the dual
        ;; context
        dual-bv (dual-concept-cover insert-ctx2-attributes-bv)
        shared-objects (intersection (objects ctx1) (objects ctx2))
        dual-ctx (dual-context attr-intermediate-ctx)
        remove-ctx1-objects-bv (attribute-deletion-cover 
                                dual-bv dual-ctx
                                (difference (objects ctx1) shared-objects))
        ctx2-only-objects (difference (objects ctx2) (objects ctx1))
        insert-ctx2-objects-bv (attribute-insertion-cover remove-ctx1-objects-bv (dual-context ctx2) ctx2-only-objects)]
     (dual-concept-cover insert-ctx2-objects-bv)))

(defn transform-bv-subctx-cover
  "Transforms the concept lattice of ctx1 to that of ctx2 using the
  algorithm presented in 'Knowledge Cores in Large Formal Contexts'
  The methods input are the two contexts and the concept lattice of
  ctx1 as cover relation generated by cover.clj. For ctx1 and ctx2 it
  required that ctx2 is a subcontext of ctx1

  This method is useful for larger contexts ctx1 and ctx2 which have
  many objects and attributes in common."
  [ctx1 ctx2 bv1]
  (let [;; update the set of attributes of the ctx and bv first
       shared-attributes (attributes ctx2)
       remove-ctx1-attributes-bv (attribute-intersection-cover bv1 shared-attributes)
       
       ;; To update all objects, we update the attributes of the dual
       ;; context
       dual-bv (dual-concept-cover remove-ctx1-attributes-bv)
       shared-objects (objects ctx2)
       remove-ctx1-objects-bv (attribute-intersection-cover dual-bv shared-objects)]
     (dual-concept-cover remove-ctx1-objects-bv)))

(defn transform-bv-intents-cores-cover
  "Transforms the intent lattice of ctx1 to that of ctx2 using the
  algorithm presented in 'Knowledge Cores in Large Formal Contexts'
  The methods input are the two contexts and the concept lattice of
  ctx1 as cover relation generated by cover.clj. For ctx1 and ctx2 it
  required that ctx2 is a pkcore of ctx1.

  This method is useful for larger contexts ctx1 and ctx2 which have
  many objects and attributes in common."
  [ctx1 core bv1 p]
  (cover-reducer ctx1 core p))

;;; general transformer

(defn cover-to-concepts 
  "This method converts the cover structure back to the list of concepts."
  [cover]
  (map #(vec [% (get-in cover [% :extent])]) (keys cover)))

(defn transform-bv
  "Transforms the set of concepts of ctx1 to that of ctx2 using the
  algorithm presented in 'Knowledge Cores in Large Formal Contexts'
  The methods input are the two contexts and the set of concepts
  ctx1. For ctx1 and ctx2 it required that there exists a super
  context ctx such that ctx1 and ctx2 are induced subcontexts of ctx.

  This method is useful for larger contexts ctx1 and ctx2 which have
  many objects and attributes in common."
  [ctx1 ctx2 concepts1]
  (let [bv1 (generate-concept-cover concepts1)
        bv2 (dual-concept-cover (transform-bv-cover ctx1 ctx2 bv1))]
    (cover-to-concepts bv2)))

