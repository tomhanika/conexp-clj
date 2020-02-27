;; Copyright ⓒ the conexp-clj developers; all rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.fca.pqcores
  (:require [conexp.base :refer :all] 
            [conexp.fca.contexts :refer :all] 
            [conexp.fca.lattices :refer 
             [concept-lattice base-set]]
            [conexp.fca.fast :as fast]
            [conexp.fca.cover :refer :all]
            [clojure.core.reducers :as r]))

;;; compute pq-core

(defn pk-dense?
  "This method tests if a context is pk-dense. Alternatively one can
  check if an object/attribute has the minimum required derivation
  size.

  A context is pk-dense iff it every object has a minimum derivation
  size of p and every attribute has a minimum derivation size of k."
  ([ctx p k]
   (or (and (= (count (objects ctx)) 0)
            (= (count (attributes ctx)) 0)) 
       (and (pk-dense? (objects ctx) ctx object-derivation p)
            (pk-dense? (attributes ctx) ctx attribute-derivation k))))
  ([target ctx deri limit]
   (not (some #(< (count (deri ctx (conj #{} %1))) limit)
              target))))


(defn- step-reduce-context
  "This is the iterator for an iterative computation of the pq-core. It
  computes a subcontext by removing all objects and attributes which
  do not have the minimum derivation size in ctx."
  [ctx p k]
  (let [rel (if (set? (incidence ctx)) 
              (incidence ctx)
              (filter (incidence ctx) 
                      (for [o (objects ctx) a (attributes ctx)] [o a])))]
    (make-context 
     (filter #(pk-dense? (conj #{} %1) ctx object-derivation p)
             (objects ctx)) 
     (filter #(pk-dense? (conj #{} %1) ctx attribute-derivation k)
             (attributes ctx))
     rel)))


(defn compute-core
  "Computes the pq-core of a context ctx.

  The pk-core of a context ctx is the larges subcontext which is
  pq-dense. Such a subcontext is unique for p,q in N"
  [context p k]
  (loop [ctx context]
    (if (pk-dense? ctx p k)
      ctx
      (recur (step-reduce-context ctx p k)))))

;;;; calculate all pq-core sizes

(defn- ctx-core-size-fix-p-iterator 
  "For a fixed p this method computes the size of all pq-core with
  q>=1. The size is measured as |G|*|M|"
  [ctx last-k p]
  (loop [core (compute-core ctx p 1) k 1 core-sizes []]
    (let [attr-count (double (* (count (attributes core)) 
                                (count (objects core))))
          new-core-sizes (conj core-sizes [p k attr-count])]
      (if (= k last-k)
        new-core-sizes
        (recur (compute-core core p (inc k)) (inc k) new-core-sizes)))))


(defn ctx-core-sizes 
  "For a formal context computes the size of all pq-cores, which have at
  least one object or attribute. This method considers only pq-cores
  for p,q != 0, since otherwise there would be an infinite number of
  p,q for which the core is not empty. A context's size is measured as
  |G|*|M|. This method is parallelized over p. If your context
  contains many more attributes, you might want to consider computing
  this for the dual context.

  The sizes are returned as a list of triples [p q size]."
  [ctx]
  (let [p (->> ctx objects (map (partial conj #{}))
               (map (comp count (partial object-derivation ctx))) 
               sort last range (map inc))
        last-k (->> ctx attributes (map (partial conj #{}))
                    (map (comp count (partial attribute-derivation ctx)))
                    (apply max))]
    (reduce (partial apply conj)
            (pmap (partial ctx-core-size-fix-p-iterator ctx last-k) p))))

;;; calculate all pq-core lattice sizes

(defn- ctx-core-lattice-size-fix-p-iterator-cover
  "For a fixed p this method computes the size of all pq-core concept
  lattices with q in k-range. 'init-intents is the cover relation of
  all concept intents."
  [ctx init-intends k-range p]
  (loop [last-core nil
         core (compute-core ctx p (first k-range))
         k k-range
         core-sizes [] 
         last-intends (cover-reducer init-intends ctx core p)]
    (let [l-count (count (keys last-intends))
          new-core-sizes (if (empty? k) core-sizes 
                             (conj core-sizes [p (first k) l-count]))]
      (if (empty? (rest k))
        new-core-sizes
        (let [next-k (apply min (last k) 
                            (map #(count (attribute-derivation core #{%}))
                                 (attributes core)))
              next-core (compute-core core p (inc next-k))
              ahead-core-sizes (into new-core-sizes (for [i (range (second k) (inc next-k))] [p i l-count]))]
          (recur core next-core (range (inc next-k) (inc (last k)))
                 ahead-core-sizes (cover-reducer last-intends core next-core p)))))))

(defn core-lattice-sizes
    "For a formal context computes the size of all pq-cores concept
  lattices, which have at least one object or attribute. This method
  considers only pq-cores for p,q != 0, since otherwise there would be
  an infinite number of p,q for which the core is not empty. This
  method is parallelized over p. If your context contains many more
  attributes, you might want to consider computing this for the dual
  context. For very large contexts you can all pq-cores with p<x and
  q<y. They will be returned with value 1.

  The sizes are returned as a list of triples [p q size]."
  ([ctx]
   (let [p (->> ctx objects (map (partial conj #{}))
                (map (comp count (partial object-derivation ctx))) 
                sort last range (map inc))
         k-range (->> ctx attributes (map (partial conj #{}))
                      (map (comp count (partial attribute-derivation ctx)))
                      (apply max) range (map inc))
         init-lattice (doall (->> ctx concept-lattice base-set (map last) generate-cover))]
     (vec (reduce (partial apply conj) 
                  (pmap (partial ctx-core-lattice-size-fix-p-iterator-cover ctx init-lattice k-range)
                        p)))))
  ([ctx x y]
   (let [p (->> ctx objects (map (partial conj #{}))
                (map (comp count (partial object-derivation ctx))) 
                sort last range (map inc))
         last-k (->> ctx attributes (map (partial conj #{}))
                     (map (comp count (partial attribute-derivation ctx)))
                     (apply max))
         init-lattice-1-y (future (doall (->> (compute-core ctx 1 y) concept-lattice base-set (map last) generate-cover))) 
         init-lattice-x-1 (future (doall (->> (compute-core ctx x 1) concept-lattice base-set (map last) generate-cover)))]
     (vec (into (for [i (range x) j (range y)] [i j 0])
                 (reduce (partial apply conj) 
                         (pmap (partial apply ctx-core-lattice-size-fix-p-iterator-cover ctx) 
                               (map #(if (< % x) [@init-lattice-1-y (range y last-k) %] [@init-lattice-x-1 (range 1 last-k) %])
                                    p))))))))

;;; find first object and attribute core for given maximal concept lattice size

(defn find-size-core 
  "Returns the minimum p,q such that the 1,q- and p,1-core have a
  concept lattice size of 'size."
  [ctx size]
  (let [;; max derivation size for q
        last-possible-q (->> ctx attributes (map (partial conj #{}))                                                              
                     (map (comp count (partial attribute-derivation ctx)))                                               
                     (apply max))   
        ;; max derivation size for p
        last-possible-p (->> ctx objects (map (partial conj #{}))                                                                 
                    (map (comp count (partial object-derivation ctx)))                                                   
                    (apply max))                      
        
        ;; binary search for minimum q, such that the 1,q-core has a
        ;; maximum concept lattice size of 'size
        p (future (loop [core (compute-core ctx (* 3 (int (/ last-possible-p 4))) 1)                                                        
                         curp (* 3 (int (/ last-possible-p 4))) lastp 1 maxp last-possible-p]
            (if (or (= curp maxp) (= curp lastp)) 
              curp  
              (let [ccount (count (fast/concepts core))]   
                (if (> ccount size )                  
                  (recur (compute-core ctx (int (/ (+ maxp curp) 2)) 1)                                                  
                         (int (/ (+ maxp curp) 2)) curp maxp)                                                            
                  (if (> (count (fast/concepts (compute-core ctx (- curp 1) 1))) size)                                        
                    curp                              
                    (recur (compute-core ctx (int (/ (+ lastp curp) 2)) 1)                                               
                           (int (/ (+ lastp curp) 2)) lastp curp)))))))                                           
        ;; binary search for minimum q, such that the 1,q-core has a
        ;; maximum concept lattice size of 'size
        q (future (loop [core (compute-core ctx 1 (* 3 (int (/ last-possible-q 4))))
                         current-q (* 3 (int (/ last-possible-q 4))) latest-q 1 maximum-q last-possible-q]
           (if (or (= current-q maximum-q) (= current-q latest-q)) current-q  
                (let [ccount (count (fast/concepts core))]   
                  (if (> ccount size )                  
                    (recur (compute-core ctx 1 (int (/ (+ maximum-q current-q) 2)) )                                                 
                           (int (/ (+ maximum-q current-q) 2)) current-q maximum-q)                                                            
                    (if (> (count (fast/concepts (compute-core ctx 1 (- current-q 1)))) size)                                        
                      current-q
                      (recur (compute-core ctx 1 (int (/ (+ latest-q current-q) 2)))                                               
                             (int (/ (+ latest-q current-q) 2)) latest-q current-q)))))))]                                             
    [@p @q]))

(defn large-ctx-lattice-sizes-partial
  "This method computes the context lattice sizes for all cores of
  'ctx. The core lattice sizes computed are also core of the largest
  object core or attribute core of size at most k."
  [ctx k]
  (apply core-lattice-sizes ctx (find-size-core ctx)))

;;; general transformer

(defn transform-bv
  "Transforms the concept lattice of ctx1 to that of ctx2 using the
  algorithm presented in .. The methods input are the two contexts and
  the concept lattice of ctx1 as cover relation generated by
  cover.clj. For ctx1 and ctx2 it required that there exists a super
  context ctx such that ctx1 and ctx2 are induced subcontexts of ctx.

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
                                dual-bv (make-context shared-objects (attributes dual-ctx) (incidence dual-ctx)) 
                                (difference (objects ctx1) shared-objects))
        ctx2-only-objects (difference (objects ctx2) (objects ctx1))
        insert-ctx2-objects-bv (attribute-insertion-cover remove-ctx1-objects-bv (dual-context ctx2) ctx2-only-objects)]
     (dual-concept-cover insert-ctx2-objects-bv)))

(defn transform-bv-subctx
  "Transforms the concept lattice of ctx1 to that of ctx2 using the
  algorithm presented in .. The methods input are the two contexts and
  the concept lattice of ctx1 as cover relation generated by
  cover.clj. For ctx1 and ctx2 it required that ctx2 is a subcontext of ctx1

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

(defn transform-bv-intents-cores
  "Transforms the intent lattice of ctx1 to that of ctx2 using the
  algorithm presented in .. The methods input are the two contexts and
  the concept lattice of ctx1 as cover relation generated by
  cover.clj. For ctx1 and ctx2 it required that ctx2 is a pkcore of ctx1.

  This method is useful for larger contexts ctx1 and ctx2 which have
  many objects and attributes in common."
  [ctx1 core bv1 p]
  (cover-reducer ctx1 core p))

(defn transform-bv-inserted-attribute-only
    "Transforms the concept lattice of ctx1 to that of ctx2 using the
  algorithm presented in .. The methods input are the two contexts and
  the concept lattice of ctx1 as cover relation generated by
  cover.clj. For ctx1 and ctx2 it required that ctx1 is a subcontext
  of ctx2 and they have the same set of objects.

  This method is useful for larger contexts ctx1 and ctx2 which have
  many objects and attributes in common."
  [ctx1 ctx2 bv1]
  (let [;; update the set of attributes of the ctx and bv first
       ctx2-only-attributes (difference (attributes ctx2) (attributes ctx1))
       attr-intermediate-ctx (make-context (objects ctx1) (attributes ctx2) 
                                           (fn [a b] (if (contains? (objects ctx2) a) 
                                                       ((incidence ctx2) [a b])
                                                       ((incidence ctx1) [a b]))))
       insert-ctx2-attributes-bv (attribute-insertion-cover bv1 attr-intermediate-ctx ctx2-only-attributes)]
    insert-ctx2-attributes-bv))

(defn transform-bv-deleted-attribute-only
"Transforms the concept lattice of ctx1 to that of ctx2 using the
  algorithm presented in .. The methods input are the two contexts and
  the concept lattice of ctx1 as cover relation generated by
  cover.clj. For ctx1 and ctx2 it required that ctx2 is a subcontext
  of ctx1 and has the same set of objects.

  This method is useful for larger contexts ctx1 and ctx2 which have
  many objects and attributes in common."
  [ctx1 ctx2 bv1]
  (let [;; update the set of attributes of the ctx and bv first
       shared-attributes (attributes ctx2)
       remove-ctx1-attributes-bv (attribute-intersection-cover bv1 shared-attributes)]
     remove-ctx1-attributes-bv))
