;; Copyright ⓒ the conexp-clj developers; all rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.fca.pqcores-test
  (:use conexp.fca.cores.pqcores conexp.fca.cores.cover)
  (:use conexp.fca.contexts)
  (:use clojure.test))

(def empty-ctx (make-context #{} #{} #{}))

(deftest test-pk-dense 
  (is (not (pk-dense? 
       (make-context (range 2) (range 2) <=) 
         2 2)))
  (is (pk-dense?
    (make-context (range 1) (range 2) <=)
      2 1))
  (is (pk-dense? 
    [0] (make-context (range 1) (range 2) <=)
    object-derivation 2))
  (is (pk-dense? 
    [1] (make-context (range 1) (range 2) <=)
    attribute-derivation 1)))

(deftest test-compute-core
  (is (= (compute-core (make-context (range 5) (range 5) <=) 2 2)
       (make-context (range 4) (range 1 5) <=)))
  (is (= (compute-core (make-context (range 5) (range 5) =) 2 1)
       empty-ctx)))

(deftest test-ctx-core-sizes
  (let [ctx (make-context (range 5) (range 5) <=)
        sizes (ctx-core-sizes ctx)]
    (is (= (count sizes)
         (* (-> ctx objects count) 
            (-> ctx attributes count))))
    (is (every? double? (map last sizes)))
    (is (= 190 
           (int (reduce + (map last sizes)))))))

(deftest test-ctx-lattice-sizes
  (let [ctx (make-context (range 5) (range 5) <=)
        sizes (core-lattice-sizes ctx)
        plimit 3 klimit 3
        sizespk (core-lattice-sizes ctx plimit klimit)
        example (nth sizes 7)]
    (is (= (count sizes)
         (* (-> ctx objects count)
            (-> ctx attributes count))))
    (is (every? int? (map last sizes)))
    (is (= 35
           (int (reduce + (map last sizes)))))
    (is (= (count (concepts (compute-core ctx (first example) (second example)))) 
           (last example)))
    (is (every? #(= 0 (last %)) 
         (filter #(and (> plimit (first %))
                       (> klimit (second %)))
                 sizespk)))))

(deftest test-find-size-core 
  (let [ctx (make-context (range 10) (range 10) <=)
        [p q] (find-size-core ctx 3)]
    (is (>= 3 
            (count (concepts (compute-core ctx p 1)))))
    (is (>= 3 
            (count (concepts (compute-core ctx 1 q)))))
    (is (< 3 
           (count (concepts (compute-core ctx (dec p) 1)))))
    (is (< 3 
           (count (concepts (compute-core ctx 1 (dec q))))))))

(deftest test-transform-bv
  (let [ctx1 (make-context (range 5) (range 5) <=)
;        ctx2 (make-context (range 3 7) (range 3 7) <=)
;        ctx3 (make-context (range 3) (range 3) <=)
        ctx4 (make-context (range 7) (range 7) <=)
        bv1 (generate-concept-cover (concepts ctx1))
;        bv2 (generate-concept-cover (concepts ctx2))
;        bv3 (generate-concept-cover (concepts ctx3))
        bv4 (generate-concept-cover (concepts ctx4))]
;    (is (= (transform-bv ctx1 ctx2 bv1) bv2))
;    (is (= (transform-bv ctx1 ctx3 bv1) bv3))
    (is (= (transform-bv ctx1 ctx4 bv1) bv4))))
