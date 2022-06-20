;; Copyright ⓒ the conexp-clj developers; all rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.fca.cover-test
  (:use conexp.fca.pqcores
        conexp.fca.cover
        conexp.fca.contexts
        conexp.fca.lattices
        conexp.math.algebra)
  (:use clojure.test clojure.set))

(deftest test-generate-cover
  (let [ctx (make-context (range 5) (range 5) <=)
        cover (generate-cover (map last (concepts ctx)))]
    (is (map? cover))
    (is (every? set? (keys cover)))
    (is (every? map? (vals cover)))
    (is (every? #((get cover %) :lower)
         (keys cover)))
    (is (every? #(contains? (get cover %) :upper)
         (keys cover)))))

(deftest test-generate-concept-cover
  (let [ctx (make-context (range 5) (range 5) <=)
        cover (generate-concept-cover (concepts ctx))]
    (is (map?
         cover))
    (is (every? set?
         (keys cover)))
    (is (every? map?
         (vals cover)))
    (is (every? #((get cover %) :lower)
         (keys cover)))
    (is (every? #(contains? (get cover %) :upper)
         (keys cover)))
    (is (every? #((get cover %) :extent)
         (keys cover)))
    (is (every? #(= (attribute-derivation ctx %)
                    (get-in cover [% :extent]))
         (keys cover)))))

(deftest test-dual-concept-cover
  (let [ctx (make-context (range 5) (range 5) <=)
        cover (generate-concept-cover (concepts ctx))]
    (is (= (dual-concept-cover cover) 
           (generate-concept-cover (concepts (dual-context ctx)))))))

(deftest test-irreducible
  (let [ctx (make-context (range 5) (range 5) <=)
        cover (generate-concept-cover (concepts ctx))]
    (is (= 5 
           (count (filter #(meet-irreducible-by-cover? % cover) (keys cover)))))
    (is (= 5 
           (count (filter #(join-irreducible-by-cover? % cover) (keys cover)))))))

(deftest test-attribute-intersection-cover
  (let [ctx (make-context (range 5) (range 5) <=)
        cover (generate-cover (map last (concepts ctx)))
        new-ctx (make-context (range 5) (range 3) <=)
        new-cover (generate-cover (map last (concepts new-ctx)))]
    (is (= new-cover 
           (attribute-intersection-cover cover (attributes new-ctx))))))

(deftest test-attribute-deletion-cover
  (let [ctx (make-context (range 5) (range 5) <=)
        cover (generate-cover (map last (concepts ctx)))
        new-ctx (make-context (range 5) (attributes (compute-core ctx 2 3)) <=)
        new-cover (generate-cover (map last (concepts new-ctx)))]
    (is (= new-cover 
           (attribute-deletion-cover cover ctx (difference 
                                                    (attributes ctx)
                                                    (attributes new-ctx)))))))

(deftest test-attribute-insertion-cover
  (let [ctx (make-context (range 5) (range 5) <=)
        cover (generate-concept-cover (concepts ctx))
        new-ctx (make-context (range 5) (range 7) <=)
        new-cover (generate-concept-cover (concepts new-ctx))
        gen-cover (attribute-insertion-cover cover new-ctx #{5 6})]
    (is (= new-cover 
           gen-cover))))

(deftest test-cover-reducer
  (let [ctx (make-context (range 10) (range 10) <=)
        cover (generate-cover (map last (concepts ctx)))
        new-ctx (compute-core ctx 4 6)
        new-cover (generate-cover (map last (concepts new-ctx)))]
    (is (= new-cover
           (cover-reducer cover ctx new-ctx 4)))))

;;;

(def test-ctx-1
  (make-context-from-matrix [1 2 3]
                            [1 2 3]
                            [1 0 0
                             0 1 0
                             0 1 1]))

(deftest test-cover-relation
  (let [lattice (concept-lattice test-ctx-1)]
    (is (= (cover-relation (base-set lattice) (order lattice))
           #{[[#{} #{1 2 3}] [#{3} #{2 3}]]
             [[#{} #{1 2 3}] [#{1} #{1}]]
             [[#{3} #{2 3}] [#{2 3} #{2}]]
             [[#{2 3} #{2}] [#{1 2 3} #{}]]
             [[#{1} #{1}] [#{1 2 3} #{}]]}))))
