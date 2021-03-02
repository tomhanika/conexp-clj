;; Copyright ⓒ the conexp-clj developers; all rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.fca.more-test
  (:require [clojure.test :refer [deftest is]]
            [conexp.base :refer :all]
            [conexp.fca.contexts :refer :all]
            [conexp.fca.contexts-test :refer :all]
            [conexp.fca.implications :refer :all]
            [conexp.fca.implications-test :as impls]
            [conexp.fca.more :refer :all]))

;;; Bonds

(def small-contexts [test-ctx-01, test-ctx-04, test-ctx-07, test-ctx-08])

(deftest test-all-bonds
  (is (= 2216 (count (all-bonds (make-context-from-matrix 5 5 [0 1 0 1 0
                                                               1 0 1 1 0
                                                               1 0 0 0 0
                                                               1 0 1 1 1
                                                               0 0 0 1 1])
                                (make-context-from-matrix 5 5 [1 1 1 0 1
                                                               1 0 1 1 1
                                                               1 1 1 0 0
                                                               1 1 0 0 0
                                                               0 1 1 0 0])))))
  (with-testing-data [ctx small-contexts]
    (every? #(bond? ctx ctx %) (all-bonds ctx ctx))))

(deftest test-smallest-bond
  (with-testing-data [ctx small-contexts,
                      :let [bonds (all-bonds ctx ctx)]
                      rel (map (fn [_]
                                 (incidence-relation (rand-context (objects ctx)
                                                                   (attributes ctx)
                                                                   (rand))))
                               (range 10))]
    (let [smallest (smallest-bond ctx ctx rel)]
      (and (exists [b bonds]
             (= smallest b))
           (forall [b bonds]
             (=> (subset? rel (incidence-relation b))
                 (subset? (incidence-relation smallest) (incidence-relation b))))))))

;;;

(deftest test-all-shared-intents
  (with-testing-data [ctx small-contexts]
    (let [intents (intents ctx)]
      (= intents (all-shared-intents ctx ctx))))
  (with-testing-data [ctx-1 small-contexts,
                      ctx-2 small-contexts,
                      :when (= (attributes ctx-1) (attributes ctx-2))]
    (= (set (all-shared-intents ctx-1 ctx-2))
       (intersection (set (intents ctx-1))
                     (set (intents ctx-2))))))

(deftest test-all-bonds-by-shared-intents
  (with-testing-data [ctx-1 small-contexts,
                      ctx-2 small-contexts]
    (= (set (all-bonds ctx-1 ctx-2))
       (set (all-bonds-by-shared-intents ctx-1 ctx-2)))))

;;;

(deftest test-context-from-clop
  (is (let [impls #{(make-implication #{1} #{2}),
                    (make-implication #{4} #{2 3})
                    (make-implication #{1 3} #{4 5})}]
        (equivalent-implications?
         impls
         (stem-base (context-from-clop #{1 2 3 4 5}
                                       (clop-by-implications impls))))))
  (is (equivalent-implications?
       impls/testing-data
       (stem-base (context-from-clop (reduce (fn [set impl]
                                               (union set (premise impl) (conclusion impl)))
                                             #{}
                                             impls/testing-data)
                                     (clop-by-implications impls/testing-data)))))
  (with-testing-data [ctx (random-contexts 20 10)]
    (let [ctx (reduce-objects (clarify-context ctx)),
          cct (context-from-clop (attributes ctx)
                                  #(context-attribute-closure ctx %))]
      (and (= (count (objects ctx))
              (count (objects cct)))
           (= (attributes ctx)
              (attributes cct))
           (forall [g (objects ctx)]
             (exists [h (objects cct)]
               (= (object-derivation ctx #{g})
                  (object-derivation cct #{h}))))))))

;;;
nil
