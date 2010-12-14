;; Copyright (c) Daniel Borchmann. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.contrib.tests.dl.util.concept-sets
  (:use conexp.main
        conexp.contrib.dl.framework.syntax
        conexp.contrib.dl.framework.reasoning
        conexp.contrib.dl.util.concept-sets
        conexp.contrib.tests.dl.examples)
  (:use clojure.test))

;;; Testing Data

(defvar- concept-seqs
  (map (fn [[dl symbols]]
         (map #(dl-expression dl %)
              symbols))
       [[FamilyDL '[Male Mother Father Female]],
        [FamilyDL '[Mother Male Father Female]],
        [FamilyDL '[Mother Father Mother Father Mother Male Father Female Mother Father Female Male]],
        [FamilyDL '[Mother Father (and Mother Father) (and Father Mother) Male (and Male Female) Female (and Male Male Female)]],
        [FamilyDL '[Male Mother Male Father Female Male Father Male Mother Female Female Female]],
        [FamilyDL '[Male Female (and Female Male) (and Male Male) (and Female Female Male Female Mother) Mother (and Mother Father)
                    (and Father Mother) (and Male Female) Male]],
        [FamilyDL '[Male Female (and Male Female) (and Male Male) (exists HasChild Male) (exists HasChild (exists HasChild Mother))
                    Father (exists HasChild (and Father (exists HasChild Male) Female))]]]))

;;; Tests

(defn- order-consistent?
  "Checks whether coll2 is ordered the same way as coll1 possibly with
  some elements missing."
  [coll1 coll2]
  (loop [coll1 coll1,
         coll2 coll2]
    (cond
     (empty? coll2) true,
     (empty? coll1) false,
     :else          (let [next (first coll2)]
                      (and (or (some #(= next %) coll1) false)
                           (recur (rest (drop-while #(not= next %) coll1))
                                  (rest coll2)))))))

(deftest- inserts-in-right-order
  (doseq [concept-seq concept-seqs]
    (let [cs-1 (make-concept-set concept-seq),
          cs-2 (make-concept-set []),
          cs-3 (make-concept-set [])]
      (doseq [c concept-seq]
        (add-concept! cs-2 c))
      (apply add-concepts! cs-3 concept-seq)
      (is (order-consistent? (reverse concept-seq) cs-1))
      (is (order-consistent? (reverse concept-seq) cs-2))
      (is (order-consistent? (reverse concept-seq) cs-3)))))

;;

(deftest- contains-no-equivalent-concepts
  (doseq [concept-seq concept-seqs]
    (let [cs (make-concept-set concept-seq)]
      (is (forall [x cs, y cs]
                  (=> (equivalent? x y) (= x y)))))))

;;

(defn- sound?
  "Tests whether a given set of implications between concepts is
  equivalent with subsumed-by?. The implications must be must be
  implications between singleton sets of concept descriptions."
  [implications]
  (forall [impl implications]
          (subsumed-by? (first (premise impl)) (first (conclusion impl)))))

(defn- minimal?
  "Tests whether the given set of implications is minimal, i.e. no
  implication follows from the other one."
  [implications]
  (forall [impl implications]
          (not (follows-semantically? impl (disj implications impl)))))

(deftest- minimal-implication-set-is-sound-and-minimal
  (doseq [concept-seq concept-seqs]
    (let [implications (minimal-implication-set (make-concept-set concept-seq))]
      (and (is (sound? implications))
           (is (minimal? implications))))))

;;

(defn test-ns-hook []
  (inserts-in-right-order)
  (contains-no-equivalent-concepts)
  (minimal-implication-set-is-sound-and-minimal))

;;;

nil