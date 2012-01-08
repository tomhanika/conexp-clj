;; Copyright (c) Daniel Borchmann. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.tests.util.graph
  (:use clojure.test
        conexp.util.graph))

;;;

(def refine-ordered-partition @#'conexp.util.graph/refine-ordered-partition)

;;;

(def graph-1
  (make-directed-graph #{1 2 3 4 5}
                       {1 [2 3], 2 [1 3], 3 [1 2], 4 [5], 5 [4]}))

(def graph-2
  (make-directed-graph #{1 2 3 4 5}
                       {1 [2], 2 [1], 3 [4 5], 4 [3 5], 5 [3 4]}))


(def pi-1
  (make-ordered-partition [[1 2 3 4 5]]))

;;;

;; Partitions

;; make-ordered-partition
;; discrete-partition?
;; unit-partition?
;; nr-neighors-in-set
;; partition-by-set
;; find-index-of-vertex
;; first-maximal-set-index
;; first-minimal-set-index
;; replace-partition-cell
;; append-partition
;; equitable-partition?

(deftest test-refine-ordered-partition
  (is (= (make-ordered-partition [#{4 5} #{1 2 3}])
         (refine-ordered-partition graph-1 pi-1 pi-1)))
  ;; More!
  )

;; circ-partition
;; splut-partition-at
;; induced-permutation

;; McKay's Algorithm

;; mckay
;; canonical-isomorph
;; automorphism-group-generators
;; automorphism-group-size
;; automorphism-group

;;

;;;

nil
