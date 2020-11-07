;; Copyright ⓒ the conexp-clj developers; all rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.contrib.fuzzy.sets-test
  (:use conexp.contrib.fuzzy.sets
        conexp.contrib.fuzzy.logics)
  (:use clojure.test))

;;;

;; Fuzzy-Set
;; make-fuzzy-set
;; fuzzy-set-as-hashmap
;; fuzzy-set?

;;;

;; fuzzy-intersection
;; fuzzy-union
;; fuzzy-difference

(deftest test-fuzzy-subsets
  (is (= (set (fuzzy-subsets [0 1/2 1] (make-fuzzy-set {1 1/2 2 1 3 0})))
         (set (map make-fuzzy-set
                   (list {} {1 1/2} {2 1/2} {2 1/2, 1 1/2} {2 1} {2 1, 1 1/2}))))))

;; fuzzy-subset?
;; subsethood

;;;

nil
