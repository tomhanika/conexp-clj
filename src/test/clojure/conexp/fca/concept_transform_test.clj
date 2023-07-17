;; Copyright ⓒ the conexp-clj developers; all rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.fca.concept-transform-test
  (:use conexp.fca.concept-transform conexp.fca.cover)
  (:use conexp.fca.contexts)
  (:use clojure.test))

(deftest test-transform-bv
  (let [ctx1 (make-context (range 5) (range 5) <=)
        ctx2 (make-context (range 3 7) (range 3 7) <=)
        ctx3 (make-context (range 3) (range 5) <=)
        ctx4 (make-context (range 7) (range 7) <=)
        bv1 (generate-concept-cover (concepts ctx1))
        bv2 (generate-concept-cover (concepts ctx2))
        bv3 (generate-concept-cover (concepts ctx3))
        bv4 (generate-concept-cover (concepts ctx4))]
    (is (= (transform-bv-cover ctx1 ctx2 bv1) bv2))
    (is (= (transform-bv-cover ctx1 ctx3 bv1) bv3))
    (is (= (transform-bv-cover ctx1 ctx4 bv1) bv4))))
