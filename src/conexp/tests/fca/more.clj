;; Copyright (c) Daniel Borchmann. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.tests.fca.more
  (:use conexp.base
        conexp.fca.contexts
        conexp.fca.more
        conexp.tests.fca.contexts)
  (:use clojure.test))

;;;

(deftest test-subcontext?
  (test-for-every-test-ctx
   [ctx] (and (subcontext? ctx ctx)
	      (subcontext? *empty-context* ctx))))

(deftest test-compatible-subcontext?
  (test-for-every-test-ctx
   [ctx] (and (compatible-subcontext? ctx ctx)
	      (compatible-subcontext? *empty-context* ctx))))

(deftest test-compatible-subcontexts
  (are [ctx] (let [ctx (reduce-context ctx)]
	       (every? #(compatible-subcontext? % ctx)
		       (compatible-subcontexts ctx)))
       *test-ctx-01*
       *test-ctx-04*
       *test-ctx-06*
       *test-ctx-07*
       *test-ctx-08*)
  (is (let [some-context (make-context #{1 2 3} '#{c d e} '#{[1 c] [2 c] [2 e] [3 e]})]
	(some #(= some-context %) (compatible-subcontexts *test-ctx-08*)))))

;;;

nil
