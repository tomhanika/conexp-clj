;; Copyright (c) Daniel Borchmann. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.tests.fca.exploration
  (:use conexp.base
        conexp.fca.contexts
        conexp.fca.implications
        conexp.fca.exploration)
  (:use clojure.test))

;;;

(defvar- *testing-data*
  [(make-context #{1 2 3 4} #{1 2 3 4}
                 #{[1 1] [1 2] [1 3]
                   [2 1] [2 2] [2 3]
                   [3 3] [4 3] [4 4]}),
   (one-context (set-of-range 5)),
   (diag-context (set-of-range 5)),
   (adiag-context (set-of-range 5)),
   (null-context (set-of-range 5)),
   (one-context #{}),
   (rand-context #{1 2 3 4 5} 0.3),
   (rand-context #{1 2 3 4 5} 0.4),
   (rand-context #{1 2 3 4 5} 0.5),
   (rand-context #{1 2 3 4 5} 0.6),
   (rand-context #{1 2 3 4 5} 0.7),
   ])

(deftest test-explore-attributes-is-stem-base
  (with-testing-data [ctx *testing-data*]
    (= (stem-base ctx)
       (explore-attributes ctx #{} (constantly [true []])))))

(defn- say-no [ctx impl]
  [false, [(gensym) (premise impl)]])

(deftest test-explore-attributes-with-always-saying-no
  (with-testing-data [ctx *testing-data*]
    (= #{} (explore-attributes ctx #{} say-no))))

;;;

nil
