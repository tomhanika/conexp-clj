;; Copyright (c) Daniel Borchmann. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.tests.fca.association-rules
  (:use conexp.base
        conexp.fca.contexts
        conexp.fca.implications
        conexp.fca.association-rules)
  (:require [conexp.tests.fca.contexts :as c])
  (:use clojure.test))

;;;

(defvar- ctx-1 (make-context #{0 1 2 3 4 5 6 7 8 9}
                             #{0 1 2 3 4 5 6 7 8 9}
                             #{[6 5] [1 0] [4 4] [9 9] [0 0] [1 1] [3 4] [6 7]
                               [8 9] [1 2] [3 5] [6 8] [0 2] [3 6] [5 8] [0 3]
                               [1 4] [0 4] [3 8] [0 5] [3 9] [2 9] [9 0] [7 0]
                               [8 1] [7 1] [9 3] [7 2] [7 3] [9 5] [6 3] [3 1]
                               [6 4] [8 6] [2 0]}))

(defvar- testing-data [ctx-1 c/empty-context c/test-ctx-01
                       c/test-ctx-04 c/test-ctx-07 c/test-ctx-08])

(deftest test-make-association-rule
  (are [context-1 premise-1 conclusion-1] (let [ar (make-association-rule context-1 premise-1 conclusion-1)]
                                            (and (= (premise ar) (set premise-1))
                                                 (= (conclusion ar) (set conclusion-1))))
       ctx-1 #{} #{},
       ctx-1 #{1 2 3 4} #{5 6 7},
       ctx-1 #{} #{1},
       ctx-1 [] [])
  (are [context premise conclusion] (thrown? IllegalArgumentException
                                             (make-association-rule context premise conclusion))
       ctx-1 #{} '[a],
       ctx-1 '[a] #{},
       ctx-1 '[a] '[b]))

(deftest test-premise-conclusion-support-confidence
  (are [p c s k] (let [ar (make-association-rule p c s k)]
                   (and (= (set p) (premise ar))
                        (= (difference (set c) (set p)) (conclusion ar))
                        (= s (support ar))
                        (= k (confidence ar))))
       #{} #{} 1 0,
       #{} #{} 0 1,
       #{1} #{1 2} 1/2 3/4,
       [] [] 0 0)
  (are [p c s k] (thrown? IllegalArgumentException (make-association-rule p c s k))
       [] [] -1 0,
       1 2 3 4)
  (are [ctx p c] (let [pr (set p)
                       cl (set c)
                       ar (make-association-rule ctx pr cl)]
                   (and (= pr (premise ar))
                        (= (difference cl pr) (conclusion ar))
                        (= (support ar)
                           (support (union pr cl) ctx))
                        (= (confidence ar)
                           (confidence pr cl ctx))))
       ctx-1 #{} #{1},
       ctx-1 #{} #{},
       ctx-1 #{1 2 3} #{3 4 1 0}))

(deftest test-support
  (is (= 1 (support #{} c/empty-context)))
  (with-testing-data [ctx testing-data]
    (let [oc (count (objects ctx))]
      (forall [x (subsets (attributes ctx))]
        (= (* oc (support x ctx))
           (count (attribute-derivation ctx x)))))))

(deftest test-confidence
  (is (= 1 (confidence #{} #{} c/empty-context))))

;;;

(deftest test-iceberg-intent-seq
  (is (= 1 (count (iceberg-intent-seq (one-context [1 2 3 4]) 0.0))))
  (with-testing-data [ctx [c/test-ctx-01 c/test-ctx-02 c/test-ctx-04
                           c/test-ctx-07 c/test-ctx-08],
                      spp [0 1/3 1/2 3/5 7/8 1]]
    (= (set-of C [C (intents ctx) :when (>= (support C ctx) spp)])
       (set (iceberg-intent-seq ctx spp)))))

(deftest test-luxenburger-basis
  (with-testing-data [ctx [ctx-1 c/test-ctx-01 c/test-ctx-02,
                           c/test-ctx-04 c/test-ctx-07 c/test-ctx-08]
                      spp [0 1/2 9/10 1]
                      cnf [0 1/2 9/10 1]]
    (let [ars (luxenburger-basis ctx spp cnf)]
      (forall [ar ars]
        (and (<= spp (support ar))
             (<= cnf (confidence ar))))))
  (are [n ctx s c] (= n (count (luxenburger-basis ctx s c)))
       58 ctx-1 0 0,
        2 ctx-1 1/3 1/2,
       26 ctx-1 1/5 1/2,
        0 c/test-ctx-04 1/5 1/2,
        1 c/test-ctx-01 1/5 1/2))

;;;

nil
