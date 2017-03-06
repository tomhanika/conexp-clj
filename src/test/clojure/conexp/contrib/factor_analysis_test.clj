;; Copyright ⓒ the conexp-clj developers; all rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.contrib.factor-analysis-test
  (:use conexp.base
        conexp.fca.contexts
        conexp.contrib.factor-analysis)
  (:use clojure.test))

;;;

(def- context-data
  [(make-context #{} #{} #{}),
   (make-context (set-of-range 15)
                 (set-of-range 15)
                 #{[2 1] [3 2] [7 6] [8 7] [10 9] [12 11] [1 0]
                   [6 6] [9 9] [10 10] [11 11] [13 13] [14 14]
                   [0 0] [4 5] [6 7] [9 10] [10 11] [13 14] [2 4]
                   [3 5] [4 6] [7 9] [9 11] [2 5] [4 7] [6 9]
                   [7 10] [8 11] [0 3] [2 6] [3 7] [4 8] [5 9]
                   [6 10] [0 4] [1 5] [2 7] [4 9] [6 11] [7 12]
                   [9 14] [3 9] [5 11] [0 6] [1 7] [4 11] [6 13]
                   [1 8] [3 11] [4 12] [6 14] [0 8] [2 11] [3 13]
                   [4 14] [0 10] [2 13] [3 14] [1 12] [14 0] [13 1]
                   [11 0] [12 1] [13 2] [12 2] [9 0] [9 1] [11 3]
                   [12 4] [13 5] [14 6] [10 3] [8 2] [10 4] [11 5]
                   [12 6] [13 7] [5 0] [7 2] [11 6] [4 0] [11 7]
                   [14 10] [3 0] [4 1] [6 3] [8 5] [10 7] [12 9]
                   [13 10] [3 1] [4 2] [5 3] [6 4] [10 8] [11 9] [2 0]}),
   (make-context #{1 2 3 4 5 6 7}
                 '#{a b c d e f g}
                 '#{[1 c] [1 g] [2 a] [2 b] [2 c]
                    [2 g] [3 a] [3 d] [3 e] [3 g]
                    [4 a] [4 f] [5 a] [5 c] [5 d]
                    [5 f] [6 a] [6 b] [6 e] [7 a]
                    [7 b] [7 e] [7 f] [7 g]}),
   (make-context-from-matrix 10 7
                             [0 1 0 0 0 1 0
                              0 0 1 1 0 1 1
                              0 0 0 1 0 1 1
                              0 1 1 0 0 1 0
                              0 0 1 1 0 0 1
                              0 1 0 0 0 0 0
                              1 0 0 0 1 0 0
                              0 0 0 1 1 1 0
                              1 1 0 0 0 1 0
                              1 1 1 1 1 1 0]),
   (make-context-from-matrix 10 7
                             [1 1 0 0 1 1 0
                              1 1 0 0 1 0 0
                              0 1 0 1 1 0 1
                              1 1 0 1 0 1 0
                              0 0 0 0 0 0 0
                              0 1 0 0 1 0 0
                              1 1 1 1 1 0 1
                              1 1 1 0 0 0 0
                              1 0 0 0 1 1 1
                              1 1 0 1 1 1 1]),])

(defn- test-for-formal-context
  [method]
  (with-testing-data [ctx context-data]
    (= (incidence ctx) (set-of [i j] [[C D] (factorize-context method ctx),
                                      i C, j D]))))

(deftest test-factorize-context
  (doseq [method [:boolean-full :boolean-adaptive]]
    (test-for-formal-context method)))

;;;

nil
