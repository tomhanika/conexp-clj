;; Copyright ⓒ the conexp-clj developers; all rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.fca.causal-implications-test
  (:require
   [conexp.base :refer :all]
   [conexp.io.contexts :refer :all]
   [conexp.fca.contexts :refer :all]
   [conexp.fca.implications :refer :all]
   [conexp.fca.causal-implications :refer :all]
   [clojure.set :as set])
   (:use clojure.test))


(def o [0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20
        21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40])
(def a ["smoking" "male" "female" "education-level-high" "education-level-low" "cancer"])
(def i #{[0 "smoking"] [0 "male"] [0 "education-level-high"] [0 "cancer"]
         [1 "smoking"] [1 "male"] [1 "education-level-high"] [1 "cancer"]
         [2 "smoking"] [2 "male"] [2 "education-level-high"] [2 "cancer"]
         [3 "smoking"] [3 "male"] [3 "education-level-high"] [3 "cancer"]
         [4 "smoking"] [4 "male"] [4 "education-level-high"] [4 "cancer"]
         [5 "smoking"] [5 "male"] [5 "education-level-high"] [5 "cancer"]
         [6 "smoking"] [6 "male"] [6 "education-level-high"]
         [7 "smoking"] [7 "male"] [7 "education-level-high"]
         [8 "smoking"] [8 "male"] [8 "education-level-low"] [8 "cancer"]
         [9 "smoking"] [9 "male"] [9 "education-level-low"] [9 "cancer"]
         [10 "smoking"] [10 "male"] [10 "education-level-low"] [10 "cancer"]
         [11 "smoking"] [11 "male"] [11 "education-level-low"] [11 "cancer"]
         [12 "smoking"] [12 "male"] [12 "education-level-low"] 
         [13 "smoking"] [13 "female"] [13 "education-level-high"] [13 "cancer"]
         [14 "smoking"] [14 "female"] [14 "education-level-high"] [14 "cancer"]
         [15 "smoking"] [15 "female"] [15 "education-level-high"] [15 "cancer"]
         [16 "smoking"] [16 "female"] [16 "education-level-high"] [16 "cancer"]
         [17 "smoking"] [17 "female"] [17 "education-level-high"] [17 "cancer"]
         [18 "smoking"] [18 "female"] [18 "education-level-high"] 
         [19 "smoking"] [19 "female"] [19 "education-level-high"]
         [20 "smoking"] [20 "female"] [20 "education-level-low"] [20 "cancer"]
         [21 "smoking"] [21 "female"] [21 "education-level-low"] [21 "cancer"]
         [22 "smoking"] [22 "female"] [22 "education-level-low"] [22 "cancer"]
         [23 "smoking"] [23 "female"] [23 "education-level-low"] [23 "cancer"]
         [24 "smoking"] [24 "female"] [24 "education-level-low"] 
         [25 "male"] [25 "education-level-high"] [25 "cancer"]
         [26 "male"] [26 "education-level-high"] [26 "cancer"]
         [27 "male"] [27 "education-level-high"] 
         [28 "male"] [28 "education-level-high"]
         [29 "male"] [29 "education-level-high"]
         [30 "male"] [30 "education-level-low"] [30 "cancer"]
         [31 "male"] [31 "education-level-low"] 
         [32 "male"] [32 "education-level-low"] 
         [33 "male"] [33 "education-level-low"]
         [34 "female"] [34 "education-level-high"] [34 "cancer"]
         [35 "female"] [35 "education-level-high"] 
         [36 "female"] [36 "education-level-high"]
         [37 "female"] [37 "education-level-low"] [37 "cancer"]
         [38 "female"] [38 "education-level-low"]
         [39 "female"] [39 "education-level-low"]
         [40 "female"] [40 "education-level-low"]})

(def ctx (make-context o a i))

(def test-premise #{"smoking"})
(def test-conclusion #{"cancer"})
(def smoking-rule (make-implication test-premise test-conclusion))

(def fds (fair-data-set ctx smoking-rule #{"male" "female" "education-level-high" "education-level-low"}))

(def smoking-odds-ratio (odds-ratio ctx smoking-rule))
(def ivars-smoking (irrelevant-variables ctx a "cancer" 1.7))
(def fair-smoking-odds-ratio (fair-odds-ratio ctx smoking-rule fds))

(def birds (read-context "testing-data/Bird-Diet.ctx"))

(def diagnosis (read-context "testing-data/Diagnosis.ctx"))


(causal-association-rule-discovery  ctx 0.7 3 "cancer" 1.7)

(causal-association-rule-discovery diagnosis 0.7 3 "[Urine pushing yes]" 1.7)
