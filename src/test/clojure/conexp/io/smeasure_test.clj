;; Copyright ⓒ the conexp-clj developers; all rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.io.smeasure-test
  (:use conexp.base
        [conexp.fca.contexts :refer [make-context-nc]]
        conexp.fca.lattices
        [conexp.fca.smeasure :refer [make-smeasure-nc]]
        conexp.layouts.base
        conexp.io.latex
        conexp.io.smeasure)
  (:use clojure.test))

;;;

(def ctx1 (make-context-nc #{1 2 3 4} #{1 2 3 4 5} 
                           #{[1 1][2 2][1 3][2 4][3 5][4 1][4 3]}))
(def ctx2 (make-context-nc #{1 2 3} #{1 2 3 4 5} #{[1 1][2 2][1 3][2 4][3 5]}))
(def ctx3 (make-context-nc #{1 2 3 4 5 6 7 8 9} #{1 2 3 4 5} 
                           #{[1 1][2 2][1 3][2 4][3 5]}))

(def sm1 (make-smeasure-nc ctx1 ctx2 #(case % 1 1 4 1 2 1 3 2)))
(def sm2 (make-smeasure-nc ctx1 ctx3 #(case % 1 1 4 1 2 1 3 2)))
(def sm3 (make-smeasure-nc ctx3 ctx1 
                           #(case % 1 1 4 1 2 1 3 2 5 1 6 2 7 1 8 4 9 3)))

(deftest test-smeasure->tikz
    (is (= (latex sm1 :tikz)
           (slurp "testing-data/latex/tikz-smeasure1-2.tex")))
    (is (= (latex sm2 :tikz)
           (slurp "testing-data/latex/tikz-smeasure1-3.tex")))
    (is (= (latex sm3 :tikz)
           (slurp "testing-data/latex/tikz-smeasure3-1.tex"))))

(deftest test-smeasure->tikz
    (is (= (latex sm1 :lattice)
           (slurp "testing-data/latex/lattice-smeasure1-2.tex")))
    (is (= (latex sm2 :lattice)
           (slurp "testing-data/latex/lattice-smeasure1-3.tex")))
    (is (= (latex sm3 :lattice)
           (slurp "testing-data/latex/lattice-smeasure3-1.tex"))))

;;;

nil
