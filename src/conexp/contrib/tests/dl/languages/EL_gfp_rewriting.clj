;; Copyright (c) Daniel Borchmann. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.contrib.tests.dl.languages.EL-gfp-rewriting
  (:use conexp.main
        conexp.contrib.dl.framework.syntax
        conexp.contrib.dl.framework.semantics
        conexp.contrib.dl.languages.EL-gfp-rewriting
        conexp.contrib.tests.dl.examples)
  (:use clojure.test))

;;;

(deftest test-minimal-elements
  (let [me @#'conexp.contrib.dl.languages.EL-gfp-rewriting/minimal-elements]
    (is (= '(1) (me [1 2 3 4 5 6] <)))
    (is (= '(6) (me [1 2 3 4 5 6] >)))
    (is (= '(1) (me [6 4 3 1 3 5] <)))
    (is (= '(1) (me [6 5 4 3 2 1] <)))
    (is (= #{#{1} #{2}} (set (me #{#{1} #{1 2 3} #{2 3} #{2} #{1 3}} subset?))))))

(deftest test-abbreviate-expression
  (let [ab @#'conexp.contrib.dl.languages.EL-gfp-rewriting/abbreviate-expression]
    (with-dl SimpleDL
      (is (= (ab (dl-expression (and (exists HasChild A)
                                     (exists HasChild B)))
                 #{(make-implication #{(dl-expression A)}
                                     #{(dl-expression B)})})
             (dl-expression (and (exists HasChild A)))))
      (is (= (ab (dl-expression (and (exists HasChild B)
                                     (exists HasChild A)))
                 #{(make-implication #{(dl-expression A)}
                                     #{(dl-expression B)})})
             (dl-expression (and (exists HasChild A)))))
      (is (= (ab (dl-expression (and (exists HasChild A)
                                     (exists HasChild B)
                                     (exists HasChild C)))
                 #{(make-implication #{(dl-expression A)}
                                     #{(dl-expression B)}),
                   (make-implication #{(dl-expression (exists HasChild A))}
                                     #{(dl-expression (exists HasChild C))})})
             (dl-expression (and (exists HasChild A)))))
      (is (= (ab (dl-expression (and A (exists HasChild A)))
                 #{(make-implication #{(dl-expression (exists HasChild A))}
                                     #{(dl-expression A)})})
             (dl-expression (and (exists HasChild A))))))))

(deftest test-normalize-EL-gfp-term
  (are [term norm-term] (= (normalize-EL-gfp-term 'term) 'norm-term)
       (and A B C D E D C B A)
       (and A B C D E),
       (and (exists HasChild (and (exists HasChild A) (exists HasChild B)))
            (exists HasChild (and (exists HasChild A))))
       (and (exists HasChild (and (exists HasChild A) (exists HasChild B))))))

;;;

nil
