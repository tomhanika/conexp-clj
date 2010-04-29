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

(deftest test-abbreviate-expression
  (with-dl SimpleDL
    (let [ab @#'conexp.contrib.dl.languages.EL-gfp-rewriting/abbreviate-expression]
    (is (= (ab (dl-expression (and (exists HasChild A)
                                   (exists HasChild B)))
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
