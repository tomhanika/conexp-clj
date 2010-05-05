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
  (:use clojure.test))

;;;

(defvar- ctx-1 (make-context #{0 1 2 3 4 5 6 7 8 9}
                             #{0 1 2 3 4 5 6 7 8 9}
                             #{[6 5] [1 0] [4 4] [9 9] [0 0] [1 1] [3 4] [6 7]
                               [8 9] [1 2] [3 5] [6 8] [0 2] [3 6] [5 8] [0 3]
                               [1 4] [0 4] [3 8] [0 5] [3 9] [2 9] [9 0] [7 0]
                               [8 1] [7 1] [9 3] [7 2] [7 3] [9 5] [6 3] [3 1]
                               [6 4] [8 6] [2 0]}))

(deftest test-make-association-rule
  (are [context-1 premise-1 conclusion-1] (let [ar (make-association-rule context-1 premise-1 conclusion-1)]
                                            (and (= (context ar) context-1)
                                                 (= (premise ar) (set premise-1))
                                                 (= (conclusion ar) (set conclusion-1))))
       ctx-1 #{} #{},
       ctx-1 #{1 2 3 4} #{5 6 7},
       ctx-1 #{} #{1},
       ctx-1 [] [])
  (are [context premise conclusion] (thrown? IllegalArgumentException
                                             (make-association-rule context premise conclusion))
       ctx-1 #{} '[a]
       ctx-1 '[a] #{}
       ctx-1 '[a] '[b]))

;;;

(deftest test-luxenburger-basis
  (are [context minsupp minconf] (let [ars (luxenburger-basis context minsupp minconf)]
                                   (forall [ar ars]
                                     (and (<= minsupp (support (union (premise ar) (conclusion ar))
                                                               context))
                                          (<= minconf (confidence ar)))))
       ctx-1 0.9 0.9
       ctx-1 0.5 0.5
       ctx-1 0 0))

;;;

nil
