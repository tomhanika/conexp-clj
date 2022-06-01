;; Copyright ⓒ the conexp-clj developers; all rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.fca.protoconcepts-test
  (:use conexp.base
        conexp.fca.contexts
        conexp.fca.protoconcepts)
  (:use clojure.test))

(def test-context-01 (make-context #{"a" "b" "c"}
                                   #{0 1 2 3}
                                   #{["a" 2] ["a" 3] 
                                     ["b" 1] ["b" 3]
                                     ["c" 0] ["c" 1]}))

(deftest test-preconcept
  ;; test all preconcepts of test-context-01
  (are [object-set attribute-set] (preconcept? test-context-01 [object-set attribute-set])
    #{} #{}
    #{} #{0}
    #{} #{1}
    #{} #{2}
    #{} #{3}
    #{} #{0 1}
    #{} #{0 2}
    #{} #{0 3}
    #{} #{1 2}
    #{} #{1 3}
    #{} #{2 3}
    #{} #{0 1 2}
    #{} #{0 1 3}
    #{} #{0 2 3}
    #{} #{1 2 3}
    #{} #{0 1 2 3}
    #{"a"} #{}
    #{"a"} #{2}
    #{"a"} #{3}
    #{"a"} #{2 3}
    #{"b"} #{}
    #{"b"} #{1}
    #{"b"} #{3}
    #{"b"} #{1 3}
    #{"c"} #{}
    #{"c"} #{0}
    #{"c"} #{1}
    #{"c"} #{0 1}
    #{"a" "b"} #{}
    #{"a" "b"} #{3}
    #{"a" "c"} #{}
    #{"b" "c"} #{}
    #{"b" "c"} #{1}
    #{"a" "b" "c"} #{})
  ;; test some non-preconcepts of test-context-01
  (are [object-set attribute-set] (not (preconcept? test-context-01 [object-set attribute-set]))
    #{"a"} #{0}
    #{"a"} #{1}
    #{"a"} #{0 1}
    #{"a"} #{0 2}
    #{"a"} #{0 3}
    #{"b"} #{0 3}
    #{"b"} #{1 2}
    #{"b"} #{2 3}
    #{"b"} #{1 2 3}
    #{"b"} #{0 2 3}
    #{"b"} #{0 1 2 3}
    #{"c"} #{2}
    #{"c"} #{3}
    #{"c"} #{1 2}
    #{"c"} #{1 3}
    #{"c"} #{0 1 2 3}))

(deftest test-protoconcept
  ;; test all protoconcepts of test-context-01
  (are [object-set attribute-set] (protoconcept? test-context-01 [object-set attribute-set])
    #{} #{0 2}
    #{} #{0 3}
    #{} #{1 2}
    #{} #{0 1 2}
    #{} #{0 1 3}
    #{} #{0 2 3}
    #{} #{1 2 3}
    #{} #{0 1 2 3}
    #{"a"} #{2}
    #{"a"} #{2 3}
    #{"b"} #{1 3}
    #{"c"} #{0}
    #{"c"} #{0 1}
    #{"a" "b"} #{3}
    #{"a" "c"} #{}
    #{"b" "c"} #{1}
    #{"a" "b" "c"} #{})
  ;; test some non-protoconcepts of test-context-01
  (are [object-set attribute-set] (not (protoconcept? test-context-01 [object-set attribute-set]))
    #{"a"} #{0}
    #{"a"} #{1}
    #{"a"} #{0 1}
    #{"a"} #{0 2}
    #{"a"} #{0 3}
    #{"b"} #{0 3}
    #{"b"} #{1 2}
    #{"b"} #{2 3}
    #{"b"} #{1 2 3}
    #{"b"} #{0 2 3}
    #{"b"} #{0 1 2 3}
    #{"c"} #{2}
    #{"c"} #{3}
    #{"c"} #{1 2}
    #{"c"} #{1 3}
    #{"c"} #{0 1 2 3}
    #{} #{}
    #{} #{0}
    #{} #{1}
    #{} #{2}
    #{} #{3}
    #{} #{0 1}
    #{} #{1 3}
    #{} #{2 3}
    #{"a"} #{}
    #{"a"} #{3}
    #{"b"} #{}
    #{"b"} #{1}
    #{"b"} #{3}
    #{"c"} #{}
    #{"c"} #{1}
    #{"a" "b"} #{}
    #{"b" "c"} #{}))
