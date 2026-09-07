;; Copyright ⓒ the conexp-clj developers; all rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.layouts.freese-test
  (:use conexp.fca.contexts
        conexp.fca.lattices
        conexp.fca.posets
        conexp.layouts.base
        conexp.layouts.freese)
  (:use clojure.test))

;;;

(deftest test-freese
  ;; The context is held and named in the assertion messages.  This test draws a
  ;; random lattice, and has been seen to fail about once in twenty-five runs of
  ;; the suite while never failing in two thousand runs on its own, so the input
  ;; is the one thing needed to chase it and the one thing a bare failure did
  ;; not report.
  (let [ctx (rand-context 10 10 0.5)
        lat (concept-lattice ctx)
        ctx-msg (str "context: " (pr-str ctx))]
    (is (layout? (freese-layout lat)) ctx-msg)
    (is (layout? ((interactive-freese-layout lat) 0.0)) ctx-msg))
  (let [poset (make-poset [1 2 3 4 5 6]
                        (fn [A B] 
                          (contains? #{[1 1] [1 2] [1 3] [1 5] [1 6]
                                       [2 2] [2 6] [3 3] [3 6]
                                       [4 4] [4 5] [5 5] [6 6]} [A B])))]
    (is (layout? (freese-layout poset)))
    (is (layout? ((interactive-freese-layout poset) 0.0)))))

;;;

nil
