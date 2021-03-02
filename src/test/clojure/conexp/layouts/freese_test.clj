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
        conexp.layouts.base
        conexp.layouts.freese)
  (:use clojure.test))

;;;

(deftest test-freese
  (let [lat (concept-lattice (rand-context 10 10 0.5))]
    (layout? (freese-layout lat))
    (layout? ((interactive-freese-layout lat) 0.0))))

;;;

nil
