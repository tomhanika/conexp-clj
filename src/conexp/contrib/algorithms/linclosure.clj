;; Copyright (c) Daniel Borchmann. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.contrib.algorithms.downing-gallier
  (:use [conexp.base :only (difference)]
        [conexp.fca.implications :only (premise conclusion make-implication)]))

;;;



;;;

(comment

  (require '[conexp.main :as cm])

  (let [subsets-10 (cm/subsets #{1 2 3 4 5 6 7 8 9 10})]
    (def- impls (cm/set-of (cm/make-implication A B)
                           [A subsets-10
                            B subsets-10])))

  (time (close-under-implications impls #{}))
  (time (cm/close-under-implications impls #{}))

  nil)

;;;

nil
