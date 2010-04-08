;; Copyright (c) Daniel Borchmann. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.contrib.tests.dl.util.general-sorted-sets
  (:use conexp.main
	conexp.contrib.dl.util.general-sorted-sets)
  (:use [clojure.contrib.seq :only (seq-on)]
	clojure.test))

;;;

(defn- no-equivalent-elements []
  (are [num order elts] (= num (count (let [gss (make-general-sorted-set order)]
					(doseq [elt elts]
					  (add-to-gss! gss elt))
					(seq-on gss))))
       4 <= [1 2 3 4 4 3 2 1],
       4 subset? [#{} #{1} #{2} #{1 2} #{2} #{1 2} #{}]
       6 subset? [#{1} #{2} #{3} #{4} #{1 3} #{3 4} #{1} #{2}]))

(defn- finds-correct-neighbours []
  (are [order elts new-elt lowers uppers] (let [gss (make-general-sorted-set order)]
					    (doseq [elt elts]
					      (add-to-gss! gss elt))
					    (and (= (set lowers)
						    (set (map :node
							      (@#'conexp.contrib.dl.util.general-sorted-sets/find-lower-neighbours gss new-elt))))
						 (= (set uppers)
						    (set (map :node
							      (@#'conexp.contrib.dl.util.general-sorted-sets/find-upper-neighbours gss new-elt))))))
       <= [1 2 4 5] 3 [2] [4]
       subset? [#{} #{1} #{2} #{3} #{1 2 3} #{1 2}] #{1 2 3 4} [#{1 2 3}] []))

(deftest test-general-sorted-sets
  (no-equivalent-elements)
  (finds-correct-neighbours))

;;;

nil
