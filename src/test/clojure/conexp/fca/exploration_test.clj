;; Copyright ⓒ the conexp-clj developers; all rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.fca.exploration-test
  (:use conexp.base
        conexp.fca.contexts
        conexp.fca.implications
        conexp.fca.exploration
        conexp.fca.exploration.util)
  (:use clojure.test))

;;;

(def- testing-data
  [(make-context #{1 2 3 4} #{1 2 3 4}
                 #{[1 1] [1 2] [1 3]
                   [2 1] [2 2] [2 3]
                   [3 3] [4 3] [4 4]}),
   (one-context (set-of-range 5)),
   (diag-context (set-of-range 5)),
   (adiag-context (set-of-range 5)),
   (null-context (set-of-range 5)),
   (one-context #{}),
   (rand-context #{1 2 3 4 5} 0.3),
   (rand-context #{1 2 3 4 5} 0.4),
   (rand-context #{1 2 3 4 5} 0.5),
   (rand-context #{1 2 3 4 5} 0.6),
   (rand-context #{1 2 3 4 5} 0.7),
   ])

(deftest test-explore-attributes-is-stem-base
  (with-testing-data [ctx testing-data]
    (let [result (explore-attributes :context ctx :handler (constantly nil))]
      (and (= (set (stem-base ctx))
              (:implications result))
           (= ctx
              (:context result))))))

(defn- say-no [ctx known impl]
  [[(gensym) (premise impl)]])

(deftest test-explore-attributes-with-always-saying-no
  (with-testing-data [ctx testing-data]
    (= #{} (:implications (explore-attributes :context ctx :handler say-no)))))

(deftest test-explore-attributes-is-abortable
  (let [ctx (rand-context 10 0.5)
        imp #{(make-implication #{1} #{2})}
        res (explore-attributes :context ctx
                                :background-knowledge imp
                                :handler (fn [_ _ _] :abort))]
    (is (and (= ctx (:context res))
             (= #{} (:implications res))))))

(deftest test-explore-attributes-with-background-knowledge
  (with-testing-data [ctx testing-data]
    (= #{} (:implications (explore-attributes :context ctx
                                              :background-knowledge (set (canonical-base ctx))
                                              :handler #(is false))))))

(deftest test-explore-attributes-with-automorphisms
  (let [result (explore-attributes
                :context (make-context [] [1 2 3] [])
                :handler (fn [ctx known impl]
                           (when-not (or (respects? #{1} impl)
                                         (respects? #{2} impl))
                             (examples-by-automorphisms ctx
                                                        [(gensym) #{1}]
                                                        #{(fn [^long x]
                                                            (case x
                                                              1 2
                                                              2 1
                                                              3 3))}))))]
    (is (= (:implications result)
           #{(impl 3 ==> 1 2) (impl 1 2 ==> 3)}))
    (is (= 2 (count (objects (:context result)))))))

(deftest test-explore-attributes-with-incomplete-counterexamples
  (let [result (explore-attributes :context (make-context [] '[black blue green] [])
                                   :incomplete-counterexamples true
                                   :handler (fn [_ _ _ impl]
                                              [[(gensym) (premise impl) (conclusion impl)]]))]
    (is (empty? (:implications result)))))

;;;

nil
