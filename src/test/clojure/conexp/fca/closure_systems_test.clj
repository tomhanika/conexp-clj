;; Copyright ⓒ the conexp-clj developers; all rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.fca.closure-systems-test
  (:use clojure.test
        conexp.fca.closure-systems
        conexp.base)
  (:require [clojure.set :refer [difference intersection union subset?]]))

(deftest test-lectic-<_i
  (is (lectic-<_i [5 7 3 2 1] 2 #{5 3 1} #{5 3 2 1}))
  (is (lectic-<_i [5 7 3 2 1] 5 #{3} #{5 7}))
  (is (lectic-<_i [5 7 3 2 1] 3 #{2} #{3}))
  (is (lectic-<_i [5 7 3 2 1] 1 #{} #{1}))
  (is (not (lectic-<_i [5 7 3 2 1] 5 #{5 3 2} #{5 7 3 2})))
  (is (not (lectic-<_i [5 7 3 2 1] 2 #{5 7 3} #{5 7 2})))
  (is (lectic-<_i [1 nil] nil #{1} #{1 nil})))

(deftest test-lectic-<
  (is (lectic-< [5 7 3 2 1] #{} #{5}))
  (is (lectic-< [5 7 3 2 1] #{7 2 1} #{5}))
  (is (not (lectic-< [5 7 3 2 1] #{5 7 3 2 1} #{7 3 2 1}))))

(deftest test-next-closed-set-in-family
  (are [set next] (= (next-closed-set-in-family #(< (count %) 3)
                                                [3 2 0 1]
                                                identity
                                                set)
                     next)
       #{} #{1}
       #{1} #{0}
       #{0} #{0 1}
       #{0 1} #{2}
       #{2} #{2 1}
       #{2 1} #{2 0}
       #{2 0} #{3}
       #{3} #{3 1}
       #{3 1} #{3 0}
       #{3 0} #{3 2}
       #{3 2} nil))

(deftest test-improve-basic-order
  (is (= #{1 2 3} (set (improve-basic-order [3 2 1] #(conj % 1)))))
  (is (= #{} (set (improve-basic-order [] identity)))))

(deftest test-all-closed-sets-in-family
  (is (= '(#{2} #{1 2} #{0 2} #{3} #{1 3} #{0 3} #{2 3})
         (all-closed-sets-in-family #(< (count %) 3) [3 2 0 1] identity #{2})))
  (is (= '(#{0} #{0 3} #{0 2} #{0 1})
         (all-closed-sets-in-family #(< (count %) 3) [0 1 2 3] #(conj % 0) #{})))
  (is (= '(#{0} #{0 3} #{0 2} #{0 1})
         (all-closed-sets-in-family #(< (count %) 3) [0 1 2 3] #(conj % 0) #{0}))))

(deftest test-next-closed-set
  (are [set next] (= (next-closed-set [3 2 1] identity set) next)
       #{}      #{1}
       #{1}     #{2}
       #{2}     #{2 1}
       #{2 1}   #{3}
       #{3}     #{3 1}
       #{3 1}   #{3 2}
       #{3 2}   #{3 2 1}
       #{3 2 1} nil)
  (are [set next] (= (next-closed-set [1 nil] identity set) next)
       #{}      #{nil}
       #{nil}   #{1}
       #{1}     #{1 nil}
       #{1 nil} nil))

(deftest test-all-closed-sets
  (is (= (all-closed-sets [5 7 3 2 1] #(union % #{3 2 1}))
         (seq [#{3 2 1} #{7 3 2 1} #{5 3 2 1} #{5 7 3 2 1}])))
  (is (= (all-closed-sets [3 2 1] identity)
         (seq [#{} #{1} #{2} #{2 1} #{3} #{3 1} #{3 2} #{3 2 1}])))
  (is (= (all-closed-sets [1 nil] identity)
         (seq [#{} #{nil} #{1} #{1 nil}]))))

(defn clop-by-subsets [base subsets]
  (fn [X]
    (reduce intersection base (filter #(subset? X %) subsets))))

(deftest test-parallel-closures
  (dotimes [i 13]
    (is (= (expt 2 (+ i 1))
           (count (parallel-closures (set-of-range (+ i 2)) #(conj % 1))))))
  (are [base subsets] (= (set (all-closed-sets base (clop-by-subsets base subsets)))
                         (set (parallel-closures base (clop-by-subsets base subsets))))
    #{1 2 3 4} [#{1 2} #{3 4}]
    #{1 2 3 4} [#{1} #{2} #{3}]
    #{1 2 3 5 7} [#{1 2 3} #{1 2 3 5} #{1 2 3 7}]))

;;;

(deftest test-non-closed-elements
  (is (= (non-closed-elements #{}
                              (clop-by-subsets #{} #{}))
         #{}))
  (is (= (non-closed-elements #{1 2 3 4 5 6}
                              (clop-by-subsets 
                                #{1 2 3 4 5 6} 
                                #{#{1 3} #{2 4} #{5} #{6}}))
         #{1 2 3 4}))
  (is (= (non-closed-elements #{'a 'b 'c 'd}
                              (clop-by-subsets 
                                #{'a 'b 'c 'd}
                                #{#{'a 'b} #{'c} #{'d}}))
         #{'a 'b})))

(deftest test-exclusive-closure
  (is (= (exclusive-closure #{1}
                            (clop-by-subsets 
                              #{1 2 3 4 5 6} 
                              #{#{1 3} #{2 4} #{5} #{6}}))
         #{3}))
  (is (= (exclusive-closure #{'a}
                              (clop-by-subsets 
                                #{'a 'b 'c 'd}
                                #{#{'a 'b} #{'c} #{'d}}))
         #{'b}))
  (is (= (exclusive-closure #{3}
                            (clop-by-subsets 
                              #{1 2 3 4 5 6 7} 
                              #{#{1 7} #{2 4} #{3 4 5} #{5} #{6}}))
         #{4 5})))

;;;

(deftest test-subsets
  (is (= #{#{} #{1} #{2} #{1 2}}
         (set (subsets #{1 2}))))
  (are [x y] (= y (count (subsets (set (range x)))))
        0    1
        1    2
        2    4
        8  256
       10 1024)
  (are [my-set] (and (forall [s (subsets my-set)] (subset? s my-set))
                     (exists [s (subsets my-set)] (or (not (proper-subset? s my-set))
                                                      (empty? s))))
       #{}
       #{1 2}
       #{'a 4}
       #{+ 3}
       #{2 3 4 'r -}))

(deftest test-transitive-closure
  (are [x y] (= (transitive-closure x) y)
       #{[1 2] [2 1]}   #{[1 1] [2 2] [1 2] [2 1]}
       #{}              #{}
       #{[1 'a]}        #{[1 'a]}
       #{[+ -] [- *]}   #{[+ -] [- *] [+ *]}
       #{[1 2] [2 3] [3 4]} #{[1 2] [2 3] [3 4] [1 3] [1 4] [2 4]}))

(deftest test-reflexive-transitive-closure
  (is (= #{[1 2] [1 1] [2 2]}
         (reflexive-transitive-closure [1 2] #{[1 2]})))
  (is (= #{[nil nil] ['a nil] ['a 'a]}
         (reflexive-transitive-closure [nil 'a] #{['a nil]}))))

(deftest test-transitive-reduction
  (is (= #{[1 2] [2 3]}
         (transitive-reduction #{[1 2] [2 3] [1 3]})))
  (is (empty? (transitive-reduction (cross-product [1 2 3] [1 2 3]))))
  (is (= #{[1 2] [2 3] [3 4] [4 5]}
         (transitive-reduction [1 2 3 4 5] <)))
  (is (let [subs   (subsets #{1 2 3 4 5}),
            reduct (transitive-reduction (subsets #{1 2 3 4 5 6 7})
                                         proper-subset?)]
        (forall [[x y] reduct, z subs]
          (not (and (not= x z)
                    (not= z y)
                    (proper-subset? x z)
                    (proper-subset? z y)))))))

(deftest test-graph-of-function?
  (are [rel src trg] (graph-of-function? rel src trg)
       #{[1 2] [2 1]} #{1 2} #{1 2}
       #{} #{} #{}
       #{} #{} #{1 2 3}
       #{[1 2] [2 3] [3 1]} #{1 2 3} #{1 2 3 4 5}
       #{[1 1] [2 1] [3 1]} #{1 2 3} #{1 7})
  (are [rel src trg] (not (graph-of-function? rel src trg))
       #{[1 1] [1 2]} #{1 2} #{1 2}
       #{} #{1} #{}
       #{[1 2] [2 3] [3 4]} #{1 2 3} #{1 2 3}
       #{[1 2] [2 2]} #{1} #{2}))

(deftest test-minimal-generating-sets
  (are [set clop minimal-generators] (= (set minimal-generators)
                                        (set (minimal-generating-subsets clop set)))
    #{1} identity [#{1}],
    #{} #(conj % 1) [#{}],
    #{1 2 3 4 5}
    #(if (< (count %) 3)
       %
       #{1 2 3 4 5})
    [#{3 4 5} #{2 4 5} #{2 3 5} #{2 3 4} #{1 4 5} #{1 3 5} #{1 3 4} #{1 2 5} #{1 2 4} #{1 2 3}]))

(deftest test-partial-min
  (are [order seq minimals] (= (set minimals) (set (partial-min order seq)))
    <= [1 2 3 4] [1],
    subset? [#{1 2 3} #{1 2} #{1 3}] [#{1 2} #{1 3}]))

(deftest test-partial-max
  (are [order seq minimals] (= (set minimals) (set (partial-max order seq)))
    <= [1 2 3 4] [4],
    subset? [#{1 2 3} #{1 2} #{1 3}] [#{1 2 3}],
    subset? [#{2 3 4} #{1 2 3} #{1 2} #{1}] [#{2 3 4} #{1 2 3}]))
