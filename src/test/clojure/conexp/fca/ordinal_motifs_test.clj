(ns conexp.fca.ordinal-motifs-test
  (:use conexp.fca.contexts
        conexp.fca.ordinal-motifs)
  (:use clojure.test))

(deftest generate-scale-test
  (are [scale-type scale]
      (= (generate-scale scale-type 3)
         scale)
    :nominal (make-context [1 2 3] [1 2 3] #{[1 1] [2 2] [3 3]})
    :contranominal (make-context [1 2 3] [1 2 3] #{[1 2] [1 3] [2 1] [2 3] [3 1] [3 2]})
    :ordinal (make-context [1 2 3] [1 2 3] #{[1 1] [1 2] [1 3] [2 2] [2 3] [3 3]})
    :interordinal (make-context [1 2 3] [[1 0] [2 0] [3 0] [1 1] [2 1] [3 1]]
                                #{[1 [1 0]] [1 [2 0]] [1 [3 0]] [2 [2 0]] [2 [3 0]] [3 [3 0]]
                                  [1 [1 1]] [2 [1 1]] [2 [2 1]] [3 [1 1]] [3 [2 1]] [3 [3 1]]})
    :crown (make-context [1 2 3] [1 2 3] #{[1 1] [1 2] [2 2] [2 3] [3 3] [3 1]})))

(def exts1-list
  ['(#{} #{0} #{2} #{0 2} #{1 3} #{0 1 3} #{1 2 3} #{0 1 2 3})
   '(#{} #{0} #{1} #{2} #{3} #{0 2} #{1 3} #{0 1 3} #{1 2 3} #{0 1 2} #{0 1 2 3})
   '(#{} #{0} #{1} #{2} #{3} #{0 1} #{0 2} #{0 3} #{0 1 2} #{0 1 3} #{0 1 2 3})
   '(#{} #{0} #{1} #{2} #{3} #{0 1} #{1 2} #{2 3} #{0 1 2} #{1 2 3} #{0 1 2 3})
   '(#{} #{0} #{1} #{2} #{3} #{4} #{0 1} #{1 2} #{2 3} #{3 4} #{0 1 2} #{2 3 4} #{0 1 2 3} #{0 1 2 4} #{1 2 3 4} #{0 1 2 3 4})
   '(#{} #{0} #{1} #{2} #{3} #{1 4} #{0 1 2 3 4})
   '(#{0} #{0 1} #{0 4} #{0 1 2} #{0 1 2 3} #{0 1 2 3 4})])

(def exts-interordinal-list
  [#{#{} #{1} #{2} #{3} #{1 2} #{2 3} #{1 2 3}}
   #{#{} #{1} #{2} #{3} #{4} #{1 2} #{2 3} #{3 4} #{1 2 3} #{2 3 4} #{1 2 3 4}}
   #{#{} #{1} #{2} #{3} #{4} #{5} #{1 2} #{2 3} #{3 4} #{4 5} 
     #{1 2 3} #{2 3 4} #{3 4 5} #{1 2 3 4} #{2 3 4 5} #{1 2 3 4 5}}])

(def two-element-exts1
  (mapv (fn [exts]
          (filter #(= (count %) 2) exts))
        exts1-list))

(deftest identify-full-scale-measures-check-all-test
  (are [exts1 base-set exts2-set result]
      (= (boolean 
          (#'conexp.fca.ordinal-motifs/identify-full-scale-measures-check-all
           exts1 base-set exts2-set))
         result)
    (get exts1-list 0) #{0 1 2 3}   (get exts-interordinal-list 1) false
    (get exts1-list 2) #{0 1 2 3}   (get exts-interordinal-list 1) false
    (get exts1-list 3) #{0 1 2 3}   (get exts-interordinal-list 1) true
    (get exts1-list 4) #{0 1 2 3 4} (get exts-interordinal-list 2) false))

(deftest get-two-element-chain-test
  (are [two-element-exts chain]
      (= (#'conexp.fca.ordinal-motifs/get-two-element-chain two-element-exts)
         chain)
    (get two-element-exts1 1) nil
    (get two-element-exts1 2) nil
    (get two-element-exts1 3) [3 2 1 0]))

(deftest identify-full-scale-measures-interordinal-test
  (are [exts1 base-set exts2-set result]
      (= (boolean 
          (identify-full-scale-measures :interordinal exts1 base-set exts2-set))
         result)
    (get exts1-list 0) #{0 1 2 3}   (get exts-interordinal-list 1) false
    (get exts1-list 1) #{0 1 2 3}   (get exts-interordinal-list 1) false
    (get exts1-list 2) #{0 1 2 3}   (get exts-interordinal-list 1) false
    (get exts1-list 3) #{0 1 2 3}   (get exts-interordinal-list 1) true
    (get exts1-list 4) #{0 1 2 3 4} (get exts-interordinal-list 2) false
    (get exts1-list 4) #{0 1 2}     (get exts-interordinal-list 0) true))

(def exts-nominal-list
  [#{#{} #{1} #{2} #{3} #{4} #{1 2 3 4}}])

(deftest identify-full-scale-measures-nominal-test
  (are [exts1 base-set exts2-set result]
      (= (boolean
          (identify-full-scale-measures :nominal exts1 base-set exts2-set))
         result)
    (get exts1-list 1) #{0 1 2 3} (get exts-nominal-list 0) false
    (get exts1-list 5) #{0 1 2 3} (get exts-nominal-list 0) true))

(def exts-contranominal-list
  [#{#{} #{1} #{2} #{3} #{1 2} #{1 3} #{2 3} #{1 2 3}}
   #{#{} #{1} #{2} #{3} #{4} #{1 2} #{1 3} #{1 4} #{2 3} #{2 4} #{3 4}
     #{1 2 3} #{1 2 4} #{1 3 4} #{2 3 4} #{1 2 3 4}}])

(deftest identify-full-scale-measures-contranominal-test
  (are [exts1 base-set exts2-set result]
      (= (boolean 
          (identify-full-scale-measures :contranominal exts1 base-set exts2-set))
         result)
    (get exts1-list 0) #{0 1 2}   (get exts-contranominal-list 0) true
    (get exts1-list 1) #{0 1 2 3} (get exts-contranominal-list 1) false))

(def exts-ordinal-list
  [#{#{1} #{1 2} #{1 2 3} #{1 2 3 4}}])

(deftest identify-full-scale-measures-ordinal-test
  (are [exts1 base-set exts2-set result]
      (= (boolean
          (identify-full-scale-measures :ordinal exts1 base-set exts2-set))
         result)
    (get exts1-list 1) #{0 1 2 3} (get exts-ordinal-list 0) false
    (get exts1-list 6) #{0 1 2 3} (get exts-ordinal-list 0) true))

(def exts-crown-list
  [#{#{} #{1} #{2} #{3} #{1 2} #{2 3} #{1 3} #{1 2 3}}
   #{#{} #{1} #{2} #{3} #{4} #{1 2} #{2 3} #{3 4} #{4 1} #{1 2 3 4}}])

(deftest identify-full-scale-measures-crown-test
  (are [exts1 base-set exts2-set result]
      (= (boolean
          (identify-full-scale-measures :crown exts1 base-set exts2-set))
         result)
    (get exts1-list 0) #{0 1 2}   (get exts-crown-list 0) true
    (get exts1-list 1) #{0 1 2 3} (get exts-crown-list 1) false))

(deftest candidates-by-subset-heredity-test
  (let [complex #{#{} #{0} #{1} #{2} #{3} #{4} #{0 1} #{0 2} #{0 3} #{1 2}}]
    (are [complex objects subset-size result]
        (= (set
            (#'conexp.fca.ordinal-motifs/candidates-by-subset-heredity
             complex objects subset-size)))
      complex #{0 1 2 3 4} 3 #{#{0 1 2}}
      complex #{0 1 2 3 4} 2 #{#{0 1} #{0 2} #{0 3} #{0 4} #{1 2} #{1 3} #{1 4} #{2 3} #{2 4} #{3 4}}
      complex #{0 1 3 4} 3 #{#{}})))

(deftest is-of-scale-test
  (are [scale-type ctx result]
      (= (is-of-scale? scale-type ctx)
         result)
    :nominal (generate-scale :nominal 4) true
    :nominal (generate-scale :ordinal 4) false
    :contranominal (generate-scale :contranominal 4) true
    :contranominal (generate-scale :nominal 4) false
    :ordinal (generate-scale :ordinal 4) true
    :ordinal (generate-scale :nominal 4) false
    :interordinal (generate-scale :interordinal 4) true
    :interordinal (generate-scale :ordinal 4) false
    :crown (generate-scale :crown 4) true
    :crown (generate-scale :nominal 4) false))
