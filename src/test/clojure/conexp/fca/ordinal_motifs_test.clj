(ns conexp.fca.ordinal-motifs-test
  (:use conexp.fca.ordinal-motifs)
  (:use clojure.test))

(def exts1-list
  ['(#{} #{0} #{2} #{0 2} #{1 3} #{0 1 3} #{1 2 3} #{0 1 2 3})
   '(#{} #{0} #{1} #{2} #{3} #{0 2} #{1 3} #{0 1 3} #{1 2 3} #{0 1 2} #{0 1 2 3})
   '(#{} #{0} #{1} #{2} #{3} #{0 1} #{0 2} #{0 3} #{0 1 2} #{0 1 3} #{0 1 2 3})
   '(#{} #{0} #{1} #{2} #{3} #{0 1} #{1 2} #{2 3} #{0 1 2} #{1 2 3} #{0 1 2 3})
   '(#{} #{0} #{1} #{2} #{3} #{4} #{0 1} #{1 2} #{2 3} #{3 4} #{0 1 2} #{2 3 4} #{0 1 2 3} #{0 1 2 4} #{1 2 3 4} #{0 1 2 3 4})])

(def exts2-list
  [#{#{} #{1} #{2} #{3} #{4} #{1 2} #{2 3} #{3 4} #{1 2 3} #{2 3 4} #{1 2 3 4}}
   #{#{} #{1} #{2} #{3} #{4} #{5} #{1 2} #{2 3} #{3 4} #{4 5} #{1 2 3} #{2 3 4} #{3 4 5} #{1 2 3 4} #{2 3 4 5} #{1 2 3 4 5}}])

(def two-element-exts1
  (mapv (fn [exts]
          (filter #(= (count %) 2) exts))
        exts1-list))

(deftest get-two-element-chain-test
  (are [two-element-exts chain]
      (= (#'conexp.fca.ordinal-motifs/get-two-element-chain two-element-exts)
         chain)
    (get two-element-exts1 1) nil
    (get two-element-exts1 2) nil
    (get two-element-exts1 3) [3 2 1 0]))

(deftest identify-full-scale-measures-check-interordinal-test
  (are [exts1 base-set exts2-set result]
      (= (boolean 
          (#'conexp.fca.ordinal-motifs/identify-full-scale-measures-check-interordinal exts1 base-set exts2-set))
         result)
    (get exts1-list 0) #{0 1 2 3} (get exts2-list 0) false
    (get exts1-list 1) #{0 1 2 3} (get exts2-list 0) false
    (get exts1-list 2) #{0 1 2 3} (get exts2-list 0) false
    (get exts1-list 3) #{0 1 2 3} (get exts2-list 0) true
    (get exts1-list 4) #{0 1 2 3 4} (get exts2-list 1) false))

(deftest candidates-by-subset-heredity-test
  (let [complex #{#{} #{0} #{1} #{2} #{3} #{4} #{0 1} #{0 2} #{0 3} #{1 2}}]
    (are [complex objects subset-size result]
        (= (set
            (#'conexp.fca.ordinal-motifs/candidates-by-subset-heredity
             complex objects subset-size)))
      complex #{0 1 2 3 4} 3 #{#{0 1 2}}
      complex #{0 1 2 3 4} 2 #{#{0 1} #{0 2} #{0 3} #{0 4} #{1 2} #{1 3} #{1 4} #{2 3} #{2 4} #{3 4}}
      complex #{0 1 3 4} 3 #{#{}})))
