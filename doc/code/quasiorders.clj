(ns conexp.contrib.experimental.quasi-orders
  (:use conexp.base
        conexp.fca.contexts)
  (:require [conexp.contrib.algorithms :as a]))

(defn quasi-context [n]
  (make-context (rest (drop-last (subsets (set-of-range n))))
                (set-of [a b] | a (set-of-range n)
                        b (set-of-range n)
                        :when (not= a b))
                (fn [m [a b]]
                  (or (contains? m a)
                      (not (contains? m b))))))

(defn count-quasiorders [n]
  (count (a/concepts :next-closure (quasi-context n))))
