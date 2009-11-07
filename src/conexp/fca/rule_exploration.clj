(ns conexp.fca.rule-exploration
  (:use conexp.base
	conexp.fca.implications))

(defn apply-to-implication [alpha impl]
  (make-implication (map alpha (premise impl))
		    (map alpha (conclusion impl))))

(defn position [x vec]
  (loop [pos 0
	 vec vec]
    (cond
      (empty? vec)
      nil
      (= x (first vec))
      pos
      :else
      (recur (inc pos) (rest vec)))))

(defn permutation
  ; hack only, slow and bad
  [source target]
  (fn [x]
    (nth target (position x source))))

nil
