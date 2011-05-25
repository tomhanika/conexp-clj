(ns conexp.contrib.experimental.min-hyp-trans
  (:use conexp.main
        [clojure.contrib.combinatorics :only (selections)]))

;;;

(defn all-contexts-by-type [p q]
  (let [bits (selections [0 1] (* p q))]
    (map #(make-context-from-matrix p q %) bits)))

(defn min-hypergraph-transversal? [ctx Q]
  (boolean
   (and (forall [m Q]
          (exists [g (objects ctx)]
           (= #{m} (intersection Q (object-derivation ctx #{g})))))
        (forall [g (objects ctx)]
          (not-empty (intersection Q (object-derivation ctx #{g})))))))

(defn min-hypergraph-transversals [p q Q]
  (loop [ctxs     (all-contexts-by-type p q),
         mhyp-cnt 0N]
    (if (empty? ctxs)
      mhyp-cnt
      (recur (rest ctxs)
             (if (min-hypergraph-transversal? (first ctxs) Q)
               (inc mhyp-cnt)
               mhyp-cnt)))))

(defn minhypt-sum$ [n k j p]
  (if (>= j k)
    (let [p (conj p (+ n 1))]
      (reduce *
              1N
              (map #(expt (- (expt 2N k) 1N k (- %))
                          (- (nth p (+ % 1))
                             (nth p %)
                             1N))
                   (range 0 (+ k 1)))))
    (reduce +
            0N
            (map #(minhypt-sum$ n k (inc j) (conj p %))
                 (range (+ 1 (nth p j))
                        (+ n (- k) (+ j 1) 1))))))

(defn minhypt-sum [n m k]
  (let [sums (minhypt-sum$ n k 0 [0])]
    (* (reduce * 1N (range 1N (+ 1 k)))
       (expt 2N (* n (- m k)))
       sums)))

(defn mean-minhypt-sum$ [n k j p]
  (if (>= j k)
    (let [p (conj p (+ n 1.0))]
      (reduce *
              (expt 0.5 (* k k))
              (map #(expt (- 1.0
                             (* (+ 1.0 k (- %))
                                (expt 0.5 k)))
                          (- (nth p (+ % 1))
                             (nth p %)
                             1.0))
                   (range 0.0 (+ k 1.0)))))
    (reduce +
            0.0
            (map #(mean-minhypt-sum$ n k (inc j) (conj p %))
                 (range (+ 1.0 (nth p j))
                        (+ n (- k) (+ j 1.0) 1.0))))))

(defn mean-minhypt-sum [n m k]
  (* (reduce * 1.0 (range 1.0 (+ 1.0 k)))
     (mean-minhypt-sum$ n k 0.0 [0.0])))

(defn binomial [n k]
  (prod i 1 k
        (/ (- n (- k i))
           i)))

(defn expected-number-of-proper-premises [n m]
  (* (expt 0.5 n)
     (sum k 0 m
          (* (binomial (- m 1) k)
             (sum l 0 n
                  (* (binomial n l)
                     (mean-minhypt-sum l (- m 1) k)))))))

;;;

nil