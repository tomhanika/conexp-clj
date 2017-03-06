;; Copyright ⓒ the conexp-clj developers; all rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.contrib.experimental.min-hyp-trans
  (:use conexp.base
        conexp.fca.contexts
        [clojure.math.combinatorics :only (selections)]))

;;;

(defn binomial [n k]
  (prod i 1 k
    (/ (- n (- k i))
       i)))

(defn all-contexts-by-type [p q]
  (let [bits (selections [0 1] (* p q))]
    (map #(make-context-from-matrix p q %) bits)))

;;;

(defn E-int [n m]
  (sum q 0 m
       (* (binomial m q)
          (sum r 0 n
               (* (binomial n r)
                  (expt 2 (- (* r q)))
                  (expt (- 1 (expt 2 (- r)))
                        (- m q))
                  (expt (- 1 (expt 2 (- q)))
                        (- n r)))))))

;;;

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
              1.0
              (map #(expt (- (expt 2.0 k) 1.0 k (- %))
                          (- (nth p (+ % 1))
                             (nth p %)
                             1.0))
                   (range 0 (+ k 1)))))
    (reduce +
            0.0
            (map #(minhypt-sum$ n k (inc j) (conj p %))
                 (range (+ 1 (nth p j))
                        (+ n (- k) (+ j 1) 1))))))

(defn minhypt-sum [n m k]
  (let [sums (minhypt-sum$ n k 0 [0])]
    (* (reduce * 1.0 (range 1.0 (+ 1 k)))
       (expt 2.0 (* n (- m k)))
       sums)))

(defn mean-minhypt-sum$ [n k j p]
  (if (>= j k)
    (let [p (conj p (+ n 1.0))]
      (* (expt 0.5 (* k k))
         (prod i 0 k
           (expt (- 1.0
                    (* (+ 1.0 k (- i))
                       (expt 0.5 k)))
                 (- (nth p (+ i 1))
                    (nth p i)
                    1.0)))))
    (sum i (+ 1.0 (nth p j))
           (+ n (- k) j 1.0)
      (mean-minhypt-sum$ n k (inc j) (conj p i)))))

(defn mean-minhypt-sum [n m k]
  (* (reduce * 1.0 (range 1.0 (+ 1.0 k)))
     (mean-minhypt-sum$ n k 0.0 [0.0])))

(defn E-pp [n m]
  (* (expt 0.5 n)
     (sum k 0 m
       (* (binomial (- m 1) k)
          (sum l 0 n
            (* (binomial n l)
               (mean-minhypt-sum l (- m 1) k)))))))

;;;

(defn print-data [number ratio]
  (doseq [m (range 1 number)]
    (println m (float (E-int (* ratio m) m)) (* m (E-pp (* ratio m) m)))))

;;;

nil
