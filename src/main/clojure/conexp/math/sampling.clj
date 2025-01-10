;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.math.sampling
  "Sampling function for posets. The algorithm is designed for uniform
   sampling. 
   Algorithm from:
   'Minimals Plus: An improved algorithm for the random
   generation of linear extensions of partially ordered sets'
   Elías F. Combarro, Julen Hurtado de Saracho, Irene Díaz
   https://www.sciencedirect.com/science/article/pii/S0020025519305043 "
  (:use conexp.base
        conexp.math.algebra)
  (:require [clojure.set :refer [difference union subset? intersection]]))

;; Sampler

(defn- get-levels
  "Computes the 'height' for each element of the base set."
  [poset]
  (let [relation  (order poset)
        level-map (atom {})
        base-set  (base-set poset)
        t         (atom (count base-set))]
    (loop [Q base-set
           h 1]
      (if (= @t 0) 
          @level-map
          (let [maximal (atom true)] 
            (swap! level-map assoc h #{}) 
            (doseq [a Q]
              (do
                (reset! maximal true)
                (doseq [b Q]
                  (if (and (relation a b)
                           (not= a b))
                      (reset! maximal false)))
                (if @maximal
                    ;; a is maximal among the remaining elements
                    (do (swap! t dec)
                        (swap! level-map update-in [h] conj a)))))
            (recur (difference Q (get @level-map h)) 
                   (inc h)))))))

(defn- get-covers
  "For each element computes the set of their covers."
  [order-relation level-map]
  (let [cover-map (atom {})]
    (doseq [[k v] level-map]
      (doseq [a v]
        (do
          (swap! cover-map assoc a #{})
          (doseq [l (reverse (range 1 k))]
            (doseq [b (get level-map l)]
              (if (order-relation a b)
                  (let [covers (atom true)]
                    (doseq [c (get @cover-map a)]
                      (if (order-relation c b)
                          (reset! covers false)))
                    (if @covers
                        (swap! cover-map update-in [a] conj b)))))))))
  @cover-map))

(defn- get-AB 
  "Computes thow helper hash-maps for the weight calculation."
  [order-relation level-map A-map B-map]
   (doseq [[k v] level-map]
     (do
       (swap! A-map assoc k #{})
       (swap! B-map assoc k #{})
       (doseq [a v]
         (let [contained (atom true)]
           (doseq [l (reverse (range 1 k))]
             (doseq [b (get level-map l)]
               (if (not (order-relation a b)) 
                   (reset! contained false))))
           (if @contained 
               (swap! A-map update-in [k] conj a)
               (swap! B-map update-in [k] conj a)))))))

(defn- get-weights
  "Computes the Minimals Plus weights for each element."
  [poset level-map cover-map]
  (let [base       (base-set poset)
        order      (order poset)
        A-map      (atom {})
        B-map      (atom {})
        weight-map (atom {})]
    (get-AB order level-map A-map B-map)
    (doseq [[k v] level-map]
      (if (empty? (get B-map k)) 
          (doseq [a v]
            (swap! weight-map assoc a 1))
          (let [a-union (apply union (map #(get level-map %) (range 1 k)))
                b-union (union a-union (get B-map k))] 
            (doseq [a (get B-map k)]
              (let [b (first (filter #(and (not (order a %))
                                           (subset? (get cover-map %) 
                                                    (set 
                                                      (filter 
                                                        (fn [x](order a x)) 
                                                        base)))) 
                                     a-union))] 
                (swap! weight-map assoc a 
                  (+ (get @weight-map b) 
                     (apply + (map #(get @weight-map %)
                                   (difference 
                                     (get cover-map a)
                                     (filter #(order b %) base)))))))
            (doseq [a (get A-map k)]
              (let [b (first (filter #(and (not (order a %))
                                           (subset? (get cover-map %) 
                                                    (set 
                                                      (filter 
                                                        (fn [x](order a x)) 
                                                        base)))) 
                                       b-union))] 
                (swap! weight-map assoc a 
                  (+ (get @weight-map b) 
                     (apply + (map #(get @weight-map %)
                                   (difference 
                                     (get cover-map a)
                                     (filter #(order b %) base))))))))))))
    @weight-map))

(defn- get-cover-numbers
  "Returns the number of covered elemnts per element as map."
  [poset cover-map]
  (let [cover-num-map (atom {})
        base          (base-set poset)]
    (doseq [a base]
      (swap! cover-num-map assoc a 0))
    (doseq [b base]
      (doseq [a (get cover-map b)] 
        (swap! cover-num-map update-in [a] inc)))
    @cover-num-map))

(defn minimals-plus
  "Sampling function for posets. The algorithm is designed for uniform
   sampling. 
   Algorithm from:
   'Minimals Plus: An improved algorithm for the random
   generation of linear extensions of partially ordered sets'
   Elías F. Combarro, Julen Hurtado de Saracho, Irene Díaz
   https://www.sciencedirect.com/science/article/pii/S0020025519305043 "
  [poset sample-num]
  (let [base           (base-set poset)
        order          (order poset)
        ;; run subfunctions to initialize minimal plus
        level-map      (get-levels poset)
        cover-map      (get-covers order level-map)
        weight-map     (get-weights poset level-map cover-map)
        cover-num-map  (get-cover-numbers poset cover-map)]
    (if (empty? base) 
        (repeat sample-num [])
        (for [j (range sample-num)]
        ;; generate sample
          (let [extension   (atom [])
                local-cover (atom cover-num-map)
                A           (atom (get level-map (apply max (keys level-map))))
                w           (atom (list))] 
            ;; starting set of elements
            (doseq [a @A]
              (swap! w concat (repeat (get weight-map a) a)))
            (doseq [i (range (count base))]
              ;; chose random element
              (let [a (first (shuffle @w))]
                (swap! extension conj a)
                (swap! A difference #{a})
                (swap! w (fn [k] (filter #(not= % a) k)))
                ;; extend set of chosable elements
                (doseq [b (get cover-map a)]
                  (swap! local-cover update-in [b] - 1)
                  (if (= 0 (get @local-cover b))
                      (do (swap! A conj b)
                          (swap! w concat (repeat (get weight-map b) b)))))))
            @extension)))))

;;;

nil
