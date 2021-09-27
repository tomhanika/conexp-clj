(ns conexp.fca.factorization
    (:require [conexp.base :refer :all]
              [conexp.fca.fast :refer [to-binary-matrix]]
              [conexp.fca.contexts :refer :all])
    (:gen-class))

(defn- calcOneMatrix
"Calculates Matrix with 2 vectors"
  [A B]
  (into []
    (let [X []]
      (for [a A]
       (into X
          (for [b B] (if (and (= a 1) (= b 1)) 1 0))
        )
      )
    )
  )
)

(defn- unite
"Unite-Operator for 2 matrices"
  [A B]
  (loop [i 0 C []]
    (if (>= i (count A))
      C
      (recur (inc i) (conj C (into [] (map (fn [x y] (if (or (= 1 x) (= 1 y)) 1 0)) (get A i) (get B i)))))
    )
  )
)

(defn calcAssoMatrix
"Creates binary Matrix from asso-algo result vectors"
    [map]
    (loop [i 0 end (calcOneMatrix (nth (get map :s) i) (nth (get map :b) i))]
      (if (>= i (count (get map :b)) i)
        (into [] (apply concat end))
        (recur (inc i) (unite end (calcOneMatrix (nth (get map :s) i) (nth (get map :b) i))))
      )
    )
  )

(defn calcPandaMatrix
 "Creates binary Matrix from panda-algo result vectors"
  [pi]
  (loop [i 0 end (calcOneMatrix (get (nth pi i) :ci) (get (nth pi i) :ct))]
    (if (>= i (count pi) i)
      (into [] (apply concat end))
      (recur (inc i) (unite end (calcOneMatrix (get (nth pi i) :ci) (get (nth pi i) :ct))))
    )
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;https://doi.org/10.1109/TKDE.2008.53
(defn- calcUnionMatrixAsso
  [B S i j]
  (if (empty? (get B 0))
    (calcOneMatrix (vec (repeat i 0)) (vec (repeat j 0)))
    (loop [i 0 end (calcOneMatrix (get B i) (get S i))]
      (if (>= i (count B) i)
        end
        (recur (inc i) (unite end (calcOneMatrix (get B i) (get S i))))
      )
    )
  )
)

(defn- countFalseOnesAsso
  [C D]
  (loop [i 0 j 0 c 0]
    (if (> j (count C))
      c
      (recur (cond (> i (count (get C 0))) 0 :else (inc i)) (cond (> i (count (get C 0))) (inc j) :else j) (cond (and (= (get (get C i) j) 0) (= (get (get D i) j) 1)) (inc c) :else c))
    )
  )  
)

(defn- countOnesAsso
  [C D]
  (loop [i 0 j 0 c 0]
    (if (> j (count C))
      c
      (recur (cond (> i (count (get C 0))) 0 :else (inc i)) (cond (> i (count (get C 0))) (inc j) :else j) (cond (and (= (get (get C i) j) 1) (= (get (get D i) j) 1)) (inc c) :else c))
    )
  )  
)

(defn- arbitraryVector
  [n]
  (if (= 1 (mod n 2))
    (clojure.math.combinatorics/permutations (into (vec (repeat (+ 1 (quot n 2)) 0)) (vec (repeat (quot n 2) 1))))
    (clojure.math.combinatorics/permutations (into (vec (repeat (quot n 2) 0)) (vec (repeat (quot n 2) 1))))
  )
)

(defn- constructA
  [C tau]
  (into []
    (let [X []]
      (for [ai (apply mapv vector C)] 
        (into X 
          (for [aj (apply mapv vector C)]
            (if (< 0 (reduce + (map * aj aj)))
              (if (< tau ( / (reduce + (map * ai aj)) (reduce + (map * aj aj)))) 1 0)
            0
            )
         )
        )
      )
    )  
  )
)

(defn- cover
  [C newB newS B S weightPos weightNeg]
  (let [uni (unite (calcOneMatrix newB newS) (calcUnionMatrixAsso B S (count newB) (count newS)))]
    {:b newB :s newS :value (conj [] (- (* weightPos (countOnesAsso C uni)) (* weightNeg (countFalseOnesAsso C uni))))}
  )
)

(defn- loopMatrix
  [C A B S weightPos weightNeg]
  (let [AT (arbitraryVector (count C))] 
    (loop [i 0 j 0 bestMatch {:b (get A i) :s (nth AT j) :value (get (cover C (get A 0) (nth AT 0) B S weightPos weightNeg) :value)}]
      (if (> i (count A))
        bestMatch
        (recur 
          (cond (> j (- (count AT) 2)) (inc i) :else i)
          (cond (> j (- (count AT) 2)) 0 :else (inc j))
          (cond (< (get (get bestMatch :value) 0) (get (get (cover C (get A i) (nth AT j) B S weightPos weightNeg) :value) 0))
           {:b (get (cover C (get A i) (nth AT j) B S weightPos weightNeg) :b)
            :s (get (cover C (get A i) (nth AT j) B S weightPos weightNeg) :s)
            :value (get (cover C (get A i) (nth AT j) B S weightPos weightNeg) :value)}
            :else bestMatch)          
        )
      )
    )  
  )
)


(defn- coverPairs
  [k C A weightPos weightNeg]
  (loop [i 0 pairs {:b [] :s [] :value []}]
    (if (>= i k)
      pairs
      (recur (inc i) (merge-with conj pairs (loopMatrix C A (get pairs :b) (get pairs :s) weightPos weightNeg)))
    )  
  )    
)

(defn assoAlgo
  [C k tau weightPos weightNeg]
  (let [A (constructA C tau) coverPairs (coverPairs k C A weightPos weightNeg) ]
    coverPairs
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; https://doi.org/10.1137/1.9781611972801.15
(defn- createNewD
"Returns new Matrix D from Input Matrix to adjust form already calculated vectors"
  [C Dr]
  (loop [i 0 A []]
    (if (>= i (count C))
      A
      (recur (inc i) (conj A (into [] (map (fn [x y] (if (and (= 1 x) (= 1 y)) 0 y)) (get C i) (get Dr i)))))
    )
  )
)

(defn- sortItems
"Returns sorted list S"
  [cur-D]  
  (loop [i 0 item (get cur-D 0) S (hash-map)]
    (if (>= i (count cur-D))
      S
      (recur (inc i) (get cur-D (inc i)) (assoc S i (cond (nil? (get (frequencies item) 1)) 0  :else (get (frequencies item) 1))))
    )
  )
)

(defn- createNewCt
"Creates new Vector to loop over"
  [Ct Dr]
  (let [Cts []]
    (into Cts
      (for [i (range (count Ct))]
        (if (= 0 (get Dr i)) 0 (get Ct i))
      )
    )
  )
)

(defn- countOne
"Returns count of 1's in Matrix"
  [V]
  (loop [i 0 c 0]
     (if (> i (count V))
      c
      (recur (inc i) (cond (= (get V i) 1) (inc c) :else c)) 
    ) 
  )    
)

(defn- countFalseOnes
"Returns count of how many false 1's are in the Matrix"
  [V D]
  (loop [i 0 j 0 c 0]
    (if (> j (count V))
      c
      (recur (cond (> i (count (get V 0))) 0 :else (inc i)) (cond (> i (count (get V 0))) (inc j) :else j) (cond (not= (get (get V i) j) (get (get D i) j)) (inc c) :else c))
    )
  )  
)

(defn- weightCore
"Weight function to determine if found Core is better, additionally adds list sh to the new core"
  [C CS sh D]
  (let [countOnesCS (+ (+ (countOne (get CS :ci)) (countOne (get CS :ct))) (countFalseOnes (calcOneMatrix (get CS :ci) (get CS :ct)) D)) countOnesC (+(+ (countOne (get C :ci)) (countOne (get C :ct))) (countFalseOnes (calcOneMatrix (get C :ci) (get C :ct)) D))]
    (if (<= countOnesCS countOnesC) CS (update C :e conj sh))
  )
)

(defn- weight
"Weight function to determine if found Core is better"
  [C CS D]
  (let [countOnesCS (+ (+ (countOne (get CS :ci)) (countOne (get CS :ct))) (countFalseOnes (calcOneMatrix (get CS :ci) (get CS :ct)) D)) countOnesC (+(+ (countOne (get C :ci)) (countOne (get C :ct))) (countFalseOnes (calcOneMatrix (get C :ci) (get C :ct)) D))]
    (if (<= countOnesCS countOnesC) CS C)
  )
)

(defn- findCoreLoop
"Loops to find new Cores"
  [S C newD]
  (loop [i 1 CS C CiS (assoc (get CS :ci) (get (nth S i) 0) 1) CtS (createNewCt (get CS :ct) (get newD (get (nth S i) 0)))]
      (if (>= i (count S))
        CS
        (recur (inc i) (weightCore CS {:ci CiS :ct CtS :e (get CS :e)} (get (nth S i) 0) newD) (assoc (get CS :ci) (get (nth S i) 0) 1) (createNewCt (get CS :ct) (get newD (get (nth S i) 0))))      
      )
  )
)

(defn- findCore
"finds a core and returns new D"
  [cur-D]
  (let [S (sort-by val > (sortItems cur-D)) C {:ci (assoc (vec (repeat (count cur-D) 0)) (first (first S)) 1) :ct (get cur-D (first (first S))) :e (list)}]
    (findCoreLoop S C cur-D) 
  )
)

(defn- newTransactions
"Helper function for 'extendCore'"
  [C pi D]
  (loop [i 0 CS C]
    (if (<= (count (get C :ct)) i)
      CS
      (recur (inc i) (weight CS {:ci (get CS :ci) :ct (assoc (get CS :ct) i 0)} D))
    )
  )
)

(defn- extendCore
"ExtendCores with noise"
  [C pi D]
  (let [E (get C :e)]
    (loop [i 0 item (nth E 0) CS C]
      (if (<= (count E) i)
        CS
        (recur (inc i) (nth E i) (newTransactions (weight CS {:ci (assoc (get CS :ci) item 1) :ct (get CS :ct)} D) pi D))
      )  
    )
  )
)

(defn pandaAlgo
"Call with binary Matrix D and how many iterations k"
  [D k]
  (loop [i 0 pi (list) cur-D D]
    (if (<= k i)
      pi
      (recur (inc i) (conj pi (extendCore (findCore cur-D) pi cur-D)) (createNewD (calcOneMatrix (get (extendCore (findCore cur-D) pi cur-D) :ci) (get (extendCore (findCore cur-D) pi cur-D) :ct)) D))
    )
  )
)