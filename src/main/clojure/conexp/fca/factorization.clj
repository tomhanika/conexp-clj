(ns conexp.fca.factorization
    (:require [conexp.base :refer :all]
              [conexp.fca.fast :refer [to-binary-matrix]]
              [conexp.fca.contexts :refer :all])
    (:gen-class))

;;; helper functions
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
    (loop [i 0 end (calcOneMatrix (nth (get map :b) i) (nth (get map :s) i))]
      (if (>= i (count map) i)
        end
        (recur (inc i) (unite end (calcOneMatrix (nth (get map :b) i) (nth (get map :s) i))))
      )
    )
)

(defn calcPandaMatrix
 "Creates binary Matrix from panda-algo result vectors"
  [pi]
  (loop [i 0 end (calcOneMatrix (get (nth pi i) :ci) (get (nth pi i) :ct))]
    (if (>= i (count pi) i)
      end
      (recur (inc i) (unite end (calcOneMatrix (get (nth pi i) :ci) (get (nth pi i) :ct))))
    )
  )
)

;;; asso
(defn- constructA
"Constructs Association-Matrix"
  [C tau]
  (into []
    (let [X []]
      (for [ai (apply mapv vector C)] 
        (into X 
          (for [aj (apply mapv vector C)]
            (if (< tau ( / (reduce + (map * ai aj)) (reduce + (map * aj aj)))) 1 0) 
          )
        )
      )
    )  
  )
)

(defn- calcCover
"Calculates coverage"
  [a b c weightPos weightNeg]
  (if (and (and (= b 1) (= c 1)) (= a 1))
    weightPos
    (if (and (and (= b 1) (= c 1)) (= a 0)) weightNeg 0)
  )
)

(defn- loopX
"Helper function for cover"
  [xi b X aj indexi weightPos weightNeg]
  (loop [indexj 0 list []]
    (if (<= (count (get X 0) ) indexj)
      list
      (recur (inc indexj) (conj list (calcCover (get (get X indexi) indexj) b (get aj indexj) weightPos weightNeg)))
    )
  )
)

(defn- cover
"Calculates the coverage of the current vector for the input Matrix"
  [ai aj X weightPos weightNeg]
  (loop [indexi 0 list []]  
    (if (<= (count X) indexi) 
      list
      (recur (inc indexi) (into list (loopX (get X indexi) (get ai indexi) X aj indexi weightPos weightNeg)))
    )
  )  
)

(defn- createOut
"Creates Output Vectors"
  [C A weightPos weightNeg]
  (let [AT (apply mapv vector A)]
    (reduce (fn [d [ai aj :as index]] 
            (assoc d index (reduce + (cover (get A ai) (get AT aj) C weightPos weightNeg))))
          {}
          (cross-product (range (count A)) (range (count AT)))
          )))

(defn- topk
"Returns the best candidates for the factorization"
  [k C A weightPos weightNeg]
  (loop [i 0 returnList () maxMap (createOut C A weightPos weightNeg) cur-A A]    
    (let [[row col] (key (apply max-key val maxMap)) deletedRow (assoc cur-A row (vec (repeat (count (get cur-A row)) 0))) deletedCol (vec (map #(assoc %1 col 0) deletedRow))] 
      (if (<= k i)
        returnList
        (recur (inc i) (conj returnList (key (apply max-key val maxMap))) (createOut C deletedCol weightPos weightNeg) deletedCol)
      )
    )
  )
)

(defn- assoAlgoB
"Returns Matrix B for the factorization"
  [topk A]
  (for [list topk]
    (get A (get list 0))
  )
)

(defn- assoAlgoS
"Returns Matrix S for the factorization"
  [topk A]
  (for [list topk]
    (get (apply mapv vector A) (get list 1))
  )
)

(defn assoAlgo
"Unites Matrix B and Matrix S to return the factorization of C into one Map"
  [C k tau weightPos weightNeg]
  (let [A (constructA C tau) topK (topk k C A weightPos weightNeg) Out {:b (assoAlgoB topK A) :s (assoAlgoS topK A)}]
    Out
  )
)

;;; panda
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

(defn panda
"Call with binary Matrix D and how many iterations k"
  [D k]
  (loop [i 0 pi (list) cur-D D]
    (if (<= k i)
      pi
      (recur (inc i) (conj pi (extendCore (findCore cur-D) pi cur-D)) (createNewD (calcOneMatrix (get (extendCore (findCore cur-D) pi cur-D) :ci) (get (extendCore (findCore cur-D) pi cur-D) :ct)) D))
    )
  )
)