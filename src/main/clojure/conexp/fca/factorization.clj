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
          (for [b B] (if (and (= a 1) (= b 1)) 1 0)))))))

(defn- unite
"Unite-Operator for 2 matrices"
  [A B]
  (loop [i 0 C []]
    (if (>= i (count A))
      C
      (recur (inc i)
             (conj C (into [] (map (fn [x y] (if (or (= 1 x) (= 1 y)) 1 0)) (get A i) (get B i))))))))

(defn calcAssoMatrix
"Creates binary Matrix from asso-algo result vectors"
    [map]
    (loop [i 0 end (calcOneMatrix (nth (get map :s) i) (nth (get map :b) i))]
      (if (>= i (count (get map :b)) i)
        (into [] (apply concat end))
        (recur (inc i)
               (unite end (calcOneMatrix (nth (get map :s) i) (nth (get map :b) i)))))))

 (defn calcPandaContext
  "Creates Context from panda-algo result vectors"
  [pi]
  (loop [i 0 end (calcOneMatrix (get (nth pi i) :ci) (get (nth pi i) :ct))]
    (if (>= i (count pi))
      (make-context-from-matrix  (count (get (nth pi 0) :ci)) (count (get (nth pi 0) :ct)) (into [] (flatten end)))
      (recur (inc i) 
             (unite end (calcOneMatrix (get (nth pi i) :ci) (get (nth pi i) :ct)))))))

(defn calcGrecondContext
  "Creates Context from grecond-algo result vectors"
  [F m n]
  (loop [i 1 
         end (grecondMakeMatrixFromConcept (nth (nth F 0) 0) (nth (nth F 0) 1) n m)]
    (if (>= i (count F))
      (make-context-from-matrix m n end)
      (recur
        (inc i)
        (mapv (fn [x y] (if (or (= 1 x) (= 1 y)) 1 0)) (grecondMakeMatrixFromConcept (nth (nth F i) 0) (nth (nth F i) 1) n m) end)))))

(defn calcHyperContext
  "Creates Context from hyper-algo result vectors"
  [hyper]
  (make-context-from-matrix  (count (nth hyper 1)) (count (nth (nth hyper 1) 0)) (into [] (flatten (nth hyper 1))))
)

(defn- greessMakeMatrixFromConcept
  [a b n m]
  ;; make matrix from concept a&b with n m 
  (loop [i 0 j 0 matrix []]
    (if (> j (- m 1))
    matrix
      (recur 
        (cond (>= i (- n 1)) 0 :else (inc i)) 
        (cond (>= i (- n 1)) (inc j) :else j)
        (cond (and (some #(= j %) a) (some #(= i %) b)) (conj matrix 1) :else (conj matrix 0))))))

(defn calcGreessContext
  [F n m]
  (loop [i 0 
         end (greessMakeMatrixFromConcept (nth (nth F 0) 0) (nth (nth F 0) 1) n m)]
    (if (>= i (count F))
      (make-context-from-matrix m n end)
      (recur
        (inc i)
        (mapv (fn [x y] (if (or (= 1 x) (= 1 y)) 1 0)) (greessMakeMatrixFromConcept (nth (nth F i) 0) (nth (nth F i) 1) n m) end)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;https://doi.org/10.1109/TKDE.2008.53

(defn- calcUnionMatrixAsso
  [B S i j]
  (if (empty? (get B 0))
    (calcOneMatrix (vec (repeat i 0)) (vec (repeat j 0)))
    (loop [i 0 end (calcOneMatrix (get B i) (get S i))]
      (if (>= i (count B) i)
        end
        (recur (inc i) (unite end (calcOneMatrix (get B i) (get S i))))))))

(defn- countFalseOnesAsso
  [C D]
  (loop [i 0 j 0 c 0]
    (if (> j (count C))
      c
      (recur (cond (> i (count (get C 0))) 0 :else (inc i))
             (cond (> i (count (get C 0))) (inc j) :else j) 
            (cond (and (= (get (get C i) j) 0) (= (get (get D i) j) 1)) (inc c) :else c)))))

(defn- countOnesAsso
  [C D]
  (loop [i 0 j 0 c 0]
    (if (> j (count C))
      c
      (recur (cond (> i (count (get C 0))) 0 :else (inc i))
             (cond (> i (count (get C 0))) (inc j) :else j)
             (cond (and (= (get (get C i) j) 1) (= (get (get D i) j) 1)) (inc c) :else c)))))

(defn- arbitraryVector
  [n]
  (let [arb (map #(clojure.string/split % #"") (map (partial apply str) (clojure.math.combinatorics/selections [0 1] n)))
        return []]
    (for [x arb]
      (into return (map #(Integer/parseInt %) x)))))

(defn- constructA
  [C tau]
  (into []
    (let [X []]
      (for [ai (apply mapv vector C)] 
        (into X 
          (for [aj (apply mapv vector C)]
            (if (< 0 (reduce + (map * aj aj)))
              (if (< tau ( / (reduce + (map * ai aj)) (reduce + (map * aj aj)))) 1 0) 0)))))))

(defn- cover
  [C newB newS B S weightPos weightNeg]
  (let [uni (unite (calcOneMatrix newB newS) (calcUnionMatrixAsso B S (count newB) (count newS)))]
    {:b newB :s newS :value (conj [] (- (* weightPos (countOnesAsso C uni)) (* weightNeg (countFalseOnesAsso C uni))))}))

(defn- loopMatrix
  [C A B S weightPos weightNeg]
  (let [AT (arbitraryVector (count C))] 
    (loop [i 0 
           j 0 
           bestMatch {:b (get A i) :s (nth AT j) :value (get (cover C (get A 0) (nth AT 0) B S weightPos weightNeg) :value)}]
      (if (> i (count A))
        bestMatch
        (recur 
          (cond (> j (- (count AT) 2)) (inc i) :else i)
          (cond (> j (- (count AT) 2)) 0 :else (inc j))
          (cond (< (get (get bestMatch :value) 0) (get (get (cover C (get A i) (nth AT j) B S weightPos weightNeg) :value) 0))
           {:b (get (cover C (get A i) (nth AT j) B S weightPos weightNeg) :b)
            :s (get (cover C (get A i) (nth AT j) B S weightPos weightNeg) :s)
            :value (get (cover C (get A i) (nth AT j) B S weightPos weightNeg) :value)}
            :else bestMatch))))))

(defn- coverPairs
  [k C A weightPos weightNeg]
  (loop [i 0 pairs {:b [] :s [] :value []}]
    (if (>= i k)
      pairs
      (recur (inc i)
             (merge-with conj pairs (loopMatrix C A (get pairs :b) (get pairs :s) weightPos weightNeg))))))

(defn assoAlgo
  [C k tau weightPos weightNeg]
  (let [A (constructA C tau) coverPairs (coverPairs k C A weightPos weightNeg) ]
    coverPairs))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; https://doi.org/10.1137/1.9781611972801.15;
(defn- createNewD
"Returns new Matrix D from Input Matrix to adjust form already calculated vectors"
  [C Dr]
  (loop [i 0 A []]
    (if (>= i (count C))
      A
      (recur (inc i)
             (conj A (into [] (map (fn [x y] (if (and (= 1 x) (= 1 y)) 0 y)) (get C i) (get Dr i))))))))

(defn- sortItems
"Returns sorted list S"
  [cur-D]  
  (loop [i 0 
         item (get cur-D 0)
         S (hash-map)]
    (if (>= i (count cur-D))
      S
      (recur (inc i)
             (get cur-D (inc i))
             (assoc S i (cond (nil? (get (frequencies item) 1)) 0  :else (get (frequencies item) 1)))))))

(defn- createNewCt
"Creates new Vector to loop over"
  [Ct Dr]
  (let [Cts []]
    (into Cts
      (for [i (range (count Ct))]
        (if (= 0 (get Dr i)) 0 (get Ct i))))))

(defn- countOne
"Returns count of 1's in Matrix"
  [V]
  (loop [i 0 c 0]
     (if (> i (count V))
      c
      (recur (inc i)
             (cond (= (get V i) 1) (inc c) :else c)))))

(defn countFalseOnes
  "Returns count of how many false 1's are in the Matrix"
  [V D]
  (loop [i 0 j 0 c 0]
    (if (>= i (count V))
      c
      (recur (cond (> j (count (get V 0))) (inc i) :else i)
             (cond (> j (count (get V 0))) 0 :else (inc j))
             (cond (not= (get (get V i) j) (get (get D i) j)) (inc c) :else c)))))

(defn- weightCore
"Weight function to determine if found Core is better, additionally adds list sh to the new core"
  [C CS sh D]
  (let [countOnesCS (+ (+ (countOne (get CS :ci)) (countOne (get CS :ct))) (countFalseOnes (calcOneMatrix (get CS :ci) (get CS :ct)) D))
        countOnesC (+(+ (countOne (get C :ci)) (countOne (get C :ct))) (countFalseOnes (calcOneMatrix (get C :ci) (get C :ct)) D))]
    (if (<= countOnesCS countOnesC) CS (update C :e conj sh))))

(defn- weight
"Weight function to determine if found Core is better"
  [C CS D]
  (let [countOnesCS (+ (+ (countOne (get CS :ci)) (countOne (get CS :ct))) (countFalseOnes (calcOneMatrix (get CS :ci) (get CS :ct)) D))
        countOnesC (+(+ (countOne (get C :ci)) (countOne (get C :ct))) (countFalseOnes (calcOneMatrix (get C :ci) (get C :ct)) D))]
    (if (<= countOnesCS countOnesC) CS C)))

(defn- findCoreLoop
"Loops to find new Cores"
  [S C newD]
  (loop [CiS (assoc (get C :ci) (get (nth S 1) 0) 1)
         CtS (createNewCt (get C :ct) (get newD (get (nth S 1) 0)))
         CS C
         i 1]
    (if (>= i (count S))
      CS
      (recur (assoc (get CS :ci) (get (nth S i) 0) 1)
             (createNewCt (get CS :ct) (get newD (get (nth S i) 0)))
             (weightCore CS {:ci (assoc (get CS :ci) (get (nth S i) 0) 1) :ct (createNewCt (get CS :ct) (get newD (get (nth S i) 0))) :e (get CS :e)} (get (nth S i) 0) newD)
             (inc i)))))

(defn- findCore
"finds a core and returns new D"
  [cur-D]
  (let [S (sort-by val > (sortItems cur-D))
        C {:ci (assoc (vec (repeat (count cur-D) 0)) (first (first S)) 1) :ct (get cur-D (first (first S))) :e (list)}]
    (findCoreLoop S C cur-D)))

(defn- newTransactions
"Helper function for 'extendCore'"
  [C pi D]
  (loop [i 0 CS C]
    (if (<= (count (get C :ct)) i)
      CS
      (recur (inc i)
             (weight CS {:ci (get CS :ci) :ct (assoc (get CS :ct) i 0)} D)))))

(defn- extendCore
"ExtendCores with noise"
  [C pi D]
  (let [E (get C :e)]
    (loop [i 0 item (nth E 0) CS C]
      (if (<= (count E) i)
        CS
        (recur (inc i)
               (nth E i)
               (newTransactions (weight CS {:ci (assoc (get CS :ci) item 1) :ct (get CS :ct)} D) pi D))))))

(defn pandaAlgo
"Call with binary Matrix D and how many iterations k"
  [D k]
  (loop [i 0 pi (list) cur-D D]
    (if (<= k i)
      pi
      (recur (inc i)
             (conj pi (extendCore (findCore cur-D) pi cur-D))
             (createNewD (calcOneMatrix (get (extendCore (findCore cur-D) pi cur-D) :ci) (get (extendCore (findCore cur-D) pi cur-D) :ct)) cur-D)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; grecond
;; https://doi.org/10.1016/j.jcss.2009.05.002

(defn grecondMakeMatrixFromConcept
  [a b n m]
  ;; make matrix from concept a&b with n m 
  (loop [i 0 j 0 matrix []]
    (if (> j (- m 1))
    matrix
      (recur 
        (cond (>= i (- n 1)) 0 :else (inc i)) 
        (cond (>= i (- n 1)) (inc j) :else j)
        (cond (and (some #(= j %) a) (some #(= i %) b)) (conj matrix 1) :else (conj matrix 0))))))

(defn- grecondTestFullMatch
  [A U]
  (loop [i 0 c 0 u U]
    (if (>= i (count A))
      {:u u :c c}
      (recur (inc i)
             (cond (= (get A i) (get U i) 1) (inc c) :else c)
             (cond (= (get A i) (get U i) 1) (assoc u i 0) :else u)))))

(defn- grecondTestRemainderMatch
  [A U]
  (loop [i 0 c 0 u U]
    (if (>= i (count A))
      {:u u :c c}
      (recur (inc i)
             (cond (= (get A i) (get U i) 1) (inc c) :else c) 
             (cond (= (get A i) (get U i) 1) (assoc u i 0) :else u)))))

(defn- grecondFullMatch
  [S U n m]
  (loop [i 0 u U f []]
    (if (> i (- (count S) 1))
      {:u u :f f}
      (recur 
        (inc i)
        (cond (= (* (count (nth (nth S i) 0)) (count (nth (nth S i) 1))) (:c (grecondTestFullMatch (grecondMakeMatrixFromConcept (nth (nth S i) 0) (nth (nth S i) 1) n m) u)))
        (:u (grecondTestFullMatch (grecondMakeMatrixFromConcept (nth (nth S i) 0) (nth (nth S i) 1) n m) u)) :else u)
        (cond (= (* (count (nth (nth S i) 0)) (count (nth (nth S i) 1))) (:c (grecondTestFullMatch (grecondMakeMatrixFromConcept (nth (nth S i) 0) (nth (nth S i) 1) n m) u)))
        (conj f (nth S i)) :else f)))))

(defn- grecondRemainderMatch
  [S U F n m k]
  (loop [i 0 u U f F]
    (if (<= k i)
      f
      (recur 
        (inc i)
        (cond (< 0 (:c (grecondTestRemainderMatch (grecondMakeMatrixFromConcept (nth (nth S i) 0) (nth (nth S i) 1) n m) u)))
        (:u (grecondTestRemainderMatch (grecondMakeMatrixFromConcept (nth (nth S i) 0) (nth (nth S i) 1) n m) u)) :else u)
        (cond (< 0 (:c (grecondTestRemainderMatch (grecondMakeMatrixFromConcept (nth (nth S i) 0) (nth (nth S i) 1) n m) u))) (conj f (nth S i)) :else f)))))

(defn- grecondCreateUsable
  [S]
  (loop [i 0 c []]
    (if (> i (- (count S) 1))
      c
      (recur (inc i)
      (cond (not= 0 (* (count (nth (nth S i) 0)) (count (nth (nth S i) 1))))
            (conj c (nth S i)) :else c)))))

(defn grecond
  [I k]
  (let [S (reverse (sort-by (fn [[a b]] (* (count a) (count b))) (grecondCreateUsable (concepts I))))
        U (make-matrix-from-context (context-to-string I))]
    (let [full (grecondFullMatch S (into [] (apply concat U)) (count U) (count (get U 0)))]
      (grecondRemainderMatch (remove (set (:f full)) S) (:u full) (:f full) (count U) (count (get U 0)) k))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; hyper
;; DOI 10.1007/s10618-010-0203-9

(defn- findHyper
  "Finds all Hyperrectangles and calculates their Coverage in relation to the current Coverage of the Input"
  [C Coverage]
  (let [ret ()]
    (for [c C] (conj ret {:cover (vec (for [x (mapv Coverage (sort (vec (get c 0))))]
      (mapv x (sort (vec (get c 1)))))) :g (vec (sort (vec (get c 0)))) :m (vec (sort (vec (get c 1))))}))))

(defn- calcCoverVector
  "Calculates a vector with the current Coverage"
  [v]
  (loop [i 0 j 0 c 0]
    (if (>= i (count v))
      c
      (recur 
        (cond (> j (count (first (nth v i)))) (inc i) :else i)
        (cond (> j (count (first (nth v i)))) 0 :else (inc j))
        (cond (= (get (first (nth v i)) j) 0) (inc c) :else c)))))

(defn- calcCost
  "Calcultes the Cost of an Hyperrectangle"
  [hyper]
  (let [v ()]
    (let [sortV (sort-by first (for [x (range (count (get (first hyper) :g)))] (conj v (get (get (first hyper) :g) x) (get (get (first hyper) :cover ) x))))]
      (loop [i 1 
             out (try (/ (+ (count (first (first sortV))) 1) (calcCoverVector (conj () (first sortV))))
          (catch ArithmeticException e Integer/MAX_VALUE))
         oldOut Integer/MAX_VALUE g (conj () (nth (first sortV) 1)) oldg ()]
        (if (or (> out oldOut) (>= i (count sortV)))
          (if (> out oldOut)
            {:cost oldOut :g oldg :m (get (first hyper) :m)}
            {:cost out :g g :m (get (first hyper) :m)})
          (recur 
            (inc i)
            (try (/ (+ (count (first (first sortV))) (inc i)) (calcCoverVector (take (inc i) sortV))) (catch ArithmeticException e Integer/MAX_VALUE))
            out
            (conj g (nth (nth sortV i) 1))
            g))))))

(defn- calcCostListHyper
  "Puts together a List of all Hyperrectangles and their Cost"
  [C Coverage]
  (let [hyperList (findHyper C Coverage) costList ()]
    (for [h hyperList] (conj costList (calcCost h)))))

(defn- calcNewCoverageHyper
  "Calculates the new Coverage in relation to the new Found Output Hyperrectangle"
  [C Coverage]
  (let [X (apply min (map :cost (into [] (flatten (calcCostListHyper C Coverage)))))
        Y (into [] (flatten (calcCostListHyper C Coverage)))]
    (loop [i 0]
      (if (= (:cost (nth Y i)) X)
        {:m (:m (nth Y i)) :g (:g (nth Y i))}
        (recur (inc i))))))

(defn- createVector
  "Helper Function to Create a Vector from a Concept"
  [v n]
  (loop [i 0 x [] c 0]
    (if (>= i n)
      x
      (recur 
        (inc i)
        (cond (and  (< c (count v)) (= i (nth v c))) (conj x 1) :else (conj x 0))
        (cond (and (< c (count v)) (= i (nth v c)) ) (inc c) :else c)))))

(defn- calcNewCoverageMatrixHyper
  "Calculates the new Coverage Matrix in relation to the new Found Output Hyperrectangle"
  [new Coverage n m]
  (unite Coverage (calcOneMatrix (createVector (into [] (sort (:g new))) m) (createVector (into [] (sort (:m new))) n))))

(defn hyper
  "Hyper Algorithm"
  [Input k]
  (let [n (count(nth (make-matrix-from-context (context-to-string Input))0))
        m (count (make-matrix-from-context (context-to-string Input)))]
    (loop [Output []
           Coverage (calcOneMatrix (vec (repeat (count (objects Input)) 0)) (vec (repeat(count(attributes Input)) 0))) 
           i 0 
           C (grecondCreateUsable (concepts Input))]
        (if (>= i k)
        [Output Coverage]
        (recur (conj Output (calcNewCoverageHyper C Coverage))
               (calcNewCoverageMatrixHyper (calcNewCoverageHyper C Coverage) Coverage n m)
               (inc i) 
               C )))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; greess
;; https://doi.org/10.48550/arXiv.1306.4905

(defn- cartesianProduct
  [a b]
  (for [x a y b] [x y]))

(defn- remove-indexed
  [v n]
  (into (subvec v 0 n) (subvec v (inc n))))

(defn- overlap
  [concepts mark]
  (for [x concepts] (count (intersection (set (cartesianProduct (first x) (second x))) (set mark)))))

(defn- overlapMark
  [concept mark]
  (intersection (set (cartesianProduct (first concept) (second concept)))  (set mark) ))

(defn- loopOverlaps
  "Loops over concepts to determine which should be considered a Factorization Candidate"
  [concepts mark k]
  (loop [out [] tempConcepts concepts tempMark mark i 0]
    (if (>= i k)
      out
      (recur
        (conj out (nth tempConcepts (.indexOf (overlap tempConcepts tempMark) (apply max(overlap tempConcepts tempMark)))))
        (remove-indexed tempConcepts (.indexOf (overlap tempConcepts tempMark) (apply max(overlap tempConcepts tempMark))))
        (apply disj (set tempMark) (overlapMark (nth tempConcepts (.indexOf (overlap tempConcepts tempMark) (apply max(overlap tempConcepts tempMark)))) tempMark))
        (inc i)))))

(defn- testMark
  "Tests wether Incidence should be marked"
  [i j v greek]
  (let [ testGreekGamma (concat (subvec (first greek) 0 i) (subvec (first greek) (inc i)))
         testGreekDelta (concat (subvec (second greek) 0 j) (subvec (second greek) (inc j)))
         gamma (nth (first greek) i)
         delta (nth (second greek) j)
        ]
      (if (and (= 1 (nth (nth v j) i)) (nil?
      (some #(= 1 %) (concat 
        (for [g testGreekGamma] (if (and (subset? g gamma) (= 1 (nth (nth v j) (.indexOf (first greek) g)))) 1 0))
        (for [d testGreekDelta] (if (and (subset? d delta) (= 1 (nth (nth v (.indexOf (second greek) d)) i))) 1 0))
      )))) [j i] nil)))

(defn- markGreess
  "Creates List of Marked Incidences"
  [v greek]
  (loop [marked [] j 0 i 0]
    (if (> (inc j) (count v))
      marked
      (recur 
        (conj marked (testMark i j v greek))
        (cond (>= (inc i) (count (nth v 0))) (inc j) :else j)
        (cond (>= (inc i) (count (nth v 0))) 0 :else (inc i))))))

(defn- calcDerivation
  "Creates derivation for later use in marking progress"
  [Input x y]
  (loop [gamma [] i 0]
    (if (>= i (count (x Input)))
      gamma
      (recur (conj gamma (y Input #{i})) (inc i)))))

(defn greess
  [Input k]
  (let [gamma (calcDerivation Input attributes attribute-derivation)
        mu (calcDerivation Input objects object-derivation)]
    (loopOverlaps (grecondCreateUsable (concepts Input)) (remove nil? (markGreess (make-matrix-from-context (context-to-string Input)) [gamma mu])) k)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; evaluation

(defn hamming_Distance
  [A B]
  (count (filter #{1} (map (fn [a b] (if (not= a b) 1 0)) A B))))

(defn froebeniusnorm
  [A]
  (Math/sqrt (count (filter #{1} A))))

(defn- helper
  [m1 M2]
    (for [m2 M2]
      (double (/ (count (clojure.set/intersection (set (:g m1)) (set (:g m2)))) (count (clojure.set/union (set (:g m1)) (set (:g m2))))))))

(defn importFile [file] 
  (let [raw (rest
             (clojure.string/split (slurp file)
                                   #"\n"))]
    (map
     (fn [line]
       (let [[g c] (clojure.string/split line #"\;")]
         {:g (map read-string (clojure.string/split g #","))
          :c (map read-string (clojure.string/split c #","))}))
     raw)))

(defn biClusterMatch
  [M1 M2]
  (loop [result (list) i 0]
    (if (>= i (count M1))
      (* (/ 1 (count M1)) (reduce + result))
      (recur 
        (conj result (apply max (helper (nth M1 i) M2)))
        (inc i)))))