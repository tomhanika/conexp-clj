(ns conexp.fca.causal-implications
  "Causal Implications for Formal Concept Analysis."
  (:require
   [conexp.base :refer :all]
   [conexp.io.contexts :refer :all]
   [conexp.io.fcas :refer :all]
   [conexp.fca.contexts :refer :all]
   [clojure.set :as set]))


(defn supp [ctx itemset]
  ;itemset needs to consist of two entries a and b, both sets of attributes
  ;computes the support of the itemset with all attributes in a and the negation of each attribute in b
  (let [[attributes neg-attributes] itemset, objects (objects ctx), incidence (incidence-relation ctx)]
    (max (count (filter identity (for [object objects]
                                 (every? true? (concat
                                                (for [attribute attributes]
                                                  (some? (incident? ctx object attribute)))
                                                (for [attribute neg-attributes]
                                                  (not (some? (incident? ctx object attribute)))))))))

         1    
)))

(defn conf [ctx rule]
  (let [[premise conclusion] rule]
    (/ (supp ctx [(set/union premise conclusion) #{}]) 
       (supp ctx [premise #{}]))))

(defn odds-ratio [ctx rule]
  (let [[premise conclusion] rule]
    (/ (* (supp ctx [(set/union premise conclusion) #{}]) (supp ctx [#{} (set/union premise conclusion)]))
       (* (supp ctx [premise conclusion]) (supp ctx [conclusion premise]))
       
       )))

(defn lsupp [ctx rule] 
  (let [[premise conclusion] rule]
    (/ (supp ctx [(set/union premise conclusion) #{}])
       (supp ctx [conclusion #{}]))))


(defn matched-record-pair? [ctx rule controlled-variables a b]
;returns true, if objects a and b form a matched record pair, false otherwise
  (let [[premise conclusion] rule,
        a-attributes (object-derivation ctx [a]), 
        b-attributes (object-derivation ctx [b])]

;check whether premise is present in exactly one of the objects
    (and (or (and (subset? premise a-attributes) (not (subset? premise b-attributes)))
             (and (subset? premise b-attributes) (not (subset? premise a-attributes))))
;check whether controlled variables have same realizations in both objects
         (subset? controlled-variables
                  (set/union (set/intersection a-attributes b-attributes)
                             (set/intersection (set/difference controlled-variables a-attributes) 
                                               (set/difference controlled-variables b-attributes))))))
)

(defn find-matched-record-pair [ctx rule controlled-variables objs-considered a]
;returns set containing a and an object it forms a matched record pair with, the empty set if none exists
;only objects in the collection objs will be considered
  (let [objs (into [] objs-considered)]
  (if (= (count objs) 0)
           #{}
           (if (matched-record-pair? ctx rule controlled-variables a (first objs)) 
                 #{a (first objs)}
                 (find-matched-record-pair ctx rule controlled-variables (rest objs) a)))

  ))


(defn fair-data-set [ctx rule controlled-variables]
;computes the fair data set of ctx by finding matched record pairs among the objects
;each object may only be matched with exactly one other object
  (let [objs (objects ctx)]
    (filter seq 
            (reduce (fn[present-objs new-obj]
              (if (contains? (reduce set/union present-objs) new-obj)
                present-objs
                (conj present-objs (find-matched-record-pair 
                                           ctx 
                                           rule 
                                           controlled-variables 
                                           (set/difference objs (reduce set/union present-objs)) 
                                           new-obj))))
              #{} objs))
      ))

(defn fair-odds-ratio [ctx rule fair-data]
;computes the odds ratio of a rule on its fair data set
  (let [premise-attr (first (first rule)), conclusion-attr (first (first (rest rule)))]

    (/ (reduce +  (for [pair fair-data]
      (let [a  (first pair), b (first (rest pair))]
        (if (or (and (incident? ctx a premise-attr) 
                     (and (incident? ctx a conclusion-attr) 
                          (not (incident? ctx b conclusion-attr))))
                (and (incident? ctx b premise-attr) 
                     (and (incident? ctx b conclusion-attr) 
                          (not (incident? ctx a conclusion-attr)))))
        1
        0))))

   (max (reduce +  (for [pair fair-data]
      (let [a  (first pair), b (first (rest pair))]
        (if (or (and (incident? ctx a premise-attr) 
                     (and (not (incident? ctx a conclusion-attr)) 
                          (incident? ctx b conclusion-attr)))
                (and (incident? ctx b premise-attr) 
                     (and (not (incident? ctx b conclusion-attr)) 
                          (incident? ctx a conclusion-attr))))
        1
        0))))
        
        1)
    )))


(defn confidence-interval [ctx rule odds-ratio zconfidence]
  (let [[premise conclusion] rule]
    [(Math/exp (+ (Math/log odds-ratio)
                  (* zconfidence (Math/sqrt (+ (/ 1 (supp ctx [(set/union premise conclusion) #{}]))
                                               (/ 1 (supp ctx [premise conclusion]))
                                               (/ 1 (supp ctx [conclusion premise])) 
                                               (/ 1 (supp ctx [#{} (set/union premise conclusion)])))))))
     (Math/exp (- (Math/log odds-ratio)
                  (* zconfidence (Math/sqrt (+ (/ 1 (supp ctx [(set/union premise conclusion) #{}]))
                                               (/ 1 (supp ctx [premise conclusion]))
                                               (/ 1 (supp ctx [conclusion premise])) 
                                               (/ 1 (supp ctx [#{} (set/union premise conclusion)])))))))] 
))

(defn fair-confidence-interval [ctx rule fair-odds-ratio fair-data zconf]
  (let [premise (first (first rule)), conclusion (first (first (rest rule)))]
    [(Math/exp (+ (Math/log fair-odds-ratio)
                 (* zconf (Math/sqrt (+ (/ 1 (max (reduce + 
                                                    (for [pair fair-data]
                                                    (let [a  (first pair), b (first (rest pair))]
                                                    (if (or (and (incident? ctx a premise) 
                                                                 (and (incident? ctx a conclusion) 
                                                                      (not (incident? ctx b conclusion))))
                                                            (and (incident? ctx b premise) 
                                                                 (and (incident? ctx b conclusion) 
                                                                      (not (incident? ctx a conclusion)))))
                                                     1
                                                     0))))
                                                  
                                                  1))
                                        (/ 1 (max 
                                             (reduce +  (for [pair fair-data]
                                             (let [a  (first pair), b (first (rest pair))]
                                             (if (or (and (incident? ctx a premise) 
                                             (and (not (incident? ctx a conclusion)) 
                                             (incident? ctx b conclusion)))
                                             (and (incident? ctx b premise) 
                                             (and (not (incident? ctx b conclusion)) 
                                             (incident? ctx a conclusion))))
                                              1
                                              0))))
                                              1))

                                        )
                                     
                                     ))))
     (Math/exp (- (Math/log fair-odds-ratio)
                 (* zconf (Math/sqrt (+ (/ 1 (max (reduce + 
                                                    (for [pair fair-data]
                                                    (let [a  (first pair), b (first (rest pair))]
                                                    (if (or (and (incident? ctx a premise) 
                                                                 (and (incident? ctx a conclusion) 
                                                                      (not (incident? ctx b conclusion))))
                                                            (and (incident? ctx b premise) 
                                                                 (and (incident? ctx b conclusion) 
                                                                      (not (incident? ctx a conclusion)))))
                                              1
                                              0))))

                                                  1))
                                        (/ 1 (max 
                                             (reduce +  (for [pair fair-data]
                                             (let [a  (first pair), b (first (rest pair))]
                                             (if (or (and (incident? ctx a premise) 
                                             (and (not (incident? ctx a conclusion)) 
                                             (incident? ctx b conclusion)))
                                             (and (incident? ctx b premise) 
                                             (and (not (incident? ctx b conclusion)) 
                                             (incident? ctx a conclusion))))
                                              1
                                              0))))
                                              1))

                                        )
                                     
                                     ))))]
    
))


(defn relevant? [ctx variable response-variable zconfidence]
  (let [rule [#{variable} #{response-variable}]]
    (> (last (confidence-interval ctx rule (odds-ratio ctx rule) zconfidence))
       1))
)

(defn irrelevant-variables [ctx variables response-variable zconfidence]
 ;returns a set of the variables that are irrelevant in respect to the response variable
  (set (filter (fn [x] (not (relevant? ctx x response-variable zconfidence))) variables))
  
)

(defn exclusive-variables [ctx item-set thresh] 
;returns sets of variables that are exclusive to those in the item set
  (set 
    (filter some? 
      (for [a item-set, b (attributes ctx)]
        (if (not (= a b))
          (if (or (<= (supp ctx [#{a b} #{}]) thresh) (<= (supp ctx [#{b} #{a}]) thresh))
            #{a b})))))
  )

(defn causal? [ctx rule irrelevant-vars zconf thresh]
  (let [[premise conclusion] rule
        E (reduce set/union (exclusive-variables ctx premise thresh)) 
        controlled-variables (set/difference (attributes ctx)  
                                             (set/union conclusion irrelevant-vars E premise))
        fair-data (fair-data-set ctx rule controlled-variables)
        fair-odds (fair-odds-ratio ctx rule fair-data)]


    (< 1 (last (fair-confidence-interval ctx rule fair-odds fair-data zconf)) )    
))

(defn generate-causal-rules [ctx premises response-var irrelevant-vars zconf thresh]
  (filter (fn [x] (causal? ctx [x #{response-var}] irrelevant-vars zconf thresh)) premises)
  )

(defn find-redundant [ctx current-item-sets new-item-sets response-var]
  (set (for [new new-item-sets, old current-item-sets]
    (if (and (subset? old new) 
             (= (lsupp ctx [new #{response-var}]) 
                (lsupp ctx [old #{response-var}])))
     (println new old)
))))

;discovers causes of the response variable
(defn causal-association-rule-discovery 
  ([ctx min-lsupp max-length response-var zconf]
   ;initial setup
   (let [frequent-vars (set (filter #(> (lsupp ctx [#{%} #{response-var}]) min-lsupp) (attributes ctx))) 
         ivars (irrelevant-variables ctx frequent-vars response-var zconf)]

      (causal-association-rule-discovery 
         ctx ;context
         #{} ;current causal rules
         (set/difference frequent-vars #{response-var})  ;frequent single variables
         (for [x (set/difference frequent-vars #{response-var})] #{x}) ;itemsets of the current iteration
         ivars ;irrelevant variables in respect to response-var
         min-lsupp ;minimum local support
         0 ;counter, counts up to max-length
         max-length ;maximum length of rules
         response-var ;response variable
         zconf ;confidence for significance test (1.7)
         )) 
 ) 
  
  ([ctx rule-set variables current ivars min-lsupp counter max-length response-var zconf]

    (if (= counter max-length)
      rule-set
      (let [new-causal-rules (generate-causal-rules ctx current response-var ivars zconf 1)
            new-item-sets (set (filter (fn [x] (= (count x) (+ counter 2))) (for [c current, i variables] (conj c i))))]

   (println (find-redundant ctx current new-item-sets response-var))

      (causal-association-rule-discovery 
         ctx
         (set/union rule-set new-causal-rules)
         variables
         (set/difference (filter #(> (lsupp ctx [#{%} #{response-var}]) min-lsupp) new-item-sets) 
                         (find-redundant ctx current new-item-sets response-var));filter item sets
         ivars
         min-lsupp
         (inc counter)
         max-length
         response-var
         zconf)
))))




(def o [0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20
        21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40])
(def a ["smoking" "male" "female" "education-level-high" "education-level-low" "cancer"])
(def i #{[0 "smoking"] [0 "male"] [0 "education-level-high"] [0 "cancer"]
         [1 "smoking"] [1 "male"] [1 "education-level-high"] [1 "cancer"]
         [2 "smoking"] [2 "male"] [2 "education-level-high"] [2 "cancer"]
         [3 "smoking"] [3 "male"] [3 "education-level-high"] [3 "cancer"]
         [4 "smoking"] [4 "male"] [4 "education-level-high"] [4 "cancer"]
         [5 "smoking"] [5 "male"] [5 "education-level-high"] [5 "cancer"]
         [6 "smoking"] [6 "male"] [6 "education-level-high"]
         [7 "smoking"] [7 "male"] [7 "education-level-high"]
         [8 "smoking"] [8 "male"] [8 "education-level-low"] [8 "cancer"]
         [9 "smoking"] [9 "male"] [9 "education-level-low"] [9 "cancer"]
         [10 "smoking"] [10 "male"] [10 "education-level-low"] [10 "cancer"]
         [11 "smoking"] [11 "male"] [11 "education-level-low"] [11 "cancer"]
         [12 "smoking"] [12 "male"] [12 "education-level-low"] 
         [13 "smoking"] [13 "female"] [13 "education-level-high"] [13 "cancer"]
         [14 "smoking"] [14 "female"] [14 "education-level-high"] [14 "cancer"]
         [15 "smoking"] [15 "female"] [15 "education-level-high"] [15 "cancer"]
         [16 "smoking"] [16 "female"] [16 "education-level-high"] [16 "cancer"]
         [17 "smoking"] [17 "female"] [17 "education-level-high"] [17 "cancer"]
         [18 "smoking"] [18 "female"] [18 "education-level-high"] 
         [19 "smoking"] [19 "female"] [19 "education-level-high"]
         [20 "smoking"] [20 "female"] [20 "education-level-low"] [20 "cancer"]
         [21 "smoking"] [21 "female"] [21 "education-level-low"] [21 "cancer"]
         [22 "smoking"] [22 "female"] [22 "education-level-low"] [22 "cancer"]
         [23 "smoking"] [23 "female"] [23 "education-level-low"] [23 "cancer"]
         [24 "smoking"] [24 "female"] [24 "education-level-low"] 
         [25 "male"] [25 "education-level-high"] [25 "cancer"]
         [26 "male"] [26 "education-level-high"] [26 "cancer"]
         [27 "male"] [27 "education-level-high"] 
         [28 "male"] [28 "education-level-high"]
         [29 "male"] [29 "education-level-high"]
         [30 "male"] [30 "education-level-low"] [30 "cancer"]
         [31 "male"] [31 "education-level-low"] 
         [32 "male"] [32 "education-level-low"] 
         [33 "male"] [33 "education-level-low"]
         [34 "female"] [34 "education-level-high"] [34 "cancer"]
         [35 "female"] [35 "education-level-high"] 
         [36 "female"] [36 "education-level-high"]
         [37 "female"] [37 "education-level-low"] [37 "cancer"]
         [38 "female"] [38 "education-level-low"]
         [39 "female"] [39 "education-level-low"]
         [40 "female"] [40 "education-level-low"]})

(def ctx (make-context o a i))

(def premise #{"smoking"})
(def conclusion #{"cancer"})
(def smoking-rule [premise conclusion])

(def fds (fair-data-set ctx [#{"smoking"} #{"cancer"}] #{"male" "female" "education-level-high" "education-level-low"}))

(def smoking-odds-ratio (odds-ratio ctx smoking-rule))
(def ivars-smoking (irrelevant-variables ctx a "cancer" 1.7))
(def fair-smoking-odds-ratio (fair-odds-ratio ctx smoking-rule fds))

(def birds (read-context "testing-data/Bird-Diet.ctx"))

(def diagnosis (read-context "testing-data/Diagnosis.ctx"))


(causal-association-rule-discovery ctx 0.7 3 "cancer" 1.7)

(causal-association-rule-discovery diagnosis 0.7 3 "[Urine pushing yes]" 1.7)

