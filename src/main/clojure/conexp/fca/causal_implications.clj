(ns conexp.fca.causal-implications
  "Causal Implications for Formal Concept Analysis."
  (:require
   [conexp.base :refer :all]
   [conexp.io.contexts :refer :all]
   [conexp.io.fcas :refer :all]
   [conexp.fca.contexts :refer :all]
   [clojure.set :as set]))

;!!!counts the total occurences of an itemset in the context!!!
(defn asupp [ctx itemset]
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
  ;computes the confidence using the asupp method
  (let [[premise conclusion] rule]
    (/ (asupp ctx [(set/union premise conclusion) #{}]) 
       (asupp ctx [premise #{}]))))

(defn odds-ratio [ctx rule]
  ;computes the odds ratio of a rule using the asupp method
  (let [[premise conclusion] rule]
    (/ (* (asupp ctx [(set/union premise conclusion) #{}]) (supp ctx [#{} (set/union premise conclusion)]))
       (* (asupp ctx [premise conclusion]) (asupp ctx [conclusion premise]))
       )))

(defn lsupp [ctx rule] 
  ;computes the local support of a rule
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
;only objects in the collection objs-considered will be considered
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
  (let [premise-attr (first (first rule)), conclusion-attr (first (last rule))]

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


(defn confidence-interval [ctx rule odds-ratio zconf]
  ;computes the confidence interval where odds-ratio is the regular odds-ratio of the rule and 
  ;zconf corresponds to the confidence (1.7 = 70% confidence)
  (let [[premise conclusion] rule]
    [(Math/exp (+ (Math/log odds-ratio)
                  (* zconf (Math/sqrt (+ (/ 1 (supp ctx [(set/union premise conclusion) #{}]))
                                               (/ 1 (supp ctx [premise conclusion]))
                                               (/ 1 (supp ctx [conclusion premise])) 
                                               (/ 1 (supp ctx [#{} (set/union premise conclusion)])))))))
     (Math/exp (- (Math/log odds-ratio)
                  (* zconf (Math/sqrt (+ (/ 1 (supp ctx [(set/union premise conclusion) #{}]))
                                               (/ 1 (supp ctx [premise conclusion]))
                                               (/ 1 (supp ctx [conclusion premise])) 
                                               (/ 1 (supp ctx [#{} (set/union premise conclusion)])))))))] 
))

(defn fair-confidence-interval [ctx rule fair-odds-ratio fair-data zconf]
  ;computes the conficdence interval of a rule on its fair data set
  ;where odds-ratio is the regular odds-ratio of the rule on its fair data set and 
  ;zconf corresponds to the confidence (1.7 = 70% confidence)
  (let [premise (first (first rule)), conclusion (first (last rule))]
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

                                        )))))
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
  ;computes whether a variable is relevant in respect to a response variable
  ;by coputing whether it is associated with the response variable
  (let [rule [#{variable} #{response-variable}]]
    (> (last (confidence-interval ctx rule (odds-ratio ctx rule) zconfidence))
       1))
)

(defn irrelevant-variables [ctx variables response-variable zconfidence]
 ;returns a set of the variables that are irrelevant in respect to the response variable
  (set (filter #(not (relevant? ctx % response-variable zconfidence)) variables))
  
)

(defn exclusive-variables [ctx item-set thresh] 
  ;returns sets of variables that are exclusive to those in the item set
  ;thresh is the maximum number of objects two variables are allowed to cooccurr in while still being considered exclusive
  (set 
    (filter some? 
      (for [a item-set, b (attributes ctx)]
        (if (not (= a b))
          (if (or (<= (supp ctx [#{a b} #{}]) thresh) (<= (supp ctx [#{b} #{a}]) thresh))
            #{a b})))))
  )

(defn causal? [ctx rule irrelevant-vars zconf thresh]
  ;computes whether a rule is causal 
  (let [[premise conclusion] rule
        E (reduce set/union (exclusive-variables ctx premise thresh)) 
        controlled-variables (set/difference (attributes ctx)  
                                             (set/union conclusion irrelevant-vars E premise))
        fair-data (fair-data-set ctx rule controlled-variables)
        fair-odds (fair-odds-ratio ctx rule fair-data)]


    (< 1 (last (fair-confidence-interval ctx rule fair-odds fair-data zconf)) )    
))

(defn generate-causal-rules [ctx premises response-var irrelevant-vars zconf thresh]
  ;generates all causal rules from a set of premises with the response-var as the conclusion
  (filter #(causal? ctx [% #{response-var}] irrelevant-vars zconf thresh) premises)
  )

(defn find-redundant [ctx current-item-sets new-item-sets response-var]
  ;computes redundant rules by comparing the support of the premise to that of its subsets
  ;if they have the same support, they cover the same objects, and the more specific rule redundant
  (set (for [new new-item-sets, old current-item-sets]
    (if (and (subset? old new) 
             (= (lsupp ctx [new #{response-var}]) 
                (lsupp ctx [old #{response-var}])))
     new  
))))

;discovers all causes of the response variable
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
            new-item-sets (set (filter #(= (count %) (+ counter 2)) (for [c current, i variables] (conj c i))))]

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


