(ns conexp.fca.incomplete-contexts.experts
  (:require [clojure.set :refer [subset? intersection difference union]]
            [conexp.fca.implications :refer :all]
            [conexp.base :refer [with-str-out cross-product hash-combine-hash defalias]]
            [conexp.fca.incomplete-contexts.incomplete-contexts :refer :all]
            [conexp.fca.incomplete-contexts.incomplete-contexts-exploration :refer :all]))



;;;;;;;; Define Expert and build a handler
(defprotocol Expert-Knowledge
  (known-implications [this])
  (known-examples [this]))

(defprotocol Expert-Interactions
  (valid-implication? [this implication])
  (counterexample [this implication])
  (unknown-for? [this implication]))

(defprotocol Expert-Answer
  (holds-in-view? [this implication])
  (holds-in-view?-inc-counter [this implication])
  (extended-holds-in-view? [this implication])
  (extended-holds-in-view?-inc-counter [this implication])
  (interaction-counter [this])
  (inc-interaction-counter [this])
  (reset-interaction-counter [this])
  )

(declare find-counterexample)
(declare find-counterexamples)
(defrecord Expert [known-implications known-examples name]
  Object
  #_(toString [this] (str "" name))
  (toString [this] (str "E_" name))

  ;; TODO: implement equality test (test implications and cxt)
  
  Expert-Knowledge
  (known-implications [expert] known-implications)
  (known-examples [expert] known-examples)

  ;;; This is used for the expert handlers and used to fit the answers to the attribute exploration with incomplete information 
  Expert-Interactions
  (valid-implication? [expert implication] (cond
                                             ;(follows? implication known-implications)
                                             ((:follows-fn expert) implication)
                                             :true
                                             true
                                             (let [counterexamples (find-counterexamples known-examples
                                                                                         implication)]
                                               (if (not-empty counterexamples)
                                                 :false
                                                 :unknown))))
  (counterexample [expert implication] (first (find-counterexamples known-examples implication)))
  (unknown-for? [expert implication] (let [pre (premise implication)
                                           con (conclusion implication)
                                           ;follows-from-pre (close-under-implications known-implications pre)
                                           follows-from-pre ((:implications-clop expert) pre)
                                           ]
                                       (difference (into #{} con) (into #{} follows-from-pre))))

  ;;; This is used for the exploration of shared implications and for the phiRB strategy
  Expert-Answer
  (holds-in-view? [expert implication]
    (let [M (attributes known-examples)
          prem (premise implication)
          concl (conclusion implication)
          ;follows-from-implications (follows? implication known-implications)
          follows-from-implications ((:follows-fn expert) implication)
          satisfiable-in-context (satisfiable? implication known-examples)
          answer (if follows-from-implications :yes (if satisfiable-in-context :unknown :no))]
      (cond
        (= answer :yes)
        {:follows known-true
         :counterexample (make-incomplete-context #{} M #{})}
        (= answer :no)
        {:follows known-false
         :counterexample (find-counterexample known-examples implication)}
        (= answer :unknown)
        {:follows unknown
         :counterexample  (make-single-object-incomplete-context (str "g_{" prem "-/->" concl "}")
                                                                 M
                                                                 prem
                                                                 concl)}
        )))
  
  (holds-in-view?-inc-counter [expert implication]
    (inc-interaction-counter expert)
    (holds-in-view? expert implication))
  
  (extended-holds-in-view? [expert implication]
    (let [M (attributes known-examples)
          prem (premise implication)
          concl (conclusion implication)
          ;follows-from-implications (follows? implication known-implications)
          follows-from-implications ((:follows-fn expert) implication)
          satisfiable-in-context (satisfiable? implication known-examples)
          answer (if follows-from-implications :yes (if satisfiable-in-context :unknown :no))
          ;attributes-that-follow (close-under-implications known-implications prem)
          attributes-that-follow ((:implications-clop expert) prem)
          potential-counterexamples (potential-counterexamples-subcontext known-examples implication)
          ]
      (cond
        (= answer :yes)
        {:follows known-true
         :attributes-that-follow attributes-that-follow
         :potential-counterexamples (make-incomplete-context #{} M #{})}
        (= answer :no)
        {:follows known-false
         :attributes-that-follow #{} 
         :potential-counterexamples potential-counterexamples}
        (= answer :unknown)
        {:follows unknown
         :attributes-that-follow attributes-that-follow
         :potential-counterexamples potential-counterexamples}
        )))

  (extended-holds-in-view?-inc-counter [expert implication]
    (inc-interaction-counter expert)
    (extended-holds-in-view? expert implication))

  ;;; Counter for the expert interactions
  (interaction-counter [expert] (deref (:interaction-counter expert)))
  (inc-interaction-counter [expert] (swap! (:interaction-counter expert) inc))
  (reset-interaction-counter [expert] (reset! (:interaction-counter expert) 0))
    )

(defn make-expert
  "constructor function for Expert record.
  takes either [implications examples]
  or [name implications examples]"
  ([implications examples]
   (make-expert implications examples (hash [implications examples])))
  ([implications examples name]
   (let [clop (clop-by-implications implications)]
     (->(->Expert implications examples name)
        (assoc :interaction-counter (atom 0))
        (assoc :implications-clop clop)
        (assoc :follows-fn (fn [implication] (subset? (conclusion implication)
                                                      (clop (premise implication))))
               )))))
 
(defn expert-to-string
  ""
  [expert]
  (with-str-out  (str expert) "{:known-implications\n" (known-implications expert) ",\n\n" ":known-examples\n" (known-examples expert)))


(defmethod print-method Expert [expert out]
  (.write ^java.io.Writer out ^String (str expert)))


(defn- find-counterexample
  "find a counterexample for an implication R->m with single attribute conclusion"
  [cxt implication]
  (let [prem (premise implication)
        concl (conclusion implication)
        objs (certain-extent cxt prem)
        inz (incidence cxt)]
    (loop [o (first objs)
           remaining (rest objs)]
      (if (nil? o)
        (make-incomplete-context #{} (attributes cxt) #{})
        (if (subset? concl (possible-intent cxt #{o}))
          (recur (first remaining)
                 (rest remaining))
          (incomplete-subcontext cxt #{o} (attributes cxt))
          )))))

(defn- find-counterexamples
  "given a context and an implication all objects are checked whether they are a counterexample to the implication. A set of counterexamples is returned (or an empty set if none is found)"
  [context impl]
  (let [pre (premise impl)
        con (conclusion impl)
        violates-conclusion (fn [obj] (not  (empty? (intersection con (attributes-not-had context #{obj})))))
        has-premise (fn [obj] (subset? pre (attributes-had context #{obj})))
        transform (fn [obj] [obj (into #{} (attributes-had context #{obj})) (into #{} (attributes-not-had context #{obj}))])
        ]
    (->> (objects context) 
         (filter has-premise)
         (filter violates-conclusion)
         (map transform)
         (into [])
         )
    ))

(defn interaction-counters [experts]
  (into {} (for [e experts] [(:name e) (interaction-counter e)])))

(defn reset-interaction-counters [experts]
  (into {} (for [e experts] [(:name e) (reset-interaction-counter e)])))


(defn- uuid [] (.toString (java.util.UUID/randomUUID)))

(defn expert-handler
  "responds with all counterexamples"
  ([expert]
   (expert-handler expert (uuid)))
  ([expert namestr]
   (fn [context implications new-impl]
     (inc-interaction-counter expert)
     (let [expert-impls (known-implications expert)
           expert-cxt (known-examples expert)
           expert-clop (clop-by-implications expert-impls)
           pre (premise new-impl)
           con (conclusion new-impl)
           known-con-for-pre (expert-clop pre)
           counterexamples (find-counterexamples expert-cxt new-impl)
           answer (cond
                    ;; is the implication valid for the expert?
                    (subset? con known-con-for-pre)
                    [:implication]
                    (not (empty? counterexamples))
                    [:counterexample counterexamples]
                    true
                    (let [unknown (difference con known-con-for-pre)
                          premise-implies (intersection con known-con-for-pre)]
                      [:unknown {:premise-implies premise-implies :unknown unknown}])
                    )
           ;; _ (println "\n" (str namestr) (latex new-impl) "???:")
           ;; _ (println "\t" (first answer))
           ;; _ (if (= (first answer) :unknown) (println "\t" (second answer)))
           ;; _ (if (= (first answer) :counterexample)
           ;;     (println (latex (incomplete-subcontext expert-cxt 
           ;;                                            (into #{} (map first counterexamples)) 
           ;;                                            (attributes expert-cxt)))))
           ]
       answer
       ))))

(defn expert-handler-single-counterexample
  "responds with a single counterexamples"
  ([expert]
   (expert-handler-single-counterexample expert (uuid)))
  ([expert namestr]
   (fn [context implications new-impl]
     (inc-interaction-counter expert)
     (let [expert-impls (known-implications expert)
           expert-cxt (known-examples expert)
           expert-clop (clop-by-implications expert-impls)
           pre (premise new-impl)
           con (conclusion new-impl)
           known-con-for-pre (expert-clop pre)
           counterexamples (find-counterexamples expert-cxt new-impl)
           answer (cond
                    ;; is the implication valid for the expert?
                    (subset? con known-con-for-pre)
                    [:implication]
                    (not (empty? counterexamples))
                    [:counterexample (take 1 counterexamples)]
                    true
                    (let [unknown (difference con known-con-for-pre)
                          premise-implies (intersection con known-con-for-pre)]
                      [:unknown {:premise-implies premise-implies :unknown unknown}])
                    )
           ]
       answer
       ))))

(defn maximal-expert-knowledge 
  "returns an expert who has the maximal knowledge for a group of experts for a universe"
  [experts]
  (let [contexts (map #(known-examples %) experts)
        impls (canonical-base-from-base (reduce clojure.set/union #{} (map #(known-implications %) experts)))
        objects (reduce clojure.set/union #{} (map #(objects %) contexts))
        attributes (reduce clojure.set/union #{} (map #(attributes %) contexts))
        max-inz (->> 
                 contexts
                 (map #(incidence %))
                 (map #(filter (fn [[[g m] w]] (not (= unknown w))) %))
                 (reduce clojure.set/union #{})
                 (into (into {} (cross-product (cross-product objects attributes) #{unknown}))))
        ]
    (make-expert impls (make-incomplete-context objects attributes max-inz))))

(defalias maximal-expert maximal-expert-knowledge)


(defn expert-pool-handler-maximal-expert
  "Implements the theoretically maximal expert"
  [experts]
  (expert-handler (maximal-expert experts)))


(defn expert-pool-handler-random-expert-answer
  ""
  [experts]
  {:pre [(coll? experts) (not (empty? experts))]}
  (fn [context implications new-impl]
    (let [random-expert (rand-nth (into [] experts))
          random-expert-handler (expert-handler random-expert)]
      (random-expert-handler context implications new-impl))))



(defn expert-pool-handler-cycle-expert-answers
  "Implementation of the strategy \\phi_{iterative}"
  [experts]
  {:pre [(coll? experts) (not (empty? experts))]}
  (let [handlers (map expert-handler experts)]
    (fn [context implications new-impl]
      (let [f (fn [known-to-follow handler]
                (let [answer (handler context implications new-impl)]
                  (cond
                    (= (first answer) :implication)
                    (reduced answer)
                    (= (first answer) :counterexample)
                    (reduced answer)
                    true
                    (union known-to-follow (:premise-implies (second answer)))
                                              )))
            result (reduce f #{} handlers)]
        (if (or (= (first result) :implication) (= (first result) :counterexample)) 
          result
          (if (subset? (conclusion new-impl) result)
            [:implication]
            [:unknown {:premise-implies result :unknown (difference (conclusion new-impl) result)}]))
        )))
  )


(defn expert-pool-handler-maximal-knowledge-repetetive-broadcast
  "Implementation of the strategy \\phi_{RB}"
  [experts]
  {:pre [(coll? experts) (not (empty? experts))]}
  (fn [context implications new-impl]
    (let [M (set (attributes context))
          concl (set (conclusion new-impl))]
      (loop [F (premise new-impl)]
        (let [answers (map (fn [e] (extended-holds-in-view?-inc-counter e (make-implication F concl))) experts)
              Kpc (reduce incomplete-context-supremum (map :potential-counterexamples answers))
              Gc (objects (counterexamples-subcontext Kpc new-impl))]
          (if (not-empty Gc)
            [:counterexample (find-counterexamples Kpc new-impl)]
            (let [FL (into #{} (reduce union #{} (map :attributes-that-follow answers)))]
              (if (or (= FL F)
                      (= FL M))
                (if (subset? concl FL)
                  [:implication]
                  [:unknown {:premise-implies (intersection concl FL)
                             :unknown (difference concl FL)}])
                (recur FL)))))))))




(comment
  (do
    ;; Test find-counterexample
    (def fc-impl (make-implication ['a] ['b]))
    (def fc-context (make-incomplete-context [1 2 3] ['a 'b] [[1 'a known-true] [1 'b known-false]
                                                              [2 'a unknown] [2 'b known-true]
                                                              [3 'a known-true] [3 'b known-true]]))
    (assert (= (find-counterexamples fc-context fc-impl) [[1 #{'a} #{'b}]]))


    ;; Test Expert and Expert Handler
    (def kimpl #{(make-implication [] ['b])})
    (def kexampl (make-incomplete-context [4 5] ['a 'b 'c] {[4 'a] unknown, [4 'c] known-false, [4 'b] known-true [5 'a] known-false [5 'b] unknown [5 'c] known-false}))
    (def kimpl2 #{(make-implication ['a] ['c])})
    (def kexampl2 (make-incomplete-context [2] ['a 'b 'c] [[2 'a known-true] [2 'b unknown] [2 'c known-true]]))
    (def Ex1 (make-expert kimpl kexampl "name1"))
    (def Ex2 (make-expert kimpl2 kexampl2 "name2"))
                                        ;(known-implications Ex1)
                                        ;(known-examples Ex1)
                                        ;(maximal-expert-knowledge #{Ex1 Ex2})


    (def Ex1-handler (expert-handler Ex1))
    (explore-attributes :examples-context (make-incomplete-context [] ['a 'b 'c] []) 
                        :handler Ex1-handler)

    (def maxEx1Ex2-handler (expert-handler (maximal-expert-knowledge #{Ex1 Ex2})))
    (explore-attributes :examples-context (make-incomplete-context [] ['a 'b 'c] [])
                        :handler maxEx1Ex2-handler)

    (def random-expert-handler (expert-pool-handler-random-expert-answer #{Ex1 Ex2}))
    (explore-attributes :examples-context (make-incomplete-context [] ['a 'b 'c] [])
                        :handler random-expert-handler)

    (def RB-expert-handler (expert-pool-handler-maximial-knowledge-repetetive-broadcast [Ex1 Ex2]))
    (explore-attributes :examples-context (make-incomplete-context [] ['a 'b 'c] []) 
                        :handler RB-expert-handler)
    )
  )

(comment
  (do
    (def Ex3 (make-expert
              [(make-implication #{\a} #{\b})]
              (make-incomplete-context-from-matrix [1 2 3 4] [\a \b \c \d]
                                                   [1 1 ? ?
                                                    0 1 ? ?
                                                    ? 1 0 ?
                                                    ? ? 1 0])))
    (def Ex4 (make-expert
              [(make-implication #{\d} #{\b})]
              (make-incomplete-context-from-matrix [1 2 3 4] [\a \b \c \d]
                                                   [? ? 0 1
                                                    ? 1 1 1
                                                    ? 1 ? 1
                                                    ? 0 ? 0])))

    (def max34-handler (expert-handler (maximal-expert-knowledge #{Ex3 Ex4})))
    (explore-attributes :examples-context (make-incomplete-context []  #{\a \b \c \d} [])
                        :handler max34-handler)
    
    (def RB-expert-handler34 (expert-pool-handler-maximal-knowledge-repetetive-broadcast [Ex3 Ex4]))
    (explore-attributes :examples-context (make-incomplete-context []  #{\a \b \c \d} []) 
                        :handler RB-expert-handler34)

    (def Iter-expert-handler34 (expert-pool-handler-cycle-expert-answers [Ex3 Ex4]))
    (explore-attributes :examples-context (make-incomplete-context []  #{\a \b \c \d} []) 
                        :handler Iter-expert-handler34)
    


    (def living (conexp.io.contexts/json->ctx
                 (read-string 
                  "{:attributes (\"dicotyledon\" \"monocotyledon\" \"can move\" \"lives in water\" \"lives on land\" \"needs water to live\" \"needs chlorophyll\" \"breast feeds\" \"has limbs\"), :adjacency-list [{:object \"dog\", :attributes (\"has limbs\" \"breast feeds\" \"needs water to live\" \"lives on land\" \"can move\")} {:object \"fish leech\", :attributes (\"needs water to live\" \"lives in water\" \"can move\")} {:object \"corn\", :attributes (\"needs chlorophyll\" \"needs water to live\" \"lives on land\" \"monocotyledon\")} {:object \"bream\", :attributes (\"has limbs\" \"needs water to live\" \"lives in water\" \"can move\")} {:object \"water weeds\", :attributes (\"needs chlorophyll\" \"needs water to live\" \"lives in water\" \"monocotyledon\")} {:object \"bean\", :attributes (\"needs chlorophyll\" \"needs water to live\" \"lives on land\" \"dicotyledon\")} {:object \"frog\", :attributes (\"has limbs\" \"needs water to live\" \"lives on land\" \"lives in water\" \"can move\")} {:object \"reed\", :attributes (\"needs chlorophyll\" \"needs water to live\" \"lives on land\" \"lives in water\" \"monocotyledon\")}]}")
                 ))




    (require 'conexp.fca.incomplete-contexts.random-experts)
    (defn- make-n-random-experts-for-universe
      [K n]
      (for [i (range n)] (conexp.fca.incomplete-contexts.random-experts/make-random-expert-for-universe K (str "E-" i))))

    (defn- implication-sets-same-premises?
      [L1 L2]
      (let [P1 (->> L1
                    (map premise)
                    (into #{}))
            P2 (->> L2
                    (map premise)
                    (into #{}))]
        (= P1 P2)))
    
    (defn- implication-sets-equivalent?
      [L1 L2]
      (let [clop1 (clop-by-implications L1)
            follows1 (fn [impl] (subset? (conclusion impl)
                                         (clop1 (premise impl))))
            clop2 (clop-by-implications L2)
            follows2 (fn [impl] (subset? (conclusion impl)
                                         (clop2 (premise impl))))]
        (every? true? (into (into [] (for [RS L1] (follows2 RS)))
                            (for [RS L2] (follows1 RS))))
        ))


    (defn- test-and-log-for-n-experts
      [K n]
      (let [experts (make-n-random-experts-for-universe K n)
            maxExpert (maximal-expert experts)
            handlers (-> {}
                         (assoc :maxExpert                            (expert-handler maxExpert))
                         (assoc :phiRB                         (expert-pool-handler-maximal-knowledge-repetetive-broadcast experts))
                         (assoc :phiITER (expert-pool-handler-cycle-expert-answers experts)))
            Kempty (make-empty-incomplete-context (attributes (known-examples (first experts))))]
        (let [ results
              (->>
               (for [[hk hv] handlers]
                 (do
                   (println "start" hk)
                   (let [res (explore-attributes :examples-context Kempty :handler hv)]
                     (println (:implications res))
                     (println "handler:" hk (interaction-counters experts) "maxexpert:" (interaction-counter maxExpert))
                     (reset-interaction-counters experts)
                     (reset-interaction-counter maxExpert)
                     (println "end" hk)
                     [hk (:implications res)]
                     )))
               (into {})
               )]
          (println "max=RB"  (implication-sets-equivalent? (:maxExpert results) (:phiRB results)))
          (println "max=iter?" (implication-sets-equivalent? (:maxExpert results) (:phiITER results)))
          (println "RB=iter?" (implication-sets-equivalent? (:phiRB results) (:phiITER results)))

          )))
    )
  (test-and-log-for-n-experts living 5)
  )



(comment
  ;;; example for different bases with maxexpert, RB and iterative
  (do
    (def Kempty (make-empty-incomplete-context [1 2 3 4 5]))
    (def e1 (make-expert [(make-implication [1] [2])] (make-empty-incomplete-context [1 2 3 4 5])))
    (def e2 (make-expert [(make-implication [2] [3])] (make-empty-incomplete-context [1 2 3 4 5])))
    (def e3 (make-expert [(make-implication [3] [4])] (make-empty-incomplete-context [1 2 3 4 5])))
    (def e4 (make-expert [(make-implication [4] [5])] (make-empty-incomplete-context [1 2 3 4 5])))
    (def e5 (make-expert [(make-implication [] [1])] (make-empty-incomplete-context [1 2 3 4 5])))
    (def eMax (maximal-expert [e1 e2 e3 e4 e5]))
    (def iterative-handler (expert-pool-handler-cycle-expert-answers [e1 e2 e3 e4 e5]))
    (def RB-handler (expert-pool-handler-maximal-knowledge-repetetive-broadcast [e1 e2 e3 e4 e5]))
    (def eMax-handler (expert-handler eMax))
    
    (iterative-handler Kempty [] (make-implication [1] [5]))
    (eMax-handler Kempty [] (make-implication [1] [5]))
    (def resI (explore-attributes :examples-context Kempty :handler iterative-handler))
    (def resRB (explore-attributes :examples-context Kempty :handler RB-handler))
    (def resMAX (explore-attributes :examples-context Kempty :handler eMax-handler))
    (println)
    (println "I-RB same implication sets" (implication-sets-same-premises? (:implications resI)
                                                                           (:implications resRB)))
    (println "I-MAX same implication sets" (implication-sets-same-premises? (:implications resI)
                                                                           (:implications resMAX)))
    (println "RB-MAX same implication sets" (implication-sets-same-premises? (:implications resRB)
                                                                           (:implications resMAX)))
    (println "I-RB same implications follow" (implication-sets-equivalent? (:implications resI)
                                                                           (:implications resRB)))
    (println "I-MAX same implications follow" (implication-sets-equivalent? (:implications resI)
                                                                           (:implications resMAX)))
    (println "RB-MAX same implications follow" (implication-sets-equivalent? (:implications resRB)
                                                                           (:implications resMAX)))
    )
  )
