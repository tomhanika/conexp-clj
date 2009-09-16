(ns conexp.fca.exploration
  (:use clojure.set
	conexp.util
	conexp.base
	conexp.fca.contexts
	conexp.fca.implications))

(defn explore-attributes [ctx handler]
  (loop [implications #{}
	 last         #{}
	 ctx          ctx]
    (if (not last)
      implications
      (let [conclusion-from-last (context-attribute-closure ctx last)]
	(if (= last conclusion-from-last)
	  (recur implications
		 (next-closed-set (attributes ctx)
				  (clop-by-implications* implications)
				  last)
		 ctx)
	  (let [new-impl (make-implication last conclusion-from-last)
		[ok new-row] (handler ctx new-impl)]
	    (if ok
	      (let [new-implications (conj implications new-impl)]
		(recur new-implications
		       (next-closed-set (attributes ctx)
					(clop-by-implications* new-implications)
					last)
		       ctx))
	      (let [new-ctx (make-context (conj (objects ctx) (new-row 0))
					  (attributes ctx)
					  (union (incidence ctx) (new-row 1)))]
		(recur implications
		       (next-closed-set (attributes new-ctx)
					(clop-by-implications* implications)
					last)
		       new-ctx)))))))))

(defn falsifies-implication? [new-atts impl]
  (and (subset? (premise impl) new-atts)
       (not (subset? (conclusion impl) new-atts))))

(defn ask [prompt pred fail-message]
  (do
    (print prompt)
    (flush)
    (loop [answer (read)]
      (if (pred answer)
	answer
	(do
	  (print fail-message)
	  (flush)
	  (recur (read)))))))

(defn default-handler [ctx impl]
  (do
    (prn ctx)
    (let [answer (ask (str "Does the implication " impl " hold? ") 
		      #{'yes 'no} 
		      "Please answer 'yes' or 'no': ")]
      (if (= answer 'yes)
	[true []]
	(let [new-obj (ask (str "Please enter new object: ")
			   (fn [new-obj] (not ((objects ctx) new-obj)))
			   "This object is already present, please enter a new one: ")
	      new-att (ask (str "Please enter the attributes the new object should have: ")
			   (fn [new-atts]
			     (and (subset? new-atts (attributes ctx))
				  (falsifies-implication? new-atts impl)))
			   "These attributes are not valid or do not falsify the implication.\nPlease enter a new set (in the form #{... atts ...}): "
				  
			   )]
	  [false [new-obj (set (map (fn [att] [new-obj att]) new-att))]])))))
