;; Copyright (c) Daniel Borchmann. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.contrib.dl.languages.interaction
  (:use conexp.main
	conexp.contrib.dl.framework.syntax
	conexp.contrib.dl.framework.models))

;;;

(defn expert-refuses?
  "Asks an expert (the user) whether a given subsumption is to be refuted or not."
  [subsumption]
  (let [answer (ask (str "Do you accept the subsumption \n\n"
			 (print-str subsumption)
			 " ?\n\n")
		    '#{yes no}
		    "Please enter yes or no.\n")]
    (if (= answer 'yes)
      false
      true)))

(defn extend-model-by-contradiction
  "Extends given model by asking an expert (the user) to extend model
  to a connected supermodel in which the given subsumption does not
  hold."
  [model subsumption]
  (when-not (holds-in-model? model subsumption)
    (illegal-argument "Given subsumption " (print-str subsumption) " is already wrong in model."))
  (let [language (model-language model),
	old-objects (model-base-set model),

	new-objects (loop []
		      (println "Please enter your new objects:")
		      (let [new-objects (read-string (str "#{" (first (drop-while empty? (repeatedly read-line))) "}"))]
			(if-not (empty? (intersection old-objects new-objects))
			  (do
			    (println "The objects " (difference new-objects old-objects)
				     " are already definied in the given model. Please use other names.")
			    (recur))
			  (if-not (every? #(Character/isUpperCase #^Character (first (str %)))
					  new-objects)
			    (do
			      (println (str "New objects need to start with a capital letter."))
			      (recur))
			    new-objects)))),
	interpretation (loop [objects (seq new-objects),
			      attributes {}]
			 (if (empty? objects)
			   attributes
			   (let [next-object (first objects),
				 new-attributes
				 (loop []
				   (let [_ (println (str "Please enter concept- and role-names which are satisfied by " next-object ":"))
					 new-attributes (read-string (str "(" (read-line) ")"))]
				     (if-let [invalid (first (filter #(not (or (symbol? %)
									       (and (seq? %) (= 2 (count %)))))
								     new-attributes))]
				       (do
					 (println (str "Attribute " invalid " is invalid, i.e. it's neither a symbol nor a pair."))
					 (recur))
				       (if-let [invalid (first (for [att new-attributes
								     :when (or (and (symbol? att)
										    (not (contains? (concept-names language) att)))
									       (and (seq? att)
										    (or (not (contains? (role-names language) (first att)))
											(not (contains? new-objects (second att))))))]
								 att))]
					 (do
					   (if (symbol? invalid)
					     (println (str invalid " is not a valid concept name"))
					     (println (str invalid " does not contain a valid role name or has an invalid object\n"
							   "(note that you can only use new objects in role specifications.")))
					   (recur))
					 new-attributes))))]
			     (recur (rest objects)
				    (assoc attributes next-object new-attributes))))),
	new-att-map (loop [pairs interpretation,
			   att-map (model-interpretation model)]
		      (if (empty? pairs)
			att-map
			(let [[object attributes] (first pairs)]
			  (recur (rest pairs)
                                 (reduce (fn [att-map att]
                                           (if (symbol? att)
                                             (update-in att-map [att] conj object)
                                             (update-in att-map [(first att)] conj [object (second att)])))
                                         att-map
                                         attributes))))),
	new-model (make-model language (union old-objects new-objects) new-att-map)]
    (if (holds-in-model? new-model subsumption)
      (do
	(println (str "New model does not contradict subsumption " (print-str subsumption) ", restarting."))
	(recur model subsumption))
      new-model)))

;;;

nil