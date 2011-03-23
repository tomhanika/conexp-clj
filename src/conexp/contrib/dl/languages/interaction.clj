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
        conexp.contrib.dl.framework.semantics))

;;;

(defn ^{:dynamic true} expert-refuses?
  "Asks an expert (the user) whether a given subsumption is to be
  refuted or not. This is a dynamically defined function and can be
  rebound."
  [subsumption]
  (let [answer (ask (str "Do you accept the subsumption \n\n"
                         (print-str subsumption)
                         " ?\n\n")
                    read
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
  (when-not (holds-in-interpretation? model subsumption)
    (illegal-argument "Given subsumption "
                      (print-str subsumption)
                      " is already wrong in model."))
  (let [language    (interpretation-language model),
        old-objects (interpretation-base-set model),

        new-objects (ask "Please enter your new objects: "
                         #(read-string (str "#{" (first (drop-while nil? (repeatedly read-line))) "}"))
                         not-empty
                         "Please specify at least one new object.\n",
                         #(empty? (intersection old-objects %))
                         #(str "The objects "
                               (intersection % old-objects)
                               " are already defined. Please use other names.\n"),
                         #(every? (fn [s]
                                    (Character/isUpperCase ^Character (first (str s))))
                                  %)
                         (str "New objects need to start with a capital letter.\n")),
        
        interfun    (loop [objects    (seq new-objects),
                           attributes {}]
                         (if (empty? objects)
                           attributes
                           (let [next-object (first objects),
                                 new-attributes
                                 (loop []
                                   (println (str "Please enter concept-names and role-relations which are satisfied by " next-object ":"))
                                   (let [new-attributes (read-string (str "(" (read-line) ")"))]
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
                                             (println (str invalid
                                                           " is not a valid concept name."))
                                             (println (str invalid
                                                           " does not contain a valid role name or has an invalid object\n"
                                                           "(note that you can only use new objects in role specifications.)")))
                                           (recur))
                                         new-attributes))))]
                             (recur (rest objects)
                                    (assoc attributes next-object new-attributes))))),
        
        new-att-map (loop [pairs   interfun,
                           att-map (interpretation-function model)]
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
        
        new-model (make-interpretation language (union old-objects new-objects) new-att-map)]

    (if (holds-in-interpretation? new-model subsumption)
      (do
        (println (str "New model does not contradict subsumption " (print-str subsumption) ", restarting."))
        (recur model subsumption))
      new-model)))

;;;

nil