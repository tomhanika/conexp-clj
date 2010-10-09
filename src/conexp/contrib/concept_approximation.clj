;; Copyright (c) Daniel Borchmann. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.contrib.concept-approximation
  (:use conexp.main))

(ns-doc "Concept Approximation as described by C. Meschke.")

;;;

(defn- apprx-handler
  "Special handler for concept approximation exploration."
  [ctx new-impl new-objs new-handler]
  (let [[result new-col] (default-handler ctx new-impl)]
    (if result
      [result new-col]
      (let [new-att  (first new-col),
            att-objs (ask (str "Which of the objs " new-objs " definitively has the attribute " new-att "? ")
                          #(read-string (str "#{" (read-line) "}"))
                          #(subset? % new-objs)
                          "Please enter only objects mentioned: ")]
        (new-handler (map #(vector % new-att) att-objs))
        [result new-col]))))

(defn explore-approximations
  "Performs concept approximation exploration and returns the final
  context."
  [context]
  (let [_                (println "Starting attribute exploration")
        att-explored-ctx (:context (explore-attributes context)),

        _                (println "Starting object exploration")
        new-objects      (difference (objects att-explored-ctx)
                                     (objects context)),
        known-crosses    (atom #{}),
        handler          (fn [ctx impl]
                           (apprx-handler ctx impl new-objects
                                          #(swap! known-crosses into %))),
        obj-explored-ctx (dual-context
                          (:context (explore-attributes
                                     (dual-context context)
                                     #{}
                                     handler)))]
    (context-subposition
     (context-apposition context obj-explored-ctx)
     (context-apposition att-explored-ctx
                         (smallest-bond att-explored-ctx
                                        obj-explored-ctx
                                        (union (incidence att-explored-ctx)
                                               (incidence obj-explored-ctx)
                                               @known-crosses))))))

;;;

nil
