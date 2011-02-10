;; -*- clojure -*-

(use 'conexp.main)

;;

(defn context-for-clop-v0
  "Computes context for clop the naive way."
  [c M]
  (let [K (make-context (all-closed-sets M c)
                        M
                        #(contains? %1 %2))]
    (reduce-objects K)))

;;

(defn context-for-clop-v1
  "Computes context for clop with AE."
  [c M]
  (let [K (:context
           (explore-attributes (make-context [] M [])
                               :handler (fn [_ _ impl]
                                          (let [A (premise impl),
                                                B (conclusion impl),
                                                cA (c A)]
                                            (if (subset? B cA)
                                              nil
                                              [[(gensym), cA]])))))]
    (reduce-objects K)))

;;

(defn maximal-counterexample
  [c M B C m]
  (loop [C C,
         elts (difference (disj M m) C)]
    (if (empty? elts)
      C
      (let [n (first elts),
            new-C (c (conj C n))]
        (if (contains? new-C m)
          (recur C (rest elts))
          (recur new-C (rest elts)))))))

(defn context-for-clop-v2
  "Computes context for clop with AE and maximal counterexamples."
  [c M]
  (:context
   (explore-attributes (make-context #{} M #{})
                       :handler (fn [_ _ impl]
                                  (let [A (premise impl),
                                        B (conclusion impl),
                                        C (c A)]
                                    (when-not (subset? B C)
                                      [[(gensym),
                                        (maximal-counterexample c M B C
                                                                (first (difference B C)))]]))))))

;;

(defn context-for-clop-v3
  "Computes context for clop with AE and multiple maximal
  counterexamples."
  [c M]
  (:context
   (explore-attributes (make-context #{} M #{})
                       :handler (fn [_ _ impl]
                                  (let [A (premise impl),
                                        B (conclusion impl),
                                        C (c A)]
                                    (when-not (subset? B C)
                                      (map #(vector (gensym) %)
                                           (distinct
                                            (map #(maximal-counterexample c M B C %)
                                                 (difference B C))))))))))

;;; Testing

(use 'clojure.pprint)

(defmacro time* [& body]
  `(let [start# (System/currentTimeMillis)]
     ~@body
     (/ (- (System/currentTimeMillis)
           start#)
        1000.0)))

(defn test-context-for-clop-versions [number-of-contexts size-of-base-set]
  (doseq [ctx  (take number-of-contexts
                     (remove #(or (empty? (objects %))
                                  (empty? (attributes %)))
                             (repeatedly #(reduce-context
                                           (rand-context (rand-int (expt 2 size-of-base-set))
                                                         size-of-base-set
                                                         (rand 1.0))))))]
    (let [cntr   (atom 0)
          clop   #(do (swap! cntr inc)
                      (context-attribute-closure ctx %)),
          M      (attributes ctx),
          reset! (fn [atom]
                   (let [val @atom]
                     (reset! atom 0)
                     val))]
      (cl-format *out*
                 "~5d ~5d ~5d ~5d ~8,3f ~5d ~8,3f ~5d ~8,3f ~5d ~8,3f ~5d~%"
                 (count (objects ctx))
                 (count (attributes ctx))
                 (count (concepts ctx))
                 (count (pseudo-intents ctx))
                 (time* (context-for-clop-v0 clop M))
                 (reset! cntr)
                 (time* (context-for-clop-v1 clop M))
                 (reset! cntr)
                 (time* (context-for-clop-v2 clop M))
                 (reset! cntr)
                 (time* (context-for-clop-v3 clop M))
                 (reset! cntr)))))

;;

nil
