;;
;; Experiments on Sebastian Rudolph's algorithm to compute the canonical base
;;


(ns conexp.contrib.experimental.rudolph-computation
  (:use conexp.main))

;;

(defn rudolph-base [ctx]
  (let [new-names (reduce! (fn [map g]
                             (assoc! map g (gensym (str g "-"))))
                           {}
                           (objects ctx)),
        pairs     (for [m (attributes ctx)]
                    [#{m} (map new-names
                               (difference (objects ctx)
                                           (aprime ctx #{m})))])]
    [(union (set-of (make-implication X Y) | [X Y] pairs)
            (set-of (make-implication Y X) | [X Y] pairs))
     (set (vals new-names))]))

(defonce ctx (random-context 16 16 0.7))

(defn context-to-imp-set [ctx]
  (let [[base auxiliary-names] (rudolph-base ctx),
        base                   (atom base),
        impls                  (atom (transient #{}))]
    (doseq [name auxiliary-names]
      (reset! impls (transient #{}))
      (doseq [A→B @base
              :let [A (premise A→B)
                    B (conclusion A→B)]]
        (if (contains? B name)
          (do
            (swap! impls conj! (make-implication A (disj B name)))
            (when (not (contains? A name))
              (doseq [C→D @base
                      :when (contains? (premise C→D) name)
                      :let [C (premise C→D)
                            D (conclusion C→D)]]
                (swap! impls conj! (make-implication (disj (union A C) name)
                                                     D)))))
          (when (not (contains? A name))
            (swap! impls conj! A→B))))
      (reset! base (canonical-base-from-base (persistent! @impls))))
    @base))

;;

nil
