(ns conexp-gui.events
  "re-frame events + effects for the conexp-clj GUI."
  (:require [re-frame.core :as rf]
            [conexp-gui.api-client :as api]
            [conexp.gui-shared.layout-json :as lj]
            [conexp.layouts.movement :as mv]))

;;; initial state

(def default-db
  {:context   {:objects    ["1" "2" "3"]
               :attributes ["a" "b" "c"]
               :incidence  #{["1" "a"] ["2" "a"] ["2" "b"]
                             ["3" "a"] ["3" "b"] ["3" "c"]}}
   :layout-name    "standard"
   :valuation-name "none"
   :move-mode      "single"
   :model          nil
   :view           :editor
   :loading?       false
   :error          nil
   :highlight      #{}})

(rf/reg-event-db ::init (fn [_ _] default-db))

;;; effect: JSON-RPC call

(rf/reg-fx
 ::rpc
 (fn [{:keys [request on-success on-error]}]
   (api/post-rpc request
                 #(rf/dispatch (conj on-success %))
                 #(rf/dispatch (conj on-error %)))))

;;; context editing

(rf/reg-event-db
 ::toggle-incidence
 (fn [db [_ o a]]
   (update-in db [:context :incidence]
              (fn [inc] (if (contains? inc [o a]) (disj inc [o a]) (conj inc [o a]))))))

(defn- rename-in
  "Renames `old`->`new` in the object/attribute vector `k` and rewrites the
  incidence pairs (position `slot` = 0 for objects, 1 for attributes)."
  [db k slot old new]
  (if (or (empty? new) (some #{new} (get-in db [:context k])))
    db
    (-> db
        (update-in [:context k] (fn [xs] (mapv #(if (= % old) new %) xs)))
        (update-in [:context :incidence]
                   (fn [inc] (set (map #(if (= (nth % slot) old) (assoc % slot new) %) inc)))))))

(rf/reg-event-db ::rename-object    (fn [db [_ o n]] (rename-in db :objects 0 o n)))
(rf/reg-event-db ::rename-attribute (fn [db [_ a n]] (rename-in db :attributes 1 a n)))

(defn- fresh-name [existing base]
  (loop [i 1] (let [n (str base i)] (if (some #{n} existing) (recur (inc i)) n))))

(rf/reg-event-db
 ::add-object
 (fn [db _] (update-in db [:context :objects] conj (fresh-name (get-in db [:context :objects]) "o"))))

(rf/reg-event-db
 ::add-attribute
 (fn [db _] (update-in db [:context :attributes] conj (fresh-name (get-in db [:context :attributes]) "a"))))

;;; compute / show the diagram

(rf/reg-event-fx
 ::show-lattice
 (fn [{:keys [db]} _]
   {:db  (assoc db :loading? true :error nil :view :diagram)
    ::rpc {:request    (api/layout-with-valuation-request
                        (:context db) (:layout-name db) (:valuation-name db))
           :on-success [::layout-loaded]
           :on-error   [::api-error]}}))

(rf/reg-event-fx
 ::reload-layout
 (fn [{:keys [db]} _]
   {:db  (assoc db :loading? true :error nil)
    ::rpc {:request    (api/layout-with-valuation-request
                        (:context db) (:layout-name db) (:valuation-name db))
           :on-success [::layout-loaded]
           :on-error   [::api-error]}}))

(rf/reg-event-db
 ::layout-loaded
 (fn [db [_ resp]]
   (if-let [layout (api/layout-result resp)]
     (assoc db :loading? false :error nil :model (lj/render-model layout))
     (assoc db :loading? false :error (or (get-in resp [:layout :msg]) "No layout in response")))))

(rf/reg-event-db ::api-error (fn [db [_ err]] (assoc db :loading? false :error err)))

(rf/reg-event-fx ::set-layout-name    (fn [{:keys [db]} [_ v]] {:db (assoc db :layout-name v)    :dispatch [::reload-layout]}))
(rf/reg-event-fx ::set-valuation-name (fn [{:keys [db]} [_ v]] {:db (assoc db :valuation-name v) :dispatch [::reload-layout]}))
(rf/reg-event-db ::set-move-mode      (fn [db [_ v]] (assoc db :move-mode v)))
(rf/reg-event-db ::set-view           (fn [db [_ v]] (assoc db :view v)))

;;; interaction: node highlight + drag (with movement.cljc + order clamp)

(rf/reg-event-db
 ::toggle-highlight
 (fn [db [_ id]]
   (update db :highlight (fn [h] (if (contains? h id) (disj h id) (conj h id))))))

(defn- co-moving
  "Returns {node-id weight} of the nodes that move together with `id` under the
  given move-mode, using the shared movement.cljc traversals."
  [model id mode]
  (let [{:keys [uppers lowers]} (lj/neighbour-fns model)]
    (case mode
      "ideal"  (into {id 1} (map #(vector % 1) (mv/reachable-nodes lowers id)))
      "filter" (into {id 1} (map #(vector % 1) (mv/reachable-nodes uppers id)))
      "chain"  (into {id 1} (map #(vector % 1)
                                 (concat (mv/reachable-nodes lowers id)
                                         (mv/reachable-nodes uppers id))))
      "inf"    (into {id 1} (mv/additively-influenced-nodes id uppers lowers))
      "sup"    (into {id 1} (mv/additively-influenced-nodes id lowers uppers))
      ;; "single" / default
      {id 1})))

(rf/reg-event-db
 ::move-node
 (fn [db [_ id dx dy]]
   (let [model  (:model db)
         by-id  (into {} (map (juxt :id identity) (:nodes model)))
         {:keys [uppers lowers]} (lj/neighbour-fns model)
         node   (by-id id)
         upy    (seq (map #(:y (by-id %)) (uppers id)))
         lowy   (seq (map #(:y (by-id %)) (lowers id)))
         ;; clamp the dragged node so it stays between its lower/upper neighbours
         ny     (cond-> (+ (:y node) dy)
                  upy  (min (apply min upy))
                  lowy (max (apply max lowy)))
         dy*    (- ny (:y node))
         w      (co-moving model id (:move-mode db))
         nodes' (mapv (fn [n]
                        (if-let [k (w (:id n))]
                          (assoc n :x (+ (:x n) (* dx k))
                                   :y (+ (:y n) (* dy* k)))
                          n))
                      (:nodes model))]
     (assoc-in db [:model :nodes] nodes'))))
