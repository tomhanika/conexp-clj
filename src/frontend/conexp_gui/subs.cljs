(ns conexp-gui.subs
  "re-frame subscriptions for the conexp-clj GUI."
  (:require [re-frame.core :as rf]))

(rf/reg-sub ::context        (fn [db _] (:context db)))
(rf/reg-sub ::model          (fn [db _] (:model db)))
(rf/reg-sub ::view           (fn [db _] (:view db)))
(rf/reg-sub ::loading?       (fn [db _] (:loading? db)))
(rf/reg-sub ::error          (fn [db _] (:error db)))
(rf/reg-sub ::layout-name    (fn [db _] (:layout-name db)))
(rf/reg-sub ::valuation-name (fn [db _] (:valuation-name db)))
(rf/reg-sub ::move-mode      (fn [db _] (:move-mode db)))
(rf/reg-sub ::highlight      (fn [db _] (:highlight db)))
