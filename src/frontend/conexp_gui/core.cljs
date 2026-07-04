(ns conexp-gui.core
  "Entry point for the conexp-clj browser GUI."
  (:require [reagent.dom.client :as rdomc]
            [re-frame.core :as rf]))

;;; minimal re-frame skeleton (fleshed out in later views)

(rf/reg-event-db
 ::init-db
 (fn [_ _]
   {:view :welcome}))

(rf/reg-sub
 ::view
 (fn [db _] (:view db)))

(defn app []
  [:div {:style {:padding "2rem" :max-width "48rem" :margin "0 auto"}}
   [:h1 "conexp-clj"]
   [:p "Browser GUI — build scaffold is live."]
   [:p {:style {:opacity 0.7 :font-size "0.9rem"}}
    "Talks to the conexp.api REST server (default http://localhost:8080)."]])

(defonce root (atom nil))

(defn mount []
  (rdomc/render @root [app]))

(defn ^:export init []
  (rf/dispatch-sync [::init-db])
  (let [el (js/document.getElementById "app")]
    (reset! root (rdomc/create-root el))
    (mount)))

;; hot-reload hook
(defn ^:dev/after-load reload []
  (mount))
