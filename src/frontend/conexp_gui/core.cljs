(ns conexp-gui.core
  "Entry point for the conexp-clj browser GUI."
  (:require [reagent.dom.client :as rdomc]
            [re-frame.core :as rf]
            [conexp-gui.events :as e]
            [conexp-gui.subs :as s]
            [conexp-gui.views.context-editor :refer [context-editor]]
            [conexp-gui.views.lattice-diagram :refer [lattice-diagram]]))

(defn header []
  [:header {:style {:display "flex" :align-items "baseline" :gap "0.75rem"
                    :padding "0.6rem 1rem" :background "#2b3a55" :color "white"}}
   [:strong {:style {:font-size "1.1rem"}} "conexp-clj"]
   [:span {:style {:opacity 0.7 :font-size "0.8rem"}} "Formal Concept Analysis"]])

(defn app []
  (let [view @(rf/subscribe [::s/view])]
    [:div {:style {:display "flex" :flex-direction "column" :height "100vh"}}
     [header]
     [:main {:style {:flex 1 :min-height 0 :overflow "auto"}}
      (case view
        :diagram [lattice-diagram]
        [context-editor])]]))

(defonce root (atom nil))

(defn mount []
  (rdomc/render @root [app]))

(defn ^:export init []
  (rf/dispatch-sync [::e/init])
  (let [el (js/document.getElementById "app")]
    (reset! root (rdomc/create-root el))
    (mount)))

(defn ^:dev/after-load reload []
  (mount))
