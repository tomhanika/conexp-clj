(ns conexp-gui.views.context-editor
  "Spreadsheet-style formal-context editor."
  (:require [re-frame.core :as rf]
            [conexp-gui.events :as e]
            [conexp-gui.subs :as s]))

(defn- name-cell
  "An editable header cell that renames an object/attribute on blur/Enter."
  [value on-rename]
  [:input {:type "text"
           :default-value value
           :key value
           :style {:width "3.5rem" :border "none" :background "transparent"
                   :font "inherit" :text-align "center"}
           :on-blur     #(on-rename (.. % -target -value))
           :on-key-down #(when (= "Enter" (.-key %)) (.. % -target blur))}])

(defn context-editor []
  (let [{:keys [objects attributes incidence]} @(rf/subscribe [::s/context])
        loading? @(rf/subscribe [::s/loading?])]
    [:div {:style {:padding "1rem"}}
     [:div {:style {:overflow-x "auto"}}
      [:table {:style {:border-collapse "collapse"}}
       [:thead
        [:tr
         [:th {:style {:border "1px solid #ccc" :padding "0.2rem"}}]
         (for [a attributes]
           ^{:key a}
           [:th {:style {:border "1px solid #ccc" :padding "0.2rem"}}
            [name-cell a #(rf/dispatch [::e/rename-attribute a %])]])]]
       [:tbody
        (for [o objects]
          ^{:key o}
          [:tr
           [:th {:style {:border "1px solid #ccc" :padding "0.2rem"}}
            [name-cell o #(rf/dispatch [::e/rename-object o %])]]
           (for [a attributes]
             ^{:key (str o "|" a)}
             [:td {:style {:border "1px solid #ccc" :text-align "center"
                           :cursor "pointer" :width "2.2rem" :height "2.2rem"}
                   :on-click #(rf/dispatch [::e/toggle-incidence o a])}
              (when (contains? incidence [o a]) "✕")])])]]]
     [:div {:style {:margin-top "0.8rem" :display "flex" :gap "0.5rem" :flex-wrap "wrap"}}
      [:button {:on-click #(rf/dispatch [::e/add-object])}    "+ object"]
      [:button {:on-click #(rf/dispatch [::e/add-attribute])} "+ attribute"]
      [:button {:disabled loading?
                :style {:font-weight "bold"}
                :on-click #(rf/dispatch [::e/show-lattice])}
       (if loading? "Computing…" "Show Concept Lattice")]]]))
