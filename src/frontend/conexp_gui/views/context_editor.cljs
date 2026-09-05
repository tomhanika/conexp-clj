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

(def ^:private examples
  [["Living beings & water"
    {:objects    ["leech" "bream" "frog" "dog" "reed" "bean"]
     :attributes ["needs water" "lives in water" "lives on land" "needs chlorophyll" "two seed leaves" "can move"]
     :incidence  #{["leech" "needs water"] ["leech" "lives in water"] ["leech" "can move"]
                   ["bream" "needs water"] ["bream" "lives in water"] ["bream" "can move"]
                   ["frog" "needs water"] ["frog" "lives in water"] ["frog" "lives on land"] ["frog" "can move"]
                   ["dog" "needs water"] ["dog" "lives on land"] ["dog" "can move"]
                   ["reed" "needs water"] ["reed" "lives in water"] ["reed" "lives on land"] ["reed" "needs chlorophyll"]
                   ["bean" "needs water"] ["bean" "lives on land"] ["bean" "needs chlorophyll"] ["bean" "two seed leaves"]}}]
   ["Digits 1–4"
    {:objects    ["1" "2" "3" "4"]
     :attributes ["even" "odd" "prime" "square"]
     :incidence  #{["1" "odd"] ["1" "square"] ["2" "even"] ["2" "prime"]
                   ["3" "odd"] ["3" "prime"] ["4" "even"] ["4" "square"]}}]])

(defn- load-bar []
  [:div {:style {:display "flex" :gap "1rem" :align-items "center"
                 :flex-wrap "wrap" :margin-bottom "0.8rem"}}
   [:label {:style {:font-size "0.85rem"}}
    "Load context file: "
    [:input {:type "file"
             :accept ".cxt,.ctx,.csv,.txt,.json,.slf,.con"
             :on-change (fn [e]
                          (when-let [f (aget (.. e -target -files) 0)]
                            (let [rdr (js/FileReader.)]
                              (set! (.-onload rdr)
                                    #(rf/dispatch [::e/load-context-file (.. % -target -result)]))
                              (.readAsText rdr f))))}]]
   [:select {:value ""
             :on-change (fn [e]
                          (let [i (.. e -target -value)]
                            (when (seq i)
                              (rf/dispatch [::e/load-example (second (nth examples (js/parseInt i 10)))]))))}
    [:option {:value ""} "— load example —"]
    (map-indexed (fn [i [nm _]] ^{:key i} [:option {:value i} nm]) examples)]])

(defn context-editor []
  (let [{:keys [objects attributes incidence]} @(rf/subscribe [::s/context])
        loading? @(rf/subscribe [::s/loading?])
        error    @(rf/subscribe [::s/error])]
    [:div {:style {:padding "1rem"}}
     [load-bar]
     (when error [:div {:style {:color "#c53030" :margin-bottom "0.6rem"}} (str "Error: " error)])
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
