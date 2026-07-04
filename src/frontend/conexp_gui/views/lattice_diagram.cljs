(ns conexp-gui.views.lattice-diagram
  "SVG Hasse-diagram renderer with node dragging (order-clamped, via the shared
  movement.cljc), zoom/pan, valuation/layout/move-mode controls and export."
  (:require [reagent.core :as r]
            [re-frame.core :as rf]
            [clojure.string :as str]
            [conexp-gui.events :as e]
            [conexp-gui.subs :as s]))

(def ^:private node-radius 7)

(def ^:private layouts
  [["standard" "Standard"] ["inf-additive" "Inf-additive"]
   ["simple-layered" "Layered"] ["as-chain" "Chain"]
   ["dim-draw" "DimDraw"] ["freese" "Freese"] ["force" "Force"]])

(def ^:private valuations
  [["none" "None"] ["count-exts" "|Extent|"] ["count-ints" "|Intent|"]
   ["support" "Support"] ["stability" "Stability"] ["separation-index" "Separation"]
   ["modularity" "Modularity"] ["distributivity" "Distributivity"] ["probability" "Probability"]])

(def ^:private move-modes
  [["single" "Single"] ["ideal" "Ideal (below)"] ["filter" "Filter (above)"]
   ["chain" "Chain"] ["inf" "Inf-additive"] ["sup" "Sup-additive"]])

;;; coordinate helpers

(defn- fit-viewbox
  "Initial viewBox {:x :y :w :h} covering the model bounds with padding.
  Diagram is drawn Y-flipped (svgY = max-y - y) so higher concepts sit on top."
  [{:keys [min-x max-x min-y max-y]}]
  (let [w   (max 1 (- max-x min-x))
        h   (max 1 (- max-y min-y))
        pad (* 0.15 (max w h))]
    {:x (- min-x pad) :y (- pad) :w (+ w (* 2 pad)) :h (+ h (* 2 pad))}))

(defn- vb-str [{:keys [x y w h]}] (str x " " y " " w " " h))

(defn- client->svg
  "Client pixel coords -> SVG user-space coords via the element's screen CTM."
  [svg cx cy]
  (let [pt (.createSVGPoint svg)]
    (set! (.-x pt) cx)
    (set! (.-y pt) cy)
    (let [p (.matrixTransform pt (.inverse (.getScreenCTM svg)))]
      [(.-x p) (.-y p)])))

;;; node rendering: circle with attribute (upper/blue) + object (lower/black) marks

(defn- node-marks [cx cy r {:keys [attribute-concept? object-concept?]} highlighted?]
  [:g
   [:circle {:cx cx :cy cy :r r :fill "white"
             :stroke (if highlighted? "#e53e3e" "#333")
             :stroke-width (if highlighted? 3 1)}]
   (when attribute-concept?
     [:path {:d (str "M " (- cx r) " " cy " A " r " " r " 0 0 0 " (+ cx r) " " cy " Z")
             :fill "#2b6cb0"}])
   (when object-concept?
     [:path {:d (str "M " (- cx r) " " cy " A " r " " r " 0 0 1 " (+ cx r) " " cy " Z")
             :fill "#1a1a1a"}])
   [:circle {:cx cx :cy cy :r r :fill "none"
             :stroke (if highlighted? "#e53e3e" "#333")
             :stroke-width (if highlighted? 3 1)}]])

(defn- fmt-val [v]
  (cond (nil? v) nil
        (number? v) (if (integer? v) (str v) (.toFixed v 2))
        :else (str v)))

;;; the interactive canvas

(defn- diagram-svg [model highlight move-mode]
  (r/with-let [vb      (r/atom nil)
               fitted  (r/atom nil)
               svg-el  (r/atom nil)
               drag    (r/atom nil)]        ; {:id id :last [sx sy]} or {:pan true :last [sx sy]}
    (let [bounds (:bounds model)]
      (when (and bounds (not= @fitted bounds))
        (reset! fitted bounds)
        (reset! vb (fit-viewbox bounds)))
      (let [{:keys [max-y]} bounds
            by-id (into {} (map (juxt :id identity)) (:nodes model))
            sx    (fn [x] x)
            sy    (fn [y] (- max-y y))       ; Y-flip
            on-move
            (fn [ev]
              (when-let [d @drag]
                (let [[cx cy] (client->svg @svg-el (.-clientX ev) (.-clientY ev))
                      [px py] (:last d)]
                  (cond
                    (:pan d)
                    (do (swap! vb (fn [v] (-> v (update :x - (- cx px)) (update :y - (- cy py)))))
                        ;; recompute against the shifted box
                        (reset! drag (assoc d :last (client->svg @svg-el (.-clientX ev) (.-clientY ev)))))
                    (:id d)
                    (do (rf/dispatch-sync [::e/move-node (:id d) (- cx px) (- (- cy py))]) ; model dy = -(svg dy)
                        (reset! drag (assoc d :last [cx cy])))))))
            end-drag (fn [_] (reset! drag nil))]
        [:svg {:ref        #(reset! svg-el %)
               :view-box   (vb-str @vb)
               :preserve-aspect-ratio "xMidYMid meet"
               :style      {:width "100%" :height "100%" :touch-action "none"
                            :background "var(--diagram-bg, #fff)" :cursor "grab"}
               :on-wheel   (fn [ev]
                             (.preventDefault ev)
                             (let [[cx cy] (client->svg @svg-el (.-clientX ev) (.-clientY ev))
                                   f (if (pos? (.-deltaY ev)) 1.1 0.9)]
                               (swap! vb (fn [{:keys [x y w h]}]
                                           {:x (+ cx (* (- x cx) f)) :y (+ cy (* (- y cy) f))
                                            :w (* w f) :h (* h f)}))))
               :on-pointer-down (fn [ev]
                                  (.setPointerCapture (.-currentTarget ev) (.-pointerId ev))
                                  (reset! drag {:pan true
                                                :last (client->svg @svg-el (.-clientX ev) (.-clientY ev))}))
               :on-pointer-move on-move
               :on-pointer-up   end-drag
               :on-pointer-cancel end-drag}
         ;; edges
         (into [:g {:stroke "#888" :stroke-width 1.5}]
               (for [[a b] (:edges model)
                     :let [na (by-id a) nb (by-id b)]
                     :when (and na nb)]
                 ^{:key (str a "->" b)}
                 [:line {:x1 (sx (:x na)) :y1 (sy (:y na))
                         :x2 (sx (:x nb)) :y2 (sy (:y nb))}]))
         ;; nodes
         (into [:g]
               (for [n (:nodes model)
                     :let [cx (sx (:x n)) cy (sy (:y n))
                           v  (fmt-val (:valuation n))]]
                 ^{:key (:id n)}
                 [:g {:style {:cursor "pointer"}
                      :on-pointer-down
                      (fn [ev]
                        (.stopPropagation ev)
                        (.setPointerCapture (.-currentTarget ev) (.-pointerId ev))
                        (reset! drag {:id (:id n)
                                      :last (client->svg @svg-el (.-clientX ev) (.-clientY ev))}))
                      :on-context-menu
                      (fn [ev] (.preventDefault ev) (rf/dispatch [::e/toggle-highlight (:id n)]))}
                  [node-marks cx cy node-radius n (contains? highlight (:id n))]
                  (when-not (str/blank? (:attr-label n))
                    [:text {:x cx :y (- cy node-radius 3) :text-anchor "middle"
                            :font-size 11 :fill "#2b6cb0"} (:attr-label n)])
                  (when-not (str/blank? (:obj-label n))
                    [:text {:x cx :y (+ cy node-radius 12) :text-anchor "middle"
                            :font-size 11 :fill "#1a1a1a"} (:obj-label n)])
                  (when v
                    [:text {:x (+ cx node-radius 3) :y (+ cy 4)
                            :font-size 10 :fill "#c53030"} v])]))]))))

;;; controls + export

(defn- labelled-select [label value on-change options]
  [:label {:style {:display "flex" :flex-direction "column" :font-size "0.75rem"}}
   label
   [:select {:value value :on-change #(on-change (.. % -target -value))}
    (for [[v l] options] ^{:key v} [:option {:value v} l])]])

(defn- download! [filename mime text]
  (let [blob (js/Blob. #js [text] #js {:type mime})
        url  (js/URL.createObjectURL blob)
        a    (.createElement js/document "a")]
    (set! (.-href a) url)
    (set! (.-download a) filename)
    (.click a)
    (js/URL.revokeObjectURL url)))

(defn- export-svg! []
  (when-let [svg (.querySelector js/document "svg")]
    (download! "lattice.svg" "image/svg+xml"
               (.-outerHTML svg))))

(defn- export-json! [model]
  (download! "lattice.json" "application/json"
             (js/JSON.stringify (clj->js model) nil 2)))

(defn lattice-diagram []
  (let [model     @(rf/subscribe [::s/model])
        highlight @(rf/subscribe [::s/highlight])
        loading?  @(rf/subscribe [::s/loading?])
        error     @(rf/subscribe [::s/error])
        layout-name    @(rf/subscribe [::s/layout-name])
        valuation-name @(rf/subscribe [::s/valuation-name])
        move-mode      @(rf/subscribe [::s/move-mode])]
    [:div {:style {:display "flex" :flex-direction "column" :height "100%"}}
     [:div {:style {:display "flex" :gap "1rem" :align-items "flex-end"
                    :padding "0.5rem 1rem" :flex-wrap "wrap"
                    :border-bottom "1px solid #ddd"}}
      [:button {:on-click #(rf/dispatch [::e/set-view :editor])} "← Context"]
      [labelled-select "Layout" layout-name #(rf/dispatch [::e/set-layout-name %]) layouts]
      [labelled-select "Valuation" valuation-name #(rf/dispatch [::e/set-valuation-name %]) valuations]
      [labelled-select "Move mode" move-mode #(rf/dispatch [::e/set-move-mode %]) move-modes]
      [:div {:style {:display "flex" :gap "0.4rem"}}
       [:button {:on-click export-svg!} "Export SVG"]
       [:button {:on-click #(export-json! model)} "Export JSON"]]
      (when loading? [:span {:style {:color "#888"}} "Computing…"])
      (when error [:span {:style {:color "#c53030"}} (str "Error: " error)])]
     [:div {:style {:flex 1 :min-height 0}}
      (if (and model (seq (:nodes model)))
        ^{:key (:bounds model)} [diagram-svg model highlight move-mode]
        [:div {:style {:padding "2rem" :color "#888"}}
         (if loading? "Computing lattice…" "No diagram yet.")])]]))
