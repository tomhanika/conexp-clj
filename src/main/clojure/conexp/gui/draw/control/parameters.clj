(ns conexp.gui.draw.control.parameters
  (:require [conexp.fca.metrics :refer :all]
            [conexp.fca.lattices :refer [extract-context-from-bv]]
            [conexp.gui.draw.control.util :refer :all]
            [conexp.gui.draw.nodes-and-connections :refer :all]
            [conexp.gui.draw.scene-layouts :refer :all]
            [conexp.gui.draw.scenes :refer :all]
            [conexp.layouts :refer :all]
            [conexp.layouts.base :refer :all]
            [conexp.layouts.layered :refer :all]
            [conexp.layouts.util :refer :all]
            [conexp.math.util :refer :all]
            [seesaw.core :refer [listen]])
  (:import [javax.swing JButton JComboBox JTextField]))

;;;

(declare single-move-mode, ideal-move-mode, filter-move-mode,
         chain-move-mode, infimum-additive-move-mode,
         supremum-additive-move-mode, no-valuation-mode,
         count-int-valuation-mode, count-ext-valuation-mode,
         modularity-valuation-mode, distributivity-valuation-mode,
         separation-index-mode, support-valuation-mode,
         stability-valuation-mode, probability-valuation-mode)

;;;

(defn change-parameters
  "Installs parameter list which influences lattice drawing."
  [_ scn buttons]


  ;; move mode
  (make-label buttons "Move-Modes")
  (let [move-modes {"single" (single-move-mode),
                    "ideal"  (ideal-move-mode),
                    "filter" (filter-move-mode),
                    "chain"  (chain-move-mode),
                    "inf"    (infimum-additive-move-mode),
                    "sup"    (supremum-additive-move-mode)}
        ^JComboBox combo-box (make-combo-box buttons (keys move-modes)),
        current-move-mode (atom (move-modes "single"))]
    (add-scene-callback scn :move-drag
                        (fn [node dx dy]
                          (@current-move-mode node dx dy)))
    (listen combo-box :action
            (fn [evt]
              (let [selected (.getSelectedItem
                              ^JComboBox (.getSource ^java.awt.event.ActionEvent evt)),
                    move-mode (get move-modes selected)]
                (reset! current-move-mode move-mode)))))

  (make-padding buttons)
  (make-separator buttons)
  (make-padding buttons)
  
  ;; labels
  (make-label buttons "Labels")
  (let [^JButton label-toggler (make-button buttons "off")]
    (show-labels scn false)
    (listen label-toggler :action
            (fn [_]
              (if (= "on" (.getText label-toggler))
                (do
                  (show-labels scn false)
                  (.setText label-toggler "off"))
                (do
                  (show-labels scn true)
                  (.setText label-toggler "on")))
              (redraw-scene scn))))

  (make-label buttons "Valuations")
  (let [valuation-modes {"none" 'no-valuation-mode,
                         "count-ints" 'count-int-valuation-mode,
                         "count-exts" 'count-ext-valuation-mode,
                         "distributivity" 'distributivity-valuation-mode,
                         "modularity" 'modularity-valuation-mode,
                         "separation-index" 'separation-index-mode
                         "support" 'support-valuation-mode,
                         "stability" 'stability-valuation-mode,
                         "probability" 'probability-valuation-mode}
        ^JComboBox combo-box (make-combo-box buttons (keys valuation-modes)),
        current-valuation-mode (atom (valuation-modes "none"))]
    (listen combo-box :action
            (fn [evt]
              (let [selected (.getSelectedItem
                              ^JComboBox (.getSource ^java.awt.event.ActionEvent evt)),
                    valuation-mode (get valuation-modes selected)]
                (reset! current-valuation-mode valuation-mode))
              (if (not (nil? @current-valuation-mode))
                (let [layout (get-layout-from-scene scn)
                      thelattice (lattice layout)
                      thens (find-ns 'conexp.gui.draw.control.parameters)
                      val-fn (ns-resolve thens @current-valuation-mode)
                      newlayout (update-valuations layout (partial val-fn thelattice))]
                  (update-valuations-of-scene scn newlayout)
                  (fit-scene-to-layout scn newlayout))))))
  
  (make-padding buttons)
  (make-separator buttons)
  (make-padding buttons)

  ;; node radius
  (let [^JTextField
        node-radius (make-labeled-text-field buttons
                                             "radius"
                                             (str default-node-radius))]
    (add-scene-callback scn :image-changed
                        (fn []
                          (let [new-radius (Double/parseDouble (.getText node-radius)),
                                length     (min (scene-height scn) (scene-width scn))]
                            (do-nodes [n scn]
                              (set-node-radius! n (/ (* length new-radius) 400))))))
    (listen node-radius :action
            (fn [_] (call-scene-hook scn :image-changed))))

  (make-padding buttons)
  (make-separator buttons)
  (make-padding buttons)
  

  ;; layouts
  (let [layouts {"standard"     standard-layout,
                 "inf-add"      inf-additive-layout,
                 "simple-layer" simple-layered-layout,
                 "as-chain"     as-chain},
        ^JButton fit (make-button buttons "Fit Screen"),
        ^JComboBox combo-box (make-combo-box buttons (keys layouts))]
    (listen fit :action
            (fn [_]
              (fit-scene-to-layout scn)))
    (listen combo-box :action
            (fn [evt]
              (let [selected  (.getSelectedItem ^JComboBox (.getSource ^java.awt.event.ActionEvent evt)),
                    layout-fn (get layouts selected),
                    layout    (scale-layout [0.0 0.0]
                                            [100.0 100.0]
                                            (layout-fn (lattice (get-layout-from-scene scn))))]
                (update-layout-of-scene scn layout)
                (fit-scene-to-layout scn layout)))))


  (make-padding buttons)
  (make-separator buttons)
  (make-padding buttons)
  
    ;;discretize
  (let [^JTextField  grid-size (make-labeled-text-field buttons
                                                        "grid"
                                                        "0x0")]
    (add-scene-callback scn :fit-grid
                        (fn []
                          (let [grid (clojure.string/split 
                                       (.getText grid-size) #"x"),
                                layout (discretize-layout-values 
                                         (get-layout-from-scene scn)
                                         (- (Integer/parseInt (first grid)) 1)
                                         (- (Integer/parseInt (second grid)) 
                                            1))]
                            (update-layout-of-scene scn layout)
                            (fit-scene-to-layout scn))))
    (add-scene-callback scn :update-grid 
                        (fn []
                          (let [positions (vals (positions 
                                                  (get-layout-from-scene scn)))
                                y-pos     (sort (map second positions))
                                y-size    (+ (- (last y-pos) (first y-pos)) 1)
                                x-pos     (sort (map first positions))
                                x-size    (+ (- (last x-pos) (first x-pos)) 1)]
                            (.setText grid-size (str x-size "x" y-size))
                            (call-scene-hook scn :draw-grid false))))
    (add-scene-callback scn :estimate-grid
                        (fn []
                          (let [positions (vals (positions 
                                                  (get-layout-from-scene scn)))
                                y-vals    (distinct (map second positions))
                                x-vals    (distinct (map first positions))]
                            (do
                              (.setText 
                                grid-size 
                                (str (count x-vals) "x" (count y-vals)))
                              (call-scene-hook scn :fit-grid)))))
    

    (listen grid-size :action
      (fn [_]
        (do 
          (call-scene-hook scn :fit-grid)
          (call-scene-hook scn :draw-grid false))))
    (call-scene-hook scn :estimate-grid)
    (make-padding buttons)

    (let [^JButton fit-grid (make-button buttons "Fit Grid")]
      (listen fit-grid :action
        (fn [_]
          (do 
            (call-scene-hook scn :fit-grid)
            (call-scene-hook scn :draw-grid false)))))
    (make-padding buttons)

    (let [^JButton est-grid (make-button buttons "Auto Grid")]
      (listen est-grid :action
        (fn [_]
          (call-scene-hook scn :estimate-grid))))
    (make-padding buttons)

    (let [^JButton grid-toggler (make-button buttons "Toggled Off")]
      (add-scene-callback scn :draw-grid
        (fn [toggle?]
          (do
          (fit-scene-to-layout 
            (set-layout-of-scene 
              scn 
              (get-layout-from-scene scn)))
            (if (= (if toggle? "Toggled On" "Toggled Off") 
                   (.getText grid-toggler))
              (.setText grid-toggler "Toggled Off")
              (do
                (let [positions (vals (positions (get-layout-from-scene scn)))
                      x-pos     (sort (distinct (map first positions)))
                      x-grid    (apply max (map #(- %2 %1) 
                                                (drop 1 x-pos) 
                                                (drop-last 1 x-pos)))
                      y-pos     (sort (distinct (map second positions)))
                      y-grid    (apply max (map #(- %2 %1) 
                                                (drop 1 y-pos) 
                                                (drop-last 1 y-pos)))]
                  (draw-grid scn 
                             (first positions)
                             [x-grid y-grid]))
                (.setText grid-toggler "Toggle")))
            (redraw-scene scn))))
      (listen grid-toggler :action
        (fn [_]
          (call-scene-hook scn :draw-grid true)))))
  (make-padding buttons)
  (make-separator buttons)
  (make-padding buttons)
  

  nil)



;;; Move modes

(defn- single-move-mode
  "Moves the single node only."
  []
  (fn [node dx dy]
    nil))

(defn- neighbor-move-mode
  "Moves nodes neighbored to node by [dx dy]."
  [neighbors]
  (fn [node dx dy]
    (doseq [n (neighbors node)]
      (move-node-by n dx dy))))

(defn- ideal-move-mode
  "Moves all nodes below the current node."
  []
  (neighbor-move-mode (memoize all-nodes-below)))

(defn- filter-move-mode
  "Moves all nodes above the current node."
  []
  (neighbor-move-mode (memoize all-nodes-above)))

(defn- chain-move-mode
  "Combined ideal and filter move mode."
  []
  (let [ideal (ideal-move-mode),
        filter (filter-move-mode)]
    (fn [node dx dy]
      (ideal node dx dy)
      (filter node dx dy))))

(defn- additive-move-mode
  "Abstract move mode for moving nodes according to additive
  influence."
  [influenced-nodes]
  (fn [node dx dy]
    (doseq [[n weight] (influenced-nodes node)]
      (with-doubles [dx dy weight]
        (move-node-by n (* dx weight) (* dy weight))))))

(defn- infimum-additive-move-mode
  "Moves all nodes infimum-additively with node."
  []
  (additive-move-mode (memoize all-inf-add-influenced-nodes)))

(defn- supremum-additive-move-mode
  "Moves all nodes supremum-additively with node."
  []
  (additive-move-mode (memoize all-sup-add-influenced-nodes)))

(defn- count-int-valuation-mode
  [_ c]
  (count (second c)))

(defn- count-ext-valuation-mode
  [_ c]
  (count (first c)))

(defn- modularity-valuation-mode
  [lat c]
  (float (elements-modularity lat c)))

(defn- distributivity-valuation-mode
  [lat c]
  (float (elements-distributivity lat c)))

(defn- separation-index-mode
  [lat c]
  (let [ctx (extract-context-from-bv lat)]
    (float (separation-index ctx c))))

(defn- support-valuation-mode
  [lat c]
  (float (/ (count (first c))
            (apply max (map (comp count first) (.base-set lat))))))

(defn- stability-valuation-mode
  [lat c]
  (let [ctx (extract-context-from-bv lat)]
    (float (concept-stability ctx c))))

(defn- probability-valuation-mode
  [lat c]
  (let [ctx (extract-context-from-bv lat)]
    (binding [*fast-computation* true]
      (if (empty? (second c)) 1
        (float (concept-probability ctx c))))))

(defn- no-valuation-mode
  [_ concept]
  "")
;;;

nil
