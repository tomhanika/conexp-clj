(ns conexp.gui.base
  "Provides basic definitions for the standard conexp-clj GUI."
  (:require [clojure.java.io :as io]
            [conexp.base :refer :all]
            [conexp.gui.editors.code :refer :all]
            [conexp.gui.editors.contexts :refer :all]
            [conexp.gui.editors.lattices :refer :all]
            [conexp.gui.plugins :refer :all]
            [conexp.gui.plugins.base :refer :all]
            [conexp.gui.util :refer :all])
  (:import java.awt.event.WindowEvent))

;;; Helper Functions

(defn- show-license []
  (show!
   (dialog :content (scrollable
                     (editor-pane :content-type "text/html"
                                  :text (slurp
                                         (io/resource "res/epl-v10.html"))
                                  :editable? false
                                  :caret-position 0))
           :size [800 :by 500]
           :resizable? false)))

(defn- show-about []
  (show!
   (dialog :content (vertical-panel :items [(label :text (str "conexp-clj " (conexp-version))
                                                   :size [300 :by 16]
                                                   :halign :center)
                                            (label :text "Graphical User Interface"
                                                   :size [300 :by 16]
                                                   :halign :center)
                                            (label :text "ⓒ 2013, 2014 Daniel Borchmann"
                                                   :size [300 :by 30]
                                                   :halign :center
                                                   :valign :bottom)])
           :resizable? false
           :size [300 :by 121])))

;;; Conexp Main Frame

(defn conexp-main-frame
  "Returns main frame for conexp standard gui."
  [& {:keys [default-close-operation]
      :or   {default-close-operation :dispose}}]
  (let [tabbed-pane  (tabbed-panel)
        main-frame   (frame :title "conexp-clj"
                            :size [640 :by 480]
                            :on-close default-close-operation)]

    ;; set content
    (config! main-frame :content tabbed-pane)

    ;; Main menu first
    (add-menus main-frame
               [(menu :text "Main"
                      :items
                      [(menu-item :text "Save window configuration to file"
                                  :enabled? false)
                       (menu-item :text "Load window configuration from file"
                                  :enabled? false)
                       :separator
                       (menu-item :text "Quit"
                                  :listen [:action
                                           (fn [_]
                                             (.processWindowEvent
                                              main-frame
                                              (WindowEvent. main-frame
                                                            WindowEvent/WINDOW_CLOSING)))])])])

    ;; Add plugins (may add new menus)
    (add-plugin-manager main-frame)
    (let [pm (get-plugin-manager main-frame)]
      (load-plugin pm context-editor)
      (load-plugin pm lattice-editor)
      (load-plugin pm code-editor)
      nil)

    ;; Add Help menu at right position
    (add-menus main-frame
               [:separator
                (menu :text "Help"
                      :items
                      [(menu-item :text "Online Documentation"
                                  :listen [:action
                                           (fn [_]
                                             (open-link-in-external-browser
                                              main-frame
                                              "http://github.com/exot/conexp-clj/wiki/"))])
                       (menu-item :text "Report Bug"
                                  :enabled? false)
                       :separator
                       (menu-item :text "License"
                                  :listen [:action (fn [_]
                                                     (show-license))])
                       (menu-item :text "About"
                                  :listen [:action (fn [_]
                                                     (show-about))])])])

    main-frame))

;;;

nil
