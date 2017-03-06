;; Copyright ⓒ the conexp-clj developers; all rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.contrib.gui.base
  "Provides basic definitions for the standard conexp-clj GUI."
  (:import [java.awt event.WindowEvent])
  (:use [conexp.base :only (illegal-state
                            unsupported-operation
                            conexp-version)]
        conexp.contrib.gui.util
        conexp.contrib.gui.plugins
        [conexp.contrib.gui.plugins.base :only (load-plugin)]
        [conexp.contrib.gui.editors.contexts :only (context-editor)]
        [conexp.contrib.gui.editors.lattices :only (lattice-editor)]
        [conexp.contrib.gui.editors.code :only (code-editor)])
  (:require [clojure.java.io :as io])
  (:use seesaw.core))

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
