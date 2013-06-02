;; Copyright (c) Daniel Borchmann. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.contrib.gui.base
  (:import [java.awt event.WindowEvent])
  (:use [conexp.base :only (defvar-, defvar, defnk, illegal-state, ns-doc, unsupported-operation)]
        conexp.contrib.gui.util
        conexp.contrib.gui.repl
        conexp.contrib.gui.plugins
        [conexp.contrib.gui.plugins.base :only (load-plugin)]
        [conexp.contrib.gui.editors.contexts :only (context-editor)]
        [conexp.contrib.gui.editors.lattices :only (lattice-editor)]
        [conexp.contrib.gui.editors.code :only (code-editor)])
  (:use seesaw.core))

(ns-doc "Provides basic definitions for the standard conexp-clj GUI.")

;;; Conexp Main Frame

(defnk conexp-main-frame
  "Returns main frame for conexp standard gui."
  [:default-close-operation :dispose]
  (let [tabbed-pane  (tabbed-panel)
        main-frame   (frame :title "conexp-clj"
                            :on-close default-close-operation)
        content-pane (top-bottom-split tabbed-pane
                                       (make-repl main-frame)
                                       :divider-location 1.0
                                       :resize-weight 0.9
                                       :one-touch-expandable? true)]

    ;; set content
    (config! main-frame :content content-pane)

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
      )

    ;; Add Help menu at right position
    (add-menus main-frame [:separator
                           (menu :text "Help"
                                 :items
                                 [(menu-item :text "Online Documentation"
                                             :listen [:action (fn [_]
                                                                (with-swing-error-msg main-frame "Error"
                                                                  (let [desktop (java.awt.Desktop/getDesktop)]
                                                                    (when-not (.isSupported desktop java.awt.Desktop$Action/BROWSE)
                                                                      (unsupported-operation "Not supported"))
                                                                    (.browse (java.awt.Desktop/getDesktop)
                                                                             (java.net.URI. "http://github.com/exot/conexp-clj/wiki")))))])
                                  (menu-item :text "Report Bug"
                                             :enabled? false)
                                  :separator
                                  (menu-item :text "License"
                                             :listen [:action (fn [_]
                                                                (show!
                                                                 (dialog :content
                                                                         (scrollable
                                                                          (text :text (get-resource "res/epl-v10.txt")
                                                                                :multi-line? true
                                                                                :editable? false
                                                                                :caret-position 0)))))])
                                  (menu-item :text "About"
                                             :enabled? false)])])

    main-frame))

;;;

nil
