;; Copyright (c) Daniel Borchmann. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.gui
  (:import [javax.swing UIManager JFrame])
  (:use conexp.base
	conexp.gui.base)
  (:use clojure.contrib.swing-utils))

(update-ns-meta! conexp.gui
  :doc "Provides standard gui for conexp-clj.")

;;;

(defn gui
  "Starts the standard gui for conexp-clj."
  []
  (do-swing
   (. UIManager (setLookAndFeel (. UIManager (getSystemLookAndFeelClassName))))
   (let [#^JFrame frame (conexp-main-frame)]
     (.setVisible frame true))))

;;;

nil
