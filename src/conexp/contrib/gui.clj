;; Copyright (c) Daniel Borchmann. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.contrib.gui
  (:import [javax.swing UIManager JFrame])
  (:use conexp.base
	conexp.contrib.gui.base)
  (:use clojure.contrib.swing-utils))

(update-ns-meta! conexp.contrib.gui
  :doc "Provides standard gui for conexp-clj.")

;;;


(defn gui
  "Starts the standard gui for conexp-clj. args may be a sequence of
  parameters given by keywords and values."
  [& args]
  (do-swing
   (. UIManager (setLookAndFeel (. UIManager (getSystemLookAndFeelClassName))))
   (let [#^JFrame frame (apply conexp-main-frame args)]
     (.setVisible frame true))))

;;;

nil
