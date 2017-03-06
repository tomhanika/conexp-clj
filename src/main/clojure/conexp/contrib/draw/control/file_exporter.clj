;; Copyright ⓒ the conexp-clj developers; all rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.contrib.draw.control.file-exporter
  (:use conexp.contrib.draw.scenes
        conexp.contrib.draw.control.util
        conexp.contrib.gui.util)
  (:use seesaw.core)
  (:import [javax.swing JButton JFileChooser]
           [javax.swing.filechooser FileNameExtensionFilter]
           [java.io File]))

;;;

(defn- get-file-extension
  "Returns file extension of given file."
  [^File file]
  (let [name (.getName file),
        idx  (.lastIndexOf name ".")]
    (if-not (= -1 idx)
      (.toLowerCase (.substring name (+ 1 idx)))
      nil)))

(defn export-as-file
  "Installs a file exporter."
  [frame scn buttons]
  (let [^JButton save-button (make-button buttons "Export to File"),
        ^JFileChooser fc (JFileChooser.),
        jpg-filter (FileNameExtensionFilter. "JPEG Files" (into-array ["jpg" "jpeg"])),
        gif-filter (FileNameExtensionFilter. "GIF Files"  (into-array ["gif"])),
        png-filter (FileNameExtensionFilter. "PNG Files"  (into-array ["png"]))]
    (doto fc
      (.addChoosableFileFilter jpg-filter)
      (.addChoosableFileFilter gif-filter)
      (.addChoosableFileFilter png-filter))
    (listen save-button :action
            (fn [_]
              (let [retVal (.showSaveDialog fc frame)]
                (when (= retVal JFileChooser/APPROVE_OPTION)
                  (let [^File file (.getSelectedFile fc)]
                    (with-swing-error-msg frame "Error while saving"
                      (save-image scn file (get-file-extension file)))))))))
  nil)

;;;

nil
