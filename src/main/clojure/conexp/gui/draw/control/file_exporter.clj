(ns conexp.gui.draw.control.file-exporter
  (:require [conexp.gui.draw.control.util :refer :all]
            [conexp.gui.draw.scenes :refer :all]
            [conexp.gui.util :refer :all])
  (:import java.io.File
           [javax.swing JButton JFileChooser]
           javax.swing.filechooser.FileNameExtensionFilter))

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
