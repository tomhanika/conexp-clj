(ns conexp.gui.draw.control.file-exporter
  (:require [conexp.gui.draw.control.util :refer :all]
            [conexp.gui.draw.scenes :refer :all]
            [conexp.gui.util :refer :all]
            [seesaw.core :refer [listen]]
            [conexp.gui.draw.scene-layouts :refer :all]
            [conexp.gui.draw.scenes :refer :all]
            [conexp.io.layouts :refer :all])
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
        tikz-filter (FileNameExtensionFilter. "TIKZ Files" (into-array ["tikz"]))
        json-filter (FileNameExtensionFilter. "JSON Files" (into-array ["json"]))
        fca-filter (FileNameExtensionFilter. "FCA Files" (into-array ["fca"]))
        layout-filter (FileNameExtensionFilter. "Layout Files" (into-array ["layout"]))
        png-filter (FileNameExtensionFilter. "PNG Files"  (into-array ["png"]))]
    (doto fc
      (.addChoosableFileFilter jpg-filter)
      (.addChoosableFileFilter gif-filter)
      (.addChoosableFileFilter tikz-filter)
      (.addChoosableFileFilter json-filter)
      (.addChoosableFileFilter fca-filter)
      (.addChoosableFileFilter layout-filter)
      (.addChoosableFileFilter png-filter))
    (listen save-button :action
            (fn [_]
              (let [retVal (.showSaveDialog fc frame)]
                (when (= retVal JFileChooser/APPROVE_OPTION)
                  (let [^File file (.getSelectedFile fc)]
                    (with-swing-error-msg frame "Error while saving"
                      (case (get-file-extension file)
                        "tikz" (write-layout :tikz (get-layout-from-scene scn) (.getAbsolutePath file))
                        "json" (write-layout :json (get-layout-from-scene scn) (.getAbsolutePath file))
                        "fca" (write-layout :fca-style (get-layout-from-scene scn) (.getAbsolutePath file))
                        "layout" (write-layout :simple (get-layout-from-scene scn) (.getAbsolutePath file))
                        (save-image scn file (get-file-extension file))))))))))
  nil)

(defn import-from-file
  "Installs a file exporter."
  [frame scn buttons]
  (let [^JButton load-button (make-button buttons "Import from File"),
        ^JFileChooser fc (JFileChooser.),
        layout-filter (FileNameExtensionFilter. "Layout Files" (into-array ["layout"]))]
    (doto fc
      (.addChoosableFileFilter layout-filter))
    (listen load-button :action
            (fn [_]
              (let [retVal (.showSaveDialog fc frame)]
                (when (= retVal JFileChooser/APPROVE_OPTION)
                  (let [^File file (.getSelectedFile fc)]
                    (with-swing-error-msg frame "Error while saving"
                      (let [layout (read-layout (.getAbsolutePath file))]
                        (update-layout-of-scene scn layout)
                        (fit-scene-to-layout scn layout)))))))))
  nil)

;;;

nil
