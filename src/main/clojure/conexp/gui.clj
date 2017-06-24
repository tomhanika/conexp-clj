(ns conexp.contrib.gui
  "Provides standard gui for conexp-clj."
  (:require [conexp.gui.base :refer :all]))

;;;

(defn gui
  "Starts the standard gui for conexp-clj. args may be a sequence of
  parameters given by keywords and values."
  [& args]
  (native!)
  (-> (apply conexp-main-frame args)
      show!))

;;;

nil
