;; Copyright ⓒ the conexp-clj developers; all rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

;; This file has been written by Immanuel Albrecht, with modifications by DB

(ns conexp.contrib.gui.editors.context-editor.widgets
  (:import [javax.swing JSplitPane JScrollPane JOptionPane JToolBar JButton
                        JComponent]
           [java.awt Toolkit Dimension Insets FlowLayout]
           [java.awt.event ActionListener]
           [java.awt.datatransfer DataFlavor StringSelection])
  (:use [conexp.base :exclude (join)]
        conexp.contrib.gui.util)
  (:require [clojure.string :as string]))

;;; util

(defn class-to-keyword
  "Takes a class c and returns a corresponding keyword describing
  its class name."
  [c]
  (let [rv          string/reverse,
        classname   (str c),
        keywordname (rv (string/replace-first
                         (rv (string/replace-first classname #"class " ""))
                         #"\."
                         "/"))]
    (keyword keywordname)))

(defn keyword-class
  "Takes an object x and returns a corresponding keyword describing
  its class name."
  [x]
  (class-to-keyword (type x)))

(defn keyword-isa?
  "Returns true iff the keyword-class of obj isa? parent."
  [obj parent]
  (isa? (keyword-class obj) (class-to-keyword parent)))

(defmacro defwidget
  "Defines a widget. The resulting widget will have super-widgets,
  which must be a vector of valid widgets, as super-widgets and fields
  and data fields."
  [name super-widgets fields]
  `(do
     (defrecord ~name ~fields)
     ~@(for [sw super-widgets]
         `(derive (class-to-keyword ~name) (class-to-keyword ~sw)))))

;;; hookable

(defwidget hookable [] [hooks])

(defn make-hookable
  "Creates an empty hookable object."
  []
  (hookable. (ref {})))

(defn hookable?
  "Tests whether the given object is hookable."
  [obj]
  (keyword-isa? obj hookable))

(defn add-hook
  "Adds a hook to the hooksmap.

  Parameters:
    ohookable   _hookable object
    name        _key for the hook
    function    _a function that will be assigned to the hook"
  [ohookable name function]
  (assert (hookable? ohookable))
  (let [hooks (:hooks ohookable)]
    (dosync (alter hooks conj {name (list function "-no-doc-str-")}))))

(defn set-hook
  "Sets a hook in the hooksmap, throws if this hook doesn't exist."
  [ohookable name function]
  (assert (hookable? ohookable))
  (let [hooks (:hooks ohookable)]
    (if (contains? @hooks name)
      (dosync (alter hooks
                     (fn [h]
                       (let [doc-str (second (h name))
                             fun-str (list function doc-str)
                             new-h   (conj h {name fun-str})]
                         new-h))))
      (illegal-argument (str "set-hook " name " to " function " for "
                             ohookable " failed: hook undefined")))))

(defn call-hook
  "Calls a hook in the hooksmap, throws if this hook doesn't exist."
  [ohookable name & args]
  (assert (hookable? ohookable))
  (let [hooks (:hooks ohookable),
        hookmap @hooks]
    (if (contains? hookmap name)
      (apply (first (hookmap name)) args)
      (illegal-argument (str "call-hook " name " for "
                             ohookable " failed: hook undefined"
                             "\n\nmap:\n" hookmap)))))

(defn get-hook-function
  "Looks up a hook in the hooksmap and returns the associated function,
 throws if this hook doesn't exist."
  [ohookable name]
  (assert (hookable? ohookable))
  (let [hooks (:hooks ohookable),
        hookmap @hooks]
    (if (contains? hookmap name)
      (first (hookmap name))
      (illegal-argument (str "get-hook-function " name " for "
                             ohookable " failed: hook undefined"
                             "\n\nmap:\n" hookmap)))))

(defn doc-hook
  "Looks up a hook in the hooksmap and returns its doc-str,
   returns :not-found if this hook doesn't exist."
  [ohookable name]
  (assert (hookable? ohookable))
  (let [hooks (:hooks ohookable),
        hookmap @hooks]
    (if (contains? hookmap name)
      (second (hookmap name))
      :not-found)))

;;; one-to-many abstraction

(defwidget one-to-many [] [one many])

(defn make-one-to-many
  "Creates a one-to-many object.
   Parameters:
     one     _the single point object
     & many  _optional hash-set of connected objects"
  [one & many]
  (one-to-many. one (if (nil? many) #{} (hash-set many))))

(defn add
  "Returns a new one-to-many object that consists of otm
   and has some more elements."
  [otm & els]
  (assert (keyword-isa? otm one-to-many))
  (one-to-many. (:one otm) (apply conj (:many otm) els)))

(defn del
  "Returns a new one-to-many object that consists of otm
   and has some less elements."
  [otm & els]
  (assert (keyword-isa? otm one-to-many))
  (one-to-many. (:one otm) (apply disj (:many otm) els)))

(defn call-one
  "Calls the given function with the one-part of the given one-many relation
  as first parameter."
  [otm f & parms]
  (assert (keyword-isa? otm one-to-many))
  (apply f (:one otm) parms))

(defn call-many
  "Calls the given function several times with each many-part of the given
  one-many relation as first parameter for one."
  [otm f & parms]
  (assert (keyword-isa? otm one-to-many))
  (doseq [m (:many otm)] (apply f m parms)))

(defn call-first
  "Calls the given function with the given parameter for the first of
  the many."
  [otm f & parms]
  (assert (keyword-isa? otm one-to-many))
  (let [m (:many otm)]
    (when-not (empty? m)
      (apply f (first m) parms))))

;;; clipboard functions

(defn-swing get-clipboard-contents
  "Returns the contents of the system clipboard"
  []
  (let [toolkit      (Toolkit/getDefaultToolkit),
        clipboard    (.getSystemClipboard toolkit),
        transferable (.getContents clipboard nil)]
    (if (.isDataFlavorSupported transferable DataFlavor/stringFlavor)
      (.getTransferData transferable DataFlavor/stringFlavor)
      nil)))

(defn-swing set-clipboard-contents
  "Set the contents of the system clipboard to the given string
  contents."
  [contents]
  (let [toolkit   (Toolkit/getDefaultToolkit),
        clipboard (.getSystemClipboard toolkit),
        data      (StringSelection. (str contents))]
    (.setContents clipboard data nil)))


;;; managed/unmanaged interop

(defwidget widget [] [widget])
(defwidget control [widget] [widget control])

(defn- managed-by-conexp-gui-editors-util?
  "Returns true if the object given as parameter is managed by the
   conexp.contrib.gui.editors.util module."
  [thing]
  (or (and (map? thing)
           (contains? thing :managed-by-conexp-gui-editors-util))
      (keyword-isa? thing widget)))

(defn-swing ^JComponent get-widget
  "Returns the appropriate java root widget for managed java code or
   just the input parameter for other objects."
  [obj]
  (if (managed-by-conexp-gui-editors-util? obj)
    (:widget obj)
    obj))

(defn-swing ^JComponent get-control
  "Returns the appropriate java control widget for managed java code or
   just the input parameter for other objects."
  [obj]
  (if (or (keyword-isa? obj control)
          (managed-by-conexp-gui-editors-util? obj))
    (:control obj)
    obj))

(defn-swing get-size
  "Returns the size of the given widget."
  [obj]
  (assert (keyword-isa? obj widget))
  (bean (.getSize ^JComponent (get-widget obj))))

(defn-swing set-size
  "Sets the size of the given widget obj."
  [obj width height]
  (assert (keyword-isa? obj widget))
  (.setSize (get-widget obj)
            (Dimension. width height)))

(defn-swing set-width
  "Sets the width of the given widget obj."
  [obj width]
  (assert (keyword-isa? obj widget))
  (let [height (:height (get-size obj))]
    (set-size obj width height)))

(defn-swing set-height
  "Sets the height of the given widget."
  [obj height]
  (assert (keyword-isa? obj widget))
  (let [width (:width (get-size obj))]
    (set-size obj width height)))

(defn-swing add-control-mouse-listener
  "Adds a mouse-listener proxy to the given control."
  [control proxy]
  (.addMouseListener (get-control control) proxy)
  (.addMouseMotionListener (get-control control) proxy))

;;;

nil
