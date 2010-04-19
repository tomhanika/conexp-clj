;; Copyright (c) Daniel Borchmann. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.contrib.gui.editors.context-editor.util
  (:import [javax.swing JSplitPane JRootPane JTextArea JTable JList JTree
             JScrollPane JPanel JButton]
    [java.awt GridLayout BorderLayout Dimension]
    [javax.swing.tree DefaultTreeModel DefaultMutableTreeNode]
    [java.util Vector]
    [javax.swing.table DefaultTableModel]
    )
  (:use conexp.contrib.gui.plugins.base
    conexp.contrib.gui.util
    conexp.util
    conexp.util.hookable
    conexp.util.one-to-many
    conexp.util.multimethods
    clojure.contrib.swing-utils
    conexp.contrib.gui.editors.util
    conexp.fca
    [clojure.contrib.string :only (join split-lines split)]))

(def- *poly-ns* 'conexp.polymorphisms)

;;
;;
;; Helper functions
;;
;;
;;
;;

(defn string-to-cross
  "Takes a string and decides, whether it should be a cross or not

  Parameters:
   s     _string
  Returns true if there should be a cross"
  [s]
  (let [trimmed (.trim s)]
    (if (re-matches #"0*[,.]?0*" trimmed) false true)))

(defn map-to-unique-strings
  "Takes a sequence of (unique) keys and returns a map
   that maps each unique key to a unique string

  Parameters:
   keys    _sequence of (unique) keys"
  [keys]
  (loop [ k (set keys)        
          m {} 
          taken #{}
          nbr 0]
    (if (empty? k) m
      (let [ k1 (first k)
             k1-as-str (if (< 0 nbr)
                         (join "-" [(str k1) (str nbr)])
                         (str k1))]
        (if (contains? taken k1-as-str)
          (recur k m taken (+ 1 nbr))
          (recur (disj k k1) (conj m {k1 k1-as-str})
            (conj taken k1-as-str) 0))))))

(defn req-unique-string
  "Takes a sequence of keys and a new requested key and
  returns a new key that will not conflict with any of the
  keys in the sequence.

  Parameters:
    old-keys  _sequence of keys
    req-key   _requested new key"
  [old-keys req-key]
  (let [keys (set old-keys)]
    (loop [ nbr 0 ]
      (let [new-key (if (< 0 nbr)
                      (join "-" [(str req-key) (str nbr)])
                      (str req-key))]
        (if (contains? keys new-key)
          (recur (+ 1 nbr))
          new-key)))))

(defn switch-bipartit-auto
  "Takes a bipartit auto map and removes the old-key and old-value
   associations and associates new-key with new-value and new-value
   with new-key, then returns the new map

   Parameters:
     m         _bipartit auto map   
     old-key   _old key
     old-value _old value
     new-key   _new key
     new-value _new value"
  [m old-key old-value new-key new-value]
  (let [m-without-old (dissoc m old-key old-value)
        m-with-new (assoc m-without-old new-key new-value new-value new-key)]
    m-with-new))


;;
;;
;; Context editor control
;;
;;
;;
;;

(defrecord context-editor-widget [widget table toolbar e-ctx])
(derive* ::context-editor-widget :conexp.contrib.gui.editors.util/widget)

(declare make-editable-context editable-context? add-widget get-context)

(defn-swing make-context-editor-widget
  "Creates a control for editing contexts.

  Parameters:
     & setup     _an optional number of vectors that may contain additional
                  tweaks that are called after widget creation"
  [ & setup]
  (let [ root (JRootPane.)
         table (make-table-control [set-row-count 1] [set-column-count 1])
         toolbar (make-toolbar-control :vert
                   [add-button 
                     (make-button "Copy" 
                       (get-ui-icon "OptionPane.informationIcon")
                       [set-handler (fn [] (copy-to-clipboard table))] )]
                   [add-button
                     (make-button "Paste" 
                       (get-ui-icon "OptionPane.informationIcon")
                       [set-handler (fn [] (paste-from-clipboard table))] ) ])
         ectx (ref (make-editable-context))
         e-ctx @ectx
         widget (context-editor-widget. root table toolbar ectx) ]
    (.. root getContentPane 
      (add (get-widget toolbar) BorderLayout/LINE_START))
    (.. root getContentPane
      (add (get-widget table)))
    (apply-exprs widget setup)
    (dosync-wait (commute (:widgets e-ctx) add widget))
    (add-widget e-ctx widget)
    widget))

(inherit-multimethod get-ectx ::context-editor-widget
  "Returns the editable-context that is currently associated with the
   context-editor-widget.

  Parameters:
    widget  _context-editor-widget")

(defmethod get-ectx ::context-editor-widget
  [widget] @(:e-ctx widget))

(inherit-multimethod set-ectx ::context-editor-widget
  "Sets the editable-context that is currently associated with the
   context-editor-widget to the second parameter.

  Parameters:
    widget  _context-editor-widget
    e-ctx   _new editable-context")

(defmethod set-ectx ::context-editor-widget
  [widget e-ctx] (dosync-wait (ref-set (:e-ctx widget) e-ctx)))


(inherit-multimethod get-table ::context-editor-widget
  "Returns the table-control that is associated with the context-editor-widget.

  Parameters:
    widget  _context-editor-widget")

(defmethod get-table ::context-editor-widget
  [widget]
  (:table widget))

(inherit-multimethod get-table :conexp.contrib.gui.editors.util/table-control
  "Identity for table controls")

(defmethod get-table :conexp.contrib.gui.editors.util/table-control
  [x] x)


(declare change-attribute-name change-object-name change-incidence-cross)

(defn- ectx-cell-value-hook 
  [ectx row column contents]
  (let [ good-value
         (cond (= column row 0) "⇊objects⇊"
           (= row 0) 
           (let [attrib-cols @(:attr-cols ectx)
                  current-name (attrib-cols column)] 
             (if (= contents current-name) current-name
               (change-attribute-name ectx column contents)))
           (= column 0)
           (let [object-rows @(:obj-rows ectx)
                  current-name (object-rows row)]
             (if (= contents current-name) current-name
               (change-object-name ectx row contents)))
           true ;else
           (let [ cross (string-to-cross contents)
                  obj-name (@(:obj-rows ectx) row)
                  attr-name (@(:attr-cols ectx) column)
                  fca-ctx (get-context ectx)
                  inc (incidence fca-ctx)
                  current-state (contains? inc [obj-name attr-name])]
             (if (not= current-state cross)
               (change-incidence-cross ectx obj-name attr-name cross))
             (if cross "X" " "))
           )]
    good-value))

(defn- ectx-extend-rows-hook 
  [ectx rows]
  (let [ current-rows  (count (objects (get-context ectx))) ]
    (call-many @(:widgets ectx)
      (fn [w] (set-row-count (get-table w) rows)))
    (doseq [r (range (+ 1 current-rows) rows)]
      (call-first @(:widgets ectx)
        (fn [w] (set-value-at-index (get-table w) r 0 "new object"))))))

(defn- ectx-extend-columns-hook 
  [ectx cols]
  (let [ current-cols (count (attributes (get-context ectx))) ]
    (call-many @(:widgets ectx)
      (fn [w] (set-column-count (get-table w) cols)))
    (doseq [c (range (+ 1 current-cols) cols)]
      (call-first @(:widgets ectx)
        (fn [w] (set-value-at-index (get-table w) 0 c "new attribute"))))))

;;
;;
;; Editable contexts
;;
;;
;;

(defrecord editable-context [context attr-cols obj-rows widgets])


(defn editable-context?
  "Tests whether the argument is an editable context."
  [ctx?]
  (isa?* (type ctx?) ::editable-context))


(inherit-multimethod get-context ::editable-context
  "Returns the fca-context object that is currently bound to
   the editable context.

   Parameters:
    ctx   _editable-context object")

(defmethod
  get-context ::editable-context
  [ctx]
  (deref (:context ctx)))

(inherit-multimethod get-context conexp.fca.contexts.Context
  "Identity on fca-context.")

(defmethod
  get-context conexp.fca.contexts.Context
  [x] x)

(defn make-context-compatible 
  "Takes a context object and returns a compatible fca-Context.

  Parameters:
    ctx   _object for which get-context is defined."
  [ctx]
  (let [ ctx (get-context ctx)
         obj (objects ctx)
         att (attributes ctx)
         inc (incidence ctx)
         obj-map (map-to-unique-strings obj)
         att-map (map-to-unique-strings att)
         comp-obj (map obj-map obj)
         comp-att (map att-map att)
         comp-inc (map (fn [x] [(obj-map (first x)) (att-map (second x))]) inc)]
    (make-context comp-obj comp-att comp-inc)))

(defn make-editable-context
  "Takes an optional context as input and returns an appropriate 
  editable-context structure that is bound to a compatible version of the 
  input context.

  Parameters:
   [context-in]   _input context"
  ([] (make-editable-context (make-context '() '() [])))
  ([context-in]
    (let [ ctx (make-context-compatible context-in)
           att (attributes ctx)
           obj (objects ctx)
           self (promise)
           e-ctx (editable-context. (ref ctx) 
                   (ref (conj (zipmap att (range 1 (+ 1 (count att))))
                          (zipmap (range 1 (+ 1 (count att))) att)))
                   (ref (conj (zipmap obj (range 1 (+ 1 (count obj))))
                          (zipmap (range 1 (+ 1 (count obj))) obj)))
                   (ref (make-one-to-many self))) ]
      (deliver self e-ctx)
      e-ctx)))

(inherit-multimethod add-widget ::editable-context
  "Adds a context-editor-widget to an editable context, and sets the
   editor windows table to represent the new context.
   Parameters:
     e-ctx   _editable-context
     editor  _context-editor-widget")

(defmethod add-widget ::editable-context
  [e-ctx editor]
  (let [ ctx (get-context e-ctx)
         att-cols (:attr-cols e-ctx)
         obj-rows (:obj-rows e-ctx) 
         att (attributes ctx)
         obj (objects ctx)
         inc (incidence ctx)
         get-cross (fn [obj att] (if (contains? inc [obj att]) "X" " "))
         table (get-table editor)
         current-ectx (get-ectx editor) ]   

    (dosync-wait (commute (:widgets current-ectx) del editor)
      (set-ectx editor e-ctx)
      (commute (:widgets e-ctx) add editor))
    (set-hook table "cell-value" (fn [_ _ x] x))
    (set-column-count table (+ 1 (count att)))
    (set-row-count table (+ 1 (count obj)))
    (doseq [a att] (set-value-at-index table 0 (att-cols a) a))
    (doseq [o obj] (set-value-at-index table (obj-rows o) 0 o))
    (doseq [a att o obj] (set-value-at-index table (obj-rows o) (att-cols a)
                           (get-cross o a)))
    (set-value-at-index table 0 0 "⇊objects⇊")
    (set-hook table "extend-rows-to" 
      (fn [x] (ectx-extend-rows-hook e-ctx x)))
    (set-hook table "extend-columns-to" 
      (fn [x] (ectx-extend-columns-hook e-ctx x)))
    (set-hook table "cell-value" 
      (fn [r c s] (ectx-cell-value-hook e-ctx r c s)))))

(inherit-multimethod set-context ::editable-context
  "Sets the bound fca-context of an editable context object to ctx.

   Parameters:
    ectx   _editable-context object
    ctx    _new context")

(defmethod
  set-context ::editable-context
  [ectx ctx]
  (let [ new (make-editable-context ctx)
         widgets @(:widgets ectx) ]
    (dosync-wait
      (ref-set (:widgets ectx) @(:widgets new))
      (ref-set (:context ectx) @(:context new))
      (ref-set (:attr-cols ectx) @(:attr-cols new))
      (ref-set (:obj-rows ectx) @(:obj-rows new)))
    (call-many widgets (rho add-widget ectx))))


                   
(declare change-attribute-name change-object-name change-incidence-cross)

(inherit-multimethod change-incidence-cross ::editable-context
 "Sets or unsets the cross in the incidence relation of the editable-context.

  Parameters:
     ectx       _editable-context
     obj        _object
     att        _attribute
     cross      _true or false")

(defmethod change-incidence-cross ::editable-context
  [ectx obj att cross]
  (let [ ostr (if (= (type obj) String) obj
                (@(:obj-rows ectx) obj))
         astr (if (= (type att) String) att
                (@(:attr-cols ectx) att)) ]
    (dosync-wait
      (commute (:context ectx)
        (fn [x]
          (let [ as (attributes x)
                 os (objects x)
                 ir (incidence x) ]
            (if cross (make-context os as (conj ir [ostr astr]))
              (make-context os as (disj ir [ostr astr])))))))
    (let [ row (@(:obj-rows ectx) ostr)
           col (@(:attr-cols ectx) astr) ]
      (call-many @(:widgets ectx)
        (fn [w] (update-value-at-index (get-table w)
                  row col (if cross "X" " ")))))))


(inherit-multimethod change-attribute-name ::editable-context
 "Changes the attributes name of given to requested, if requested is
  not taken by another attribute. In this case, -# with a unique # is
  appended to the requested name. Returns the new attribute name

  Parameters:
     ectx       _editable-context
     given      _current attribute name or column index
     requested  _requested new attribute name")

(defmethod change-attribute-name ::editable-context
  [ectx given requested]
  (let [ att-cols @(:attr-cols ectx) ]
    (if (= (type given) String)
      (change-attribute-name ectx (att-cols given) requested)
      (dosync-wait
        (let [ ctx  (get-context ectx)
               current-name (att-cols given)
               attribs (attributes ctx)
               other-attribs (disj attribs current-name)
               new-name (req-unique-string other-attribs requested) ]
          (when-not (= current-name new-name)
            (let [ amap (fn [x] (if (= x current-name) new-name x)) ]
              (commute (:context ectx)
                (fn [x] (let [ as (conj (disj (attributes x) current-name)
                                    new-name)
                               os (objects x)
                               ir (map (fn [x] [(first x)(amap (second x))])
                                    (incidence x))]
                          (make-context os as ir))))
              (commute (:attr-cols ectx) switch-bipartit-auto
                given current-name given new-name)
              (call-many @(:widgets ectx)
                (fn [w] (update-value-at-index (get-table w)
                          0 given new-name)))))
          new-name)))))


(inherit-multimethod change-object-name ::editable-context
 "Changes the object's name of given to requested, if requested is
  not taken by another object. In this case, -# with a unique # is
  appended to the requested name. Returns the new attribute name

  Parameters:
     ectx       _editable-context
     given      _current attribute name or column index
     requested  _requested new attribute name")

(defmethod change-object-name ::editable-context
  [ectx given requested]
  (let [ obj-cols @(:obj-rows ectx) ]
    (if (= (type given) String)
      (change-object-name ectx (obj-cols given) requested)
      (dosync-wait
        (let [ ctx  (get-context ectx)
               current-name (obj-cols given)
               objs (objects ctx)
               other-objs (disj objs current-name)
               new-name (req-unique-string other-objs requested) ]
          (when-not (= current-name new-name)
            (let [ omap (fn [x] (if (= x current-name) new-name x)) ]
              (commute (:context ectx)
                (fn [x] (let [ os (conj (disj (objects x) current-name)
                                    new-name)
                               as (attributes x)
                               ir (map (fn [x] [(omap (first x))(second x)])
                                    (incidence x))]
                          (make-context os as ir))))
              (commute (:obj-rows ectx) switch-bipartit-auto
                given current-name given new-name)
              (call-many @(:widgets ectx)
                (fn [w] (update-value-at-index (get-table w)
                          given 0 new-name)))))
          new-name)))))


nil
