;; Copyright ⓒ the conexp-clj developers; all rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

;; This file has been written by Immanuel Albrecht, with modifications by DB

(ns conexp.contrib.gui.editors.context-editor.editable-contexts
  (:use conexp.fca.contexts
        conexp.contrib.gui.util
        conexp.contrib.gui.editors.context-editor.widgets
        conexp.contrib.gui.editors.context-editor.table-control)
  (:use [clojure.string :only (join)]
        [clojure.set :only (map-invert)]))

;;; Helpers

(defn- smart-str
  "Takes a single argument and returns it as a string that is usable
   for object and attribute names."
  [x]
  (cond
   (or (vector? x)
       (seq? x))
   (join " " (map smart-str x)),
   (set? x)
   (str "{" (join ", " (map smart-str (vec x))) "}"),
   :else
   (str x)))

(defn- map-to-unique-strings
  "Takes a sequence of (unique) keys and returns a map
   that maps each unique key to a unique string."
  [keys]
  (loop [k     (set keys),
         m     {},
         taken #{},
         nbr   0]
    (if (empty? k)
      m
      (let [k1        (first k),
            k1-cvrt   (smart-str k1),
            k1-as-str (if (< 0 nbr)
                        (join "-" [k1-cvrt (str nbr)])
                        k1-cvrt)]
        (if (contains? taken k1-as-str)
          (recur k m taken (+ 1 nbr))
          (recur (disj k k1)
                 (conj m {k1 k1-as-str})
                 (conj taken k1-as-str) 0))))))

(defn req-unique-string
  "Takes a sequence of keys and a new requested key and
  returns a new key that will not conflict with any of the
  keys in the sequence."
  [old-keys req-key]
  (let [keys (set old-keys)]
    (loop [nbr 0]
      (let [new-key (if (< 0 nbr)
                      (join "-" [(str req-key) (str nbr)])
                      (str req-key))]
        (if (contains? keys new-key)
          (recur (+ 1 nbr))
          new-key)))))

(defn- switch-bipartit-auto
  "Takes a bipartit auto map and removes the old-key and old-value
   associations and associates new-key with new-value and new-value
   with new-key, then returns the new map."
  [m old-key old-value new-key new-value]
  (let [m-without-old (dissoc m old-key old-value),
        m-with-new    (assoc m-without-old new-key new-value new-value new-key)]
    m-with-new))

;;; Editable contexts

(defwidget editable-context []
  [context attr-cols obj-rows widgets])

(defn editable-context?
  "Tests whether the argument is an editable context."
  [ctx]
  (keyword-isa? ctx editable-context))

(defmulti get-context
  "Returns the fca-context that belongs to the first parameter."
  (fn [& x] (keyword-class (first x)))
  :default nil)

(defmethod get-context (class-to-keyword editable-context)
  [ectx]
  (deref (:context ectx)))

(defmethod get-context :conexp.fca.contexts/Formal-Context
  [x] x)

(defn- make-context-compatible
  "Takes a context object and returns a compatible fca-Context."
  [ctx]
  (let [ctx (get-context ctx),

        obj (objects ctx),
        att (attributes ctx),
        inc (incidence ctx),

        obj-map (map-to-unique-strings obj),
        obj-map-i (map-invert obj-map),
        att-map (map-to-unique-strings att),
        att-map-i (map-invert att-map),

        comp-obj (map obj-map obj),
        comp-att (map att-map att),
        comp-inc (fn [g m] (inc [(obj-map-i g) (att-map-i m)]))]

    (make-context comp-obj comp-att comp-inc)))

(defn- restore-order
  "Takes obj-rows as a parameter and a corresponding list of object names
   that shall take their respective position if available."
  [old-obj-rows old-objs new-obj-rows obj-nbrs]
  (let [new-obj-keys (set (filter string? (keys new-obj-rows)))]
    (loop [obj-rows new-obj-rows,
           old-objs (filter (fn [x] (contains? new-obj-keys x)) old-objs) ]
      (if (empty? old-objs)
        obj-rows
        (let [obj    (first old-objs),
              target (old-obj-rows obj),
              source (obj-rows obj)]
          (if (and (contains? obj-nbrs target)
                   (not (= source target)))
            (let [obj2           (obj-rows target),
                  other-obj-rows (dissoc obj-rows obj target obj2 source)
                  switched-rows (conj other-obj-rows
                                      {obj target, target obj,
                                       obj2 source, source obj2})]
              (recur switched-rows (rest old-objs)))
            (recur obj-rows (rest old-objs))))))))

(defn make-editable-context
  "Takes an optional context as input and returns an appropriate
  editable-context structure that is bound to a compatible version of
  the input context."
  ([]
     (make-editable-context (make-context '() '() [])))
  ([context-in]
    (let [ctx (make-context-compatible context-in),
          att (attributes ctx),
          att-sort (sort (seq att)),
          obj (objects ctx),
          obj-sort (sort (seq obj)),
          self (promise),
          e-ctx (editable-context. (ref ctx)
                                   (ref (conj (zipmap att-sort (range 1 (+ 1 (count att-sort))))
                                              (zipmap (range 1 (+ 1 (count att-sort))) att-sort)))
                                   (ref (conj (zipmap obj-sort (range 1 (+ 1 (count obj-sort))))
                                              (zipmap (range 1 (+ 1 (count obj-sort))) obj-sort)))
                                   (ref (make-one-to-many self)))]
      (deliver self e-ctx)
      e-ctx))
  ([context-in keep-order]
    (let [e-ctx (make-editable-context context-in),
          old-obj-rows (:obj-rows keep-order),
          old-objs (filter string? (keys old-obj-rows)),
          old-attr-cols (:attr-cols keep-order),
          old-atts (filter string? (keys old-attr-cols)),
          other-obj-rows (deref (:obj-rows e-ctx)),
          obj-nbrs (set (filter (comp not string?)
                                (keys other-obj-rows)))
          other-attr-cols (deref (:attr-cols e-ctx))
          att-nbrs (set (filter (comp not string?)
                                (keys other-attr-cols))),
          new-obj-rows (restore-order old-obj-rows old-objs
                                      other-obj-rows obj-nbrs),
          new-attr-cols (restore-order old-attr-cols old-atts
                                       other-attr-cols att-nbrs)]
      (dosync
       (ref-set (:obj-rows e-ctx) new-obj-rows)
       (ref-set (:attr-cols e-ctx) new-attr-cols))
      e-ctx)))

(defn get-order
  "Returns the current order of the objects and attributes of the
  context."
  [ectx]
  (assert (instance? editable-context ectx))
  {:attr-cols (deref (:attr-cols ectx)),
   :obj-rows (deref (:obj-rows ectx))})

(defn get-dual-order
  "Returns the current order of the objects and attributes of the dual
   of the context."
  [ectx]
  (assert (instance? editable-context ectx))
  {:obj-rows (deref (:attr-cols ectx)),
   :attr-cols (deref (:obj-rows ectx))})

(defn change-incidence-cross
  "Sets or unsets the cross in the incidence relation of the
  editable-context."
  [ectx obj att cross]
  (assert (instance? editable-context ectx))
  (let [ostr (if (= (type obj) String)
               obj
               (@(:obj-rows ectx) obj)),
        astr (if (= (type att) String)
               att
               (@(:attr-cols ectx) att))]
    (dosync
      (alter (:context ectx)
             (fn [x]
               (let [as (attributes x),
                     os (objects x),
                     ir (incidence-relation x)]
                 (if cross
                   (make-context os as (conj ir [ostr astr]))
                   (make-context os as (disj ir [ostr astr])))))))
    (let [row (@(:obj-rows ectx) ostr),
          col (@(:attr-cols ectx) astr)]
      (call-many @(:widgets ectx)
                 (fn [w]
                   (update-value-at-index (get-table w)
                                          row
                                          col
                                          (if cross "X" "")))))))

(defn change-attribute-name
  "Changes the attributes name of given to requested, if requested is
   not taken by another attribute. In this case, -# with a unique # is
   appended to the requested name. Returns the new attribute name."
  [ectx given requested]
  (assert (instance? editable-context ectx))
  (let [att-cols @(:attr-cols ectx)]
    (if (= (type given) String)
      (change-attribute-name ectx (att-cols given) requested)
      (dosync
        (let [ctx           (get-context ectx),
              current-name  (att-cols given),
              attribs       (attributes ctx),
              other-attribs (disj attribs current-name),
              new-name (req-unique-string other-attribs requested)]
          (when-not (= current-name new-name)
            (let [amap #(if (= % current-name) new-name %)]
              (alter (:context ectx)
                     (fn [x]
                       (let [as (conj (disj (attributes x) current-name)
                                      new-name),
                             os (objects x)
                             ir (map (fn [x] [(first x)(amap (second x))])
                                     (incidence-relation x))]
                         (make-context os as ir))))
              (alter (:attr-cols ectx) switch-bipartit-auto
                     given current-name given new-name)
              (call-many @(:widgets ectx)
                         (fn [w]
                           (update-value-at-index (get-table w)
                                                  0
                                                  given new-name)))))
          new-name)))))

(defn change-object-name
  "Changes the object's name of given to requested, if requested is
  not taken by another object. In this case, -# with a unique # is
  appended to the requested name. Returns the new attribute name."
  [ectx given requested]
  (assert (instance? editable-context ectx))
  (let [obj-cols @(:obj-rows ectx)]
    (if (= (type given) String)
      (change-object-name ectx (obj-cols given) requested)
      (dosync
        (let [ctx          (get-context ectx)
              current-name (obj-cols given)
              objs         (objects ctx)
              other-objs   (disj objs current-name)
              new-name     (req-unique-string other-objs requested)]
          (when-not (= current-name new-name)
            (let [omap #(if (= % current-name) new-name %)]
              (alter (:context ectx)
                     (fn [x]
                       (let [os (conj (disj (objects x) current-name)
                                      new-name),
                             as (attributes x)
                             ir (map (fn [x] [(omap (first x))(second x)])
                                     (incidence-relation x))]
                         (make-context os as ir))))
              (alter (:obj-rows ectx) switch-bipartit-auto
                     given current-name given new-name)
              (call-many @(:widgets ectx)
                         (fn [w]
                           (update-value-at-index (get-table w)
                                                  given 0 new-name)))))
          new-name)))))

;;;

nil
