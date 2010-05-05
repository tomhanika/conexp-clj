;; Copyright (c) Daniel Borchmann. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.util.typecheck
  (:use [clojure.contrib.string :only (join replace-first-re)]
    clojure.contrib.def
    conexp.util))

;;; since in the recent version, type doens't work with keywords anymore, but
;;; returns classes (along with new deftype-constructor-dot) and derive
;;; does not work with classes (but with clojure.lang.Named), there is a need
;;; for the following macros (at least for now)

(defn class-to-keyword
  "Takes a class object and returns a corresponding keyword describing the
  class name.

  Parameters:
    c  _class"
  [c]
  (let [ rv clojure.contrib.string/reverse
         classname (str c)
         keywordname (rv 
                       (replace-first-re #"\." "/" 
                         (rv (replace-first-re #"class " "" classname)))) ]
    (keyword keywordname)))


;;;
;;;
;;; defn-typecheck
;;;

(defmacro defn-typecheck
  "This macro is a helper for typechecking the first parameter via
   class-to-keyword and derive/isa? and throws illegal-argument if
   the first parameter is not a child of the given keyword.

  Parameters:
    name    _name of the function
    parent  _parent-keyword
    doc-str _doc-string of the function
    params  _parameter vector
    & body  _function body"
  [name parent doc-str params & body]
  (let [ check-parm (first params)
         name-str (str name)
         type-name (str parent)]
    `(defn ~name ~doc-str ~params
       (if (isa? (class-to-keyword (type ~check-parm)) ~parent)
         (do ~@body)
         (illegal-argument (str ~name-str 
                             " called with the first parameter of type "
                             (class-to-keyword (type ~check-parm))
                             " which is not a child-type of " ~type-name " ."))))))
