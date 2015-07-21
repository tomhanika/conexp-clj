;; Sebastian Böhm, Daniel Borchmann, 2010
;; This file is in the public domain

(require 'conexp.main)
(in-ns 'conexp.main)

;; In- and output for formal contexts

;; Imagine you have a context given, which is needed in more than on
;; session (e.g. Chevron-context in all examples).

;; It would be sensefull to define this context once and then save it.

;; Let's take a context and save it as cxt-1.

(def ctx-1 (make-context-from-matrix ['a 'b 'c 'd 'e 'f]
                                     ['a 'b 'c 'd 'e 'f]
                                     [1 1 1 0 1 1
                                      0 1 0 0 1 0
                                      0 0 1 0 0 1
                                      0 0 0 1 1 1
                                      0 0 0 0 1 0
                                      0 0 0 0 0 1]))

;; The next step is to save this context in a file. There are
;; different formats in which you can save it:

(list-context-formats)

;; You get a back a list of all supported context-formats. These are
;; :simple, :burmeister, :colibri, :conexp, :csv and :galicia.

;; To save the given context in the :simple format use

(write-context :simple ctx-1 "/path/of/outputfile")

;; If you want to read a context from a given file and save it as
;; ctx-2 use

(def ctx-2 (read-context "/path/of/inputfile"))

;;

nil
