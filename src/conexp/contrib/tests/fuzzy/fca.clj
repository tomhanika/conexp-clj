;; Copyright (c) Daniel Borchmann. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.contrib.tests.fuzzy.fca
  (:use conexp.main
        conexp.contrib.fuzzy.sets
        conexp.contrib.fuzzy.logics
        conexp.contrib.fuzzy.fca)
  (:use clojure.test))

;;;

(defvar- *fuzzy-ctx-1* (make-fuzzy-context [1 2 3 4]
                                           [1 2 3 4 5 6]
                                           [1.0 1.0 0.0 1.0 1.0 0.2,
                                            1.0 0.4 0.3 0.8 0.5 1.0,
                                            0.2 0.9 0.7 0.5 1.0 0.6,
                                            1.0 1.0 0.8 1.0 1.0 0.5]))

;;;

nil
