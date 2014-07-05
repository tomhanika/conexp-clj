;; Copyright (c) Daniel Borchmann. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.contrib.algorithms
  "Provides some optimized versions of the standard algorithms of conexp.main"
  (:require [conexp.main :as cm]
            [clojure.core.reducers :as r]
            conexp.contrib.algorithms.linclosure))

;;;

(defn parallel-canonical-base
  "Computes the canonical base of the context «ctx» in a parallel way, dividing the object
  set in chunks of size at most «n» each"
  [ctx n]
  (apply cm/intersect-implicational-theories
         (cm/attributes ctx)
         (pmap (fn [objs]
                 (cm/canonical-base (cm/make-context objs (cm/attributes ctx) (cm/incidence ctx))))
               (partition-all n (cm/objects ctx)))))

(cm/defalias linclosure conexp.contrib.algorithms.linclosure/close-under-implications)

;;;

nil
