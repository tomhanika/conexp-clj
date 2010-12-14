;; Copyright (c) Daniel Borchmann. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.contrib.tests.dl
  (:use conexp.main))

;;;

(tests-to-run conexp.contrib.tests.dl.framework.semantics
              conexp.contrib.tests.dl.languages.description-graphs
              conexp.contrib.tests.dl.languages.EL-gfp
              conexp.contrib.tests.dl.framework.reasoning
              conexp.contrib.tests.dl.util.concept-sets
              conexp.contrib.tests.dl.languages.EL-gfp-rewriting
              conexp.contrib.tests.dl.languages.EL-gfp-exploration)

;;;

nil
