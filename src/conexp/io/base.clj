;; Copyright (c) Daniel Borchmann. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.io.base
  (:use [clojure.contrib.ns-utils :only (immigrate)]))

;;;

(immigrate 'conexp.base
	   'conexp.fca.contexts
	   'clojure.contrib.io
	   'clojure.contrib.lazy-xml
	   'clojure.contrib.prxml
	   'conexp.io.util) ; last to override with-in-reader from c.c.io

;;;

nil
