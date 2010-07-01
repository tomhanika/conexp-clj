;; Copyright (c) Daniel Borchmann. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.contrib.draw.util
  (:import [no.geosoft.cc.graphics GScene]))


;;; technical helpers

(defn device-to-world
  "Transforms a device coordinate pair [x y] to a world coordinate pair for the given scene."
  [#^GScene scn x y]
  (let [trf (.getTransformer scn)
	ptn (.deviceToWorld trf x y)]
    [(aget ptn 0) (aget ptn 1)]))

(defn world-to-device
  "Transforms a world coordinate pair [x y] of the given scene to a device coordinate pair."
  [#^GScene scn x y]
  (let [trf (.getTransformer scn)
	ptn (.worldToDevice trf x y)]
    [(aget ptn 0) (aget ptn 1)]))

(defn origin
  "Returns the origin of scn."
  [scn]
  (world-to-device scn 0 0))


;;;

nil
