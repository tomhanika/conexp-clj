(ns conexp.graphics.util
  (:import [no.geosoft.cc.graphics GScene]))


;;; technical helpers

(defn device-to-world [#^GScene scn x y]
  (let [trf (.getTransformer scn)
	ptn (.deviceToWorld trf x y)]
    [(aget ptn 0) (aget ptn 1)]))

(defn world-to-device [#^GScene scn x y]
  (let [trf (.getTransformer scn)
	ptn (.worldToDevice trf x y)]
    [(aget ptn 0) (aget ptn 1)]))

(defn origin [scn]
  (world-to-device scn 0 0))


;;;

nil
