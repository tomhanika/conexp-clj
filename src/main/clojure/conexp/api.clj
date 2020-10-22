;; Copyright ⓒ the conexp-clj developers; all rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.api
  "Provides restful api for conexp-clj."
  (:require [conexp.api.handler :refer [handler]]
            [ring.middleware.reload :refer [wrap-reload]]
            [ring.middleware.json :refer [wrap-json-body wrap-json-response]]
            [org.httpkit.server :refer [run-server]]))

;;;

(defn set-middleware
  "Applies all middleware to the handler."
  [dev]
  (if dev
    (-> #'handler
      (wrap-reload)
      (wrap-json-body {:keywords? true})
      (wrap-json-response {:pretty true}))
    (-> handler 
      (wrap-json-body {:keywords? true})
      (wrap-json-response {:pretty false}))))

(defonce server (atom nil))

(defn stop-server
  "Stops the server after a given wating time in ms. During the wait time 
  existing requests can be finished."
  [ms]
  (when-not (nil? @server)
    (@server :timeout ms)
    (reset! server nil)))

(defn start-server
  "Starts a restful server. If boolean argument is true it will support hot 
  code reload."
  [dev port]
  (reset! server ( run-server (set-middleware dev) {:port port})))

;;;

nil
