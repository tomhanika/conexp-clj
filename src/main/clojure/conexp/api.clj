;; Copyright ⓒ the conexp-clj developers; all rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns conexp.api
  "Provides a RESTful JSON-RPC API for conexp-clj, and serves the browser GUI."
  (:require [conexp.api.handler :refer [handler]]
            [ring.middleware.reload :refer [wrap-reload]]
            [ring.middleware.json :refer [wrap-json-body wrap-json-response]]
            [ring.middleware.cors :refer [wrap-cors]]
            [ring.middleware.content-type :refer [wrap-content-type]]
            [ring.middleware.not-modified :refer [wrap-not-modified]]
            [ring.util.response :as response]
            [org.httpkit.server :refer [run-server]]))

;;;

(defn- rpc-middleware
  "The JSON-RPC stack around the conexp handler (CORS + JSON in/out, hot reload
  in dev)."
  [dev]
  (if dev
    (-> #'handler
        (wrap-reload)
        (wrap-cors :access-control-allow-origin [#".+"]
                 :access-control-allow-methods [:get :post]
                 :access-control-allow-headers ["Content-Type" "Authorization"])
        (wrap-json-body {:keywords? true})
        (wrap-json-response {:pretty true}))
    (-> handler
        (wrap-cors :access-control-allow-origin [#".+"]
                 :access-control-allow-methods [:get :post]
                 :access-control-allow-headers ["Content-Type" "Authorization"])
        (wrap-json-body {:keywords? true})
        (wrap-json-response {:pretty false}))))

(defn set-middleware
  "Builds the server handler.  GET requests are served from the single-page
  app's static resources (classpath `public/`) with an index.html fallback for
  client-side routes; every other request goes to the JSON-RPC handler."
  [dev]
  (let [rpc (rpc-middleware dev)]
    (-> (fn [request]
          (if (= :get (:request-method request))
            (let [uri  (:uri request)
                  path (if (= "/" uri) "/index.html" uri)]
              (or (response/resource-response path {:root "public"})
                  (response/resource-response "index.html" {:root "public"})
                  (rpc request)))
            (rpc request)))
        (wrap-content-type {:mime-types {"webmanifest" "application/manifest+json"}})
        (wrap-not-modified))))

(defonce server (atom nil))

(defn stop-server
  "Stops the server after a given wating time in ms. During the wait time
  existing requests can be finished."
  [ms]
  (when-not (nil? @server)
    (@server :timeout ms)
    (reset! server nil)))

(defn start-server
  "Starts the API + GUI server. If `dev` is true it supports hot code reload.
  Binds 127.0.0.1 by default: the API has no authentication and a broad
  function whitelist (including filesystem-reading functions), so it is meant
  for local use. Pass an explicit `ip` (e.g. \"0.0.0.0\") to deliberately
  expose it on the network."
  ([dev port] (start-server dev port "127.0.0.1"))
  ([dev port ip] (reset! server (run-server (set-middleware dev) {:port port :ip ip}))))

;;;

nil
