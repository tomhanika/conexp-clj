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
            [clojure.java.io :as io]
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

(defn gui-built?
  "True iff the compiled web GUI (js/main.js) is on the classpath, i.e. the
  frontend was built into this jar / resources."
  []
  (some? (io/resource "public/js/main.js")))

(def ^:private gui-not-built-page
  (str "<!doctype html><html><head><meta charset=\"utf-8\">"
       "<title>conexp-clj</title></head>"
       "<body style=\"font-family:system-ui,sans-serif;max-width:40rem;margin:3rem auto;padding:0 1rem\">"
       "<h1>conexp-clj</h1>"
       "<p>The JSON-RPC API is running, but the web GUI has not been built into this jar.</p>"
       "<p>Build a self-contained jar with <code>make uberjar</code> "
       "(or run <code>npx shadow-cljs release app</code> before <code>lein uberjar</code>).</p>"
       "<p>The API endpoint itself is fully functional at this URL "
       "(POST JSON-RPC requests).</p>"
       "</body></html>"))

(defn set-middleware
  "Builds the server handler.  GET requests are served from the single-page
  app's static resources (classpath `public/`) with an index.html fallback for
  client-side routes; every other request goes to the JSON-RPC handler.  If the
  GUI was not built into the jar, the app shell is replaced by a clear notice
  (the API still works) instead of a silently blank page."
  [dev]
  (let [rpc   (rpc-middleware dev)
        shell (fn [] (-> (if (gui-built?)
                           (response/resource-response "index.html" {:root "public"})
                           (response/response gui-not-built-page))
                         ;; set explicitly: for GET "/" the content-type
                         ;; middleware cannot infer html, and from a jar the
                         ;; resource is a jar: URL, so the browser would
                         ;; otherwise download index.html instead of rendering.
                         (response/content-type "text/html; charset=utf-8")))]
    (-> (fn [request]
          (if (= :get (:request-method request))
            (let [uri (:uri request)]
              (if (contains? #{"/" "/index.html"} uri)
                (shell)                              ; app shell (or notice if unbuilt)
                (or (response/resource-response uri {:root "public"}) ; static asset
                    (shell)                          ; SPA client-side route
                    (rpc request))))
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

(defn- open-browser
  "Best-effort: opens `url` in the default browser. Stays silent on headless
  machines / servers where there is nothing to open."
  [url]
  (try
    (if (and (java.awt.Desktop/isDesktopSupported)
             (.isSupported (java.awt.Desktop/getDesktop) java.awt.Desktop$Action/BROWSE))
      (.browse (java.awt.Desktop/getDesktop) (java.net.URI. url))
      (let [os  (.toLowerCase (System/getProperty "os.name"))
            cmd (cond (re-find #"mac|darwin" os) ["open" url]
                      (re-find #"win"         os) ["cmd" "/c" "start" url]
                      :else                       ["xdg-open" url])]
        (.start (ProcessBuilder. ^"[Ljava.lang.String;" (into-array String cmd)))))
    (catch Throwable _ nil)))

(defn announce
  "Prints where the web GUI is served and tries to open it in a browser."
  [port]
  (let [url (str "http://127.0.0.1:" port)]
    (println)
    (println (str "  conexp-clj web GUI running at:  " url))
    (if (gui-built?)
      (println "  Opening it in your browser…  (Ctrl-D or (exit) here stops the server.)")
      (println "  NOTE: the web GUI is not built into this jar (API only)."))
    (println)
    (flush)
    (when (gui-built?) (open-browser url))
    url))

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
