(ns conexp-gui.api-client
  "Talks to the conexp.api JSON-RPC endpoint from the browser.")

(def api-base
  "Base URL of the conexp.api server. The API serves the SPA itself, so we call
  it same-origin on whatever port it runs (8080, or any -p PORT). Only the
  shadow-cljs dev server (:8280) is a separate origin and targets the API on
  :8080."
  (let [loc js/window.location]
    (if (= (.-port loc) "8280")
      (str (.-protocol loc) "//" (.-hostname loc) ":8080")
      "")))

(defn post-rpc
  "POSTs `request` (Clojure data) as JSON to the API. Calls `on-success` with
  the parsed response (keywordized) or `on-error` with a message string."
  [request on-success on-error]
  (-> (js/fetch (str api-base "/")
                #js {:method  "POST"
                     :headers #js {"Content-Type" "application/json"}
                     :body    (js/JSON.stringify (clj->js request))})
      (.then (fn [resp]
               (if (.-ok resp)
                 (.json resp)
                 (throw (js/Error. (str "HTTP " (.-status resp) " from API"))))))
      (.then (fn [data] (on-success (js->clj data :keywordize-keys true))))
      (.catch (fn [err] (on-error (or (.-message err) "network error"))))))

;;; request builders (the JSON-RPC function/args batch form)

(defn context-object
  "A JSON-RPC data object describing a formal context."
  [{:keys [objects attributes incidence]}]
  {:type "context"
   :data {:objects    (vec objects)
          :attributes (vec attributes)
          :incidence  (mapv vec incidence)}})

(defn lattice-layout-request
  "Batch request computing the concept lattice of `ctx` and a named layout."
  [ctx layout-name]
  {:ctx    (context-object ctx)
   :ln     {:type "string" :data layout-name}
   :layout {:type "function" :name "sh-lattice-layout" :args ["ctx" "ln"]}})

(defn layout-with-valuation-request
  "Batch request for a named layout whose nodes carry a named valuation."
  [ctx layout-name valuation-name]
  {:ctx    (context-object ctx)
   :ln     {:type "string" :data layout-name}
   :vn     {:type "string" :data valuation-name}
   :layout {:type "function" :name "sh-layout-with-valuation" :args ["ctx" "ln" "vn"]}})

(defn layout-result
  "Extracts the layout->json payload (or nil) from an RPC response."
  [response]
  (get-in response [:layout :result]))
