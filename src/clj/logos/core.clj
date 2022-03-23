(ns logos.core
  (:require [org.httpkit.server       :as server]
            [clojure.data.json        :as json]
            [clojure.pprint           :as pp]
            [compojure.core           :as compojure]
            [compojure.route          :as route]
            [logos.main               :as main]
            [ring.middleware.cors     :as cors]
            [ring.middleware.defaults :as middleware]))

(defn ^:private one-step-internal
  [req]
  (let [command (-> req (get :query-params) (get "command"))
        result (main/next-step! command)]
    result))

(defn one-step
  [req]
  (try
    (let [body (json/write-str (one-step-internal req))]
      {:status 200
       :headers {"Content-Type" "text/json"}
       :body body})
    (catch Exception e
      (println e)
      (println (ex-data e))
      {:status 400
       :heades {"Content-Type" "text/json"}
       :body (ex-data e)})))

(defn ^:private clear-current-proof-internal
  []
  (main/clear-current-proof))

(defn clear-current-proof
  [_]
  {:status 200
   :headers {"Content-Type" "text/json"}
   :body (json/write-str (clear-current-proof-internal))})

(defn ^:private run-steps-internal
  [req]
  (let [command (-> req (get :query-params) (get "command"))
        result  (main/eval-commands! command)]
    result))

(defn run-steps
  [req]
  {:status 200
   :headers {"Content-Type" "text/json"}
   :body (json/write-str (run-steps-internal req))})

(defn health-check
  [req]
  {:status 200
   :headers {"Content-Type" "text/json"
             "Access-Control-Allow-Origin" "*"
             "Access-Control-Allow-Headers" "x-requested-with"
             "Access-Control-Allow-Methods" "*"}

   :body (json/write-str {:status "OK"})})

(defn ^:private format-formula-internal
  [req]
  (let [formula (-> req (get :query-params) (get "formula"))
        result  (binding [pp/*print-pretty* true
                          pp/*print-miser-width* nil
                          pp/*print-right-margin* 50]
                  (with-out-str
                    (pp/pprint (read-string formula))))]
    result))

(defn format-formula
  [req]
  {:status 200
   :headers {"Content-Type"  "text/json"}
   :body (json/write-str (format-formula-internal req))})

(compojure/defroutes app
  (compojure/GET "/" req (str req))
  (compojure/GET "/health-check" [] health-check)
  (compojure/POST "/one-step" [] one-step)
  (compojure/POST "/clear-current-proof" [] clear-current-proof)
  (compojure/POST "/run-steps" [] run-steps)
  (compojure/POST "/format" [] format-formula)
  (route/not-found "<h1>Page not found</h1>"))

(defn -main [& args]
  (server/run-server
    (cors/wrap-cors
     (middleware/wrap-defaults
      #'app middleware/api-defaults)
     :access-control-allow-origin [#".*"]
     :access-control-allow-methods [:get :put :post :delete]
     :access-control-allow-headers ["Origin" "X-Requested-With"
                                    "Content-Type" "Accept"])
   {:port 4000})
  (println "Server started on port 4000"))
