(ns logos.core
  (:require [org.httpkit.server       :as server]
            [compojure.core           :as compojure]
            [compojure.route          :as route]
            [logos.main               :as main]
            [ring.middleware.defaults :as middleware]))

(defn -one-step
  [req]
  (let [command (-> req (get :query-params) (get "command"))
        result (main/next-step command)]
    result))

(defn one-step
  [req]
  {:status 200
   :headers {"Content-Type" "text/json"}
   :body (-one-step req)})

(compojure/defroutes app
  (compojure/GET "/" req (str req))
  (compojure/GET "/health-check" [] "OK")
  (compojure/POST "/one-step" [] one-step)
  (route/not-found "<h1>Page not found</h1>"))


(defn -main [& args]
  (server/run-server
   (middleware/wrap-defaults
    #'app middleware/api-defaults)
   {:port 4000})
  (println "Server started on port 4000"))
