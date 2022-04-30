(ns logos.core
  (:require [org.httpkit.server       :as server]
            [clojure.edn              :as edn]
            [clojure.data.json        :as json]
            [clojure.pprint           :as pp]
            [compojure.core           :as compojure]
            [compojure.route          :as route]
            [logos.formula            :as f]
            [logos.main               :as main]
            [ring.middleware.cors     :as cors]
            [ring.middleware.defaults :as middleware]
            [clojure.java.io :as io]))

(defn ^:private one-step-internal
  [command proof]
  (let [result (main/next-step command proof)]
    result))

(defn one-step
  [req]
  (let [rdr (io/reader (:body req))
        body (->> rdr
                  slurp
                  json/read-str
                  second
                  edn/read-string
                  :body)                ; I have no idea why all this
                                        ; is necessary
        command (get body :command)
        proof   (get body :proof)]
    (try
      (let [body (json/write-str (one-step-internal command proof))]
        {:status 200
         :headers {"Content-Type" "text/json"}
         :body body})
      (catch Exception e
        {:status 400
         :headers {"Content-Type" "text/json"}
         :body (json/write-str (ex-data e))}))))

(defn ^:private start-proof-internal
  [req]
  (let [command (-> req (get :query-params) (get "theorem"))
        result  (main/start-proof command)]
    result))


(defn start-proof
  [req]
  (let [body (json/write-str (start-proof-internal req))]
    {:status 200
     :headers {"Content-Type" "text/json"}
     :body body}))

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
                          pp/*print-right-margin* 60]
                  (with-out-str
                    (pp/pprint (f/read-formula-string formula))))]
    result))

(defn format-formula
  [req]
  {:status 200
   :headers {"Content-Type"  "text/json"
             "Access-Control-Allow-Credentials" "true"
             "Access-Control-Allow-Origin" "*"
             "Access-Control-Allow-Headers" "x-requested-with"
             "Access-Control-Allow-Methods" "*"
             }
   :body (json/write-str (format-formula-internal req))})

(defn formulas-internal
  []
  (slurp "resources/formula.md"))

(defn formulas
  [_]
  {:status 200
   :headers {"Content-Type" "text/json"
             "Access-Control-Allow-Origin" "*"
             "Access-Control-Allow-Headers" "x-requested-with"
             "Access-Control-Allow-Methods" "*"}
   :body (json/write-str (formulas-internal))})

(defn tutorial-internal
  []
  (slurp "resources/tutorial.md"))

(defn tutorial
  [_]
  {:status 200
   :headers {"Content-Type" "text/json"
             "Access-Control-Allow-Origin" "*"
             "Access-Control-Allow-Headers" "x-requested-with"
             "Access-Control-Allow-Methods" "*"}
   :body (json/write-str (tutorial-internal))})

(compojure/defroutes app
  (compojure/GET "/" req (str req))
  (compojure/GET "/health-check" [] health-check)
  (compojure/POST "/one-step" req (one-step req))

  (compojure/GET  "/start-proof"  [] start-proof)
  (compojure/POST "/run-steps" [] run-steps)
  (compojure/POST "/format" [] format-formula)
  (compojure/GET  "/tutorial" [] tutorial)
  (compojure/GET "/formulas"  [] formulas)
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
