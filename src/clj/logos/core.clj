(ns logos.core
  (:require [org.httpkit.server       :as server]
            [clojure.edn              :as edn]
            [clojure.data.json        :as json]
            [clojure.pprint           :as pp]
            [compojure.core           :as compojure]
            [compojure.route          :as route]
            [logos.formula            :as f]
            [logos.main               :as main]
            [logos.log                :refer [log] :as log]
            [ring.middleware.cors     :as cors]
            [ring.middleware.defaults :as middleware]
            [clojure.java.io :as io])
  (:gen-class))

(defn ^:private one-step-internal
  "Run `command` on `proof`."
  [command proof]
  (let [result (main/next-step command proof)]
    result))

(defn one-step
  "Given a request, parse out the information stored there and
  update the encoded proof with the encoded command."
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
  "Start a proof given a request."
  [req]
  (let [command (-> req (get :query-params) (get "theorem"))
        result  (main/start-proof command)]
    result))


(defn start-proof
  "Start a proof given a request for one handling the response."
  [req]
  (log :info {:event "Proof started" :info (get req :query-params)})
  (let [body (json/write-str (start-proof-internal req))]
    {:status 200
     :headers {"Content-Type" "text/json"}
     :body body}))

(defn ^:private run-steps-internal
  "OBSOLETE: Given a request representing a list of commands, execute each
  command on the proof that's stored as state."
  [req]
  (let [command (-> req (get :query-params) (get "command"))
        result  (main/eval-commands! command)]
    result))

(defn run-steps
  "OBSOLETE: Given a request run all the steps represented there as
  commands against the proof in memory."
  [req]
  {:status 200
   :headers {"Content-Type" "text/json"}
   :body (json/write-str (run-steps-internal req))})

(defn health-check
  "Perform a health check on the backend"
  [req]
  (log :info {:event "health-check"})
  {:status 200
   :headers {"Content-Type" "text/json"
             "Access-Control-Allow-Origin" "*"
             "Access-Control-Allow-Headers" "x-requested-with"
             "Access-Control-Allow-Methods" "*"}

   :body (json/write-str {:status "OK"})})

(defn ^:private format-formula-internal
  "Get the information from a request and format it nicely."
  [req]
  (let [formula (-> req (get :query-params) (get "formula"))
        result  (binding [pp/*print-pretty* true
                          pp/*print-miser-width* nil
                          pp/*print-right-margin* 60]
                  (with-out-str
                    (pp/pprint (f/read-formula-string formula))))]
    result))

(defn format-formula
  "Get the information from a request and format it nicely handling
  response codes."
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
  "Get the markdown for the formula specification page."
  []
  (slurp "resources/formula.md"))

(defn formulas
  "Return the markdown for the formula specification page."
  [_]
  {:status 200
   :headers {"Content-Type" "text/json"
             "Access-Control-Allow-Origin" "*"
             "Access-Control-Allow-Headers" "x-requested-with"
             "Access-Control-Allow-Methods" "*"}
   :body (json/write-str (formulas-internal))})

(defn tutorial-internal
  "Get the markdown for the tutorial page."
  []
  (slurp "resources/tutorial.md"))

(defn tutorial
  "Return the markdown for the tutorial page."
  [_]
  {:status 200
   :headers {"Content-Type" "text/json"
             "Access-Control-Allow-Origin" "*"
             "Access-Control-Allow-Headers" "x-requested-with"
             "Access-Control-Allow-Methods" "*"}
   :body (json/write-str (tutorial-internal))})

;; Definition of the URL endpoints that the backend recognized.
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

(defn -main
  "Start a backend logos server."
  [& args]
  (let [port 4000]
    (log/init (keyword (System/getenv "LOG_LEVEL")))
    (server/run-server
     (cors/wrap-cors
      (middleware/wrap-defaults #'app middleware/api-defaults)
      :access-control-allow-origin [#".*"]
      :access-control-allow-methods [:get :put :post :delete]
      :access-control-allow-headers ["Origin" "X-Requested-With"
                                     "Content-Type" "Accept"])
     {:port port})
    (log :info {:event
                (format "Server started on port %s" port)})))
