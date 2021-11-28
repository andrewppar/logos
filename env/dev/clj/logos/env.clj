(ns logos.env
  (:require
    [selmer.parser :as parser]
    [clojure.tools.logging :as log]
    [logos.dev-middleware :refer [wrap-dev]]))

(def defaults
  {:init
   (fn []
     (parser/cache-off!)
     (log/info "\n-=[logos started successfully using the development profile]=-"))
   :stop
   (fn []
     (log/info "\n-=[logos has shut down successfully]=-"))
   :middleware wrap-dev})
