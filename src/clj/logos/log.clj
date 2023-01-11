(ns logos.log
  (:require [taoensso.timbre :as timbre]
            [taoensso.timbre.appenders.core :as appenders]
            [clojure.data.json :as json]))

(defn ^:private json-output-fn
  "Convert a map into a json strong to be used in logging."
  [{:keys [vargs_ hostname_ timestamp_ level] :as args}]
  (let [messages (map (fn [msg] { :timestamp @timestamp_
                                 :level     level
                                 :hostname  @hostname_
                                 :message   msg })
                      @vargs_)
        json-messages (map #(json/write-str %) messages)]
    (clojure.string/join "\n" json-messages)))

(defn init
  "Initialize Logging."
  [raw-level]
  (let [level (or raw-level :info)
        log-path "logs/logos.log"
        exists? (.exists (java.io.File. log-path))]
    (when-not exists?
      (.mkdir (java.io.File. log-path)))
    (timbre/merge-config!
     {:min-level level
      :appenders {:spit (appenders/spit-appender {:fname log-path})}
      :output-fn json-output-fn})))

(defn log
  "Log `event` at `level`."
  [level event]
  (case level
    :trace (timbre/trace event)
    :debug (timbre/debug event)
    :info  (timbre/info event)
    :warn  (timbre/warn event)
    :error (timbre/error event)))
