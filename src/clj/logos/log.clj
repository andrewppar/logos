(ns logos.log
  (:require [taoensso.timbre :as timbre]
            [taoensso.timbre.appenders.core :as appenders]
            [clojure.data.json :as json]))

(defn json-output-fn
  [{:keys [vargs_ hostname_ timestamp_ level] :as args}]
  (let [messages (map (fn [msg] { :timestamp @timestamp_
                                 :level     level
                                 :hostname  @hostname_
                                 :message   msg })
                      @vargs_)
        json-messages (map #(json/write-str %) messages)]
    (clojure.string/join "\n" json-messages)))

(defn init [level]
  (let [log-path "logs/logos.log"
        exists? (.exists (java.io.File. log-path))]
    (when-not exists?
      (.mkdir (java.io.File. log-path)))
    (timbre/merge-config!
     {:min-level level
      :appenders {:spit (appenders/spit-appender {:fname log-path})}
      :output-fn json-output-fn})))

(defn log [level event]
  (case level
    :trace (timbre/trace event)
    :debug (timbre/debug event)
    :info  (timbre/info event)
    :warn  (timbre/warn event)
    :error (timbre/error event)))
