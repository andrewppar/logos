(ns logos.log
  (:require [taoensso.timbre :as timbre]
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
  (timbre/merge-config!
   {:appenders {:println {:output-fn json-output-fn}}}))


(defn log [level event]

  )
