(ns logos.core
  (:require [logos.router :as router]
            [re-frame.core :refer [dispatch-sync]]
            [reagent.dom :as reagent]
            [logos.events :as event]  ;; These three are only
            [logos.views :as views]
            [logos.subs]    ;; required to make the compiler
            ))

(defn ^:export init []
;;  (router/start!)
  (dispatch-sync [::event/initialize-db])
  (reagent/render [views/main-panel]
    (.getElementById js/document "app"))
  )
