(ns logos.core
  (:require [logos.ajax           :as ajax]
            [re-frame.core        :refer [dispatch-sync]]
            [reagent.dom          :as reagent]
            [reitit.core          :as reitit]
            [re-frame.core        :as rf]
            [reitit.frontend.easy :as rfe]
            [reagent.dom          :as rdom]
            [logos.events         :as events]
            [logos.views          :as views]
            [logos.subs]
            ))

(defonce router
  (reitit/router
   [["/" {:name        :prover
          :view        #'views/proof-section
          :controllers [{:start (fn [_] (rf/dispatch [::events/init-proof-section]))}]}]
    ["/tutorial" {:name :tutorial
                  :view #'views/tutorial-page
                  :controllers
                  [{:start (fn [_]
                             (rf/dispatch
                              [::events/init-tutorial-section]))}]}]
    ["/formulas" {:name :formulas
                  :view #'views/formulas-page
                  :controllers
                  [{:start (fn [_]
                             (rf/dispatch
                              [::events/init-formulas-section]))}]}]
    ]))

(defn start-router! []
  (rfe/start!
   router
   views/navigate!
   {}))

;; -------------------------
;; Initialize app
(defn ^:dev/after-load mount-components []
  (rf/clear-subscription-cache!)
  (rdom/render [#'views/page] (.getElementById js/document "app")))

(defn init []
  (start-router!)
  (ajax/load-interceptors!)
  (mount-components))
