(ns logos.events
  (:require
   [ajax.core                   :as ajax]
   [clojure.string              :as string]
   [day8.re-frame.http-fx]
   [goog.string                 :as gstring]
   [logos.db                    :as db]
   [re-frame.core               :as rf]
   [reitit.frontend.easy        :as rfe]
   [reitit.frontend.controllers :as rfc])
  (:require-macros [adzerk.env  :as env]))

(env/def LOGOS_SERVER "127.0.0.1")

;; dispatchers
(rf/reg-event-db
  :common/navigate
  (fn [db [_ match]]
    (let [old-match (:common/route db)
          new-match (assoc match :controllers
                           (rfc/apply-controllers
                            (:controllers old-match) match))]
      (assoc db :common/route new-match))))

(rf/reg-fx
  :common/navigate-fx!
  (fn [[k & [params query]]]
    (rfe/push-state k params query)))

(rf/reg-event-fx
  :common/navigate!
  (fn [_ [_ url-key params query]]
    {:common/navigate-fx! [url-key params query]}))

(rf/reg-event-db
 ::initialize-db
 (fn [_ _]
   db/default-db))

(rf/reg-event-db
 ::name-change
 (fn [db [_ new-name-value]]
   (assoc db :name new-name-value)))


(rf/reg-event-fx
 ::fetch-proof-start
 (fn [_ [_ raw-theorem-name formula]]
   (let [theorem-name (-> raw-theorem-name
                          (string/replace #" " "-")
                          string/lower-case)
         encoded-command (-> "Theorem "
                             (str theorem-name " " formula)
                             (gstring/urlEncode "UTF-8"))]
     {:http-xhrio {:uri (str
                         "http://" LOGOS_SERVER
                         ":4000/start-proof?theorem="
                         encoded-command)
                   :method :get
                   :format (ajax/transit-request-format)
                   :response-format (ajax/json-response-format
                                     {:keywords? true})
                   :on-success [::update-proof]
                   :on-failure [::set-error]}})))

(rf/reg-event-fx
 ::fetch-next-command
 (fn [_ [_ command proof]]
   (let [params (str {:body {:command command :proof proof}})]
     {:http-xhrio {:uri (str "http://" LOGOS_SERVER ":4000/one-step")
                   :method :post
                   :params params
                   :format (ajax/transit-request-format)
                   :response-format (ajax/json-response-format
                                     {:keywords? true})
                   :on-success [::update-proof]
                   :on-failure [::set-error]}})))

(rf/reg-event-db
 ::update-proof
 (fn [db [_ proof-map]]
   (assoc db
          :proof-string (:proof-string proof-map)
          :proof  (:proof proof-map)
          :proof-formulas (:proof-formulas proof-map))))

(rf/reg-event-fx
 ::proof-init
 (fn [db [_ theorem-name formula]]
   {:dispatch [::fetch-proof-start theorem-name formula]}))

(rf/reg-event-fx
 ::next-command
 (fn [_ [_ command proof]]
   {:dispatch [::fetch-next-command command proof]}))

(rf/reg-event-db
 ::store-command-db
 (fn [db [_ command]]
   (update db
           :proof-commands
           (fn [previous] (str previous command ". ")))))

(rf/reg-event-db
 ::clear-last-proof-command
 (fn [db _]
   (update db
           :proof-commands
           (fn [previous]
             (let [commands  (string/split previous #"\.")]
               (str
                (->> commands
                     reverse
                     (drop 2)
                     reverse
                     (string/join "."))
                ". "))))))

(rf/reg-event-fx
 ::store-command
 (fn [_ [_ command]]
   {:dispatch [::store-command-db command]}))


(rf/reg-event-db
 ::clear-proof-internal
 (fn [db _]
   (dissoc db
           :proof-string
           :proof
           :proof-formulas
           :proof-commands)))

(rf/reg-event-fx
 ::clear-proof
 (fn [_ _]
   {:dispatch [::clear-proof-internal]}))

(rf/reg-event-db
 ::set-error
 (fn [db [_ error]]
   (assoc db :error (get-in error [:response :caused-by]))))

(rf/reg-event-db
 ::clear-error
 (fn [db _]
   (dissoc db :error)))

(rf/reg-event-db
 ::toggle-check-box
 (fn [db [_ id]]
   (let [checked-boxes (get db :checked-boxes)]
     (if (some #{id} checked-boxes)
       (->> checked-boxes
            (remove #{id})
            (into #{})
            (assoc db :checked-boxes))
       (update db :checked-boxes conj id)))))

(rf/reg-event-db
 ::clear-checked-boxes
 (fn [db _]
   (assoc db :checked-boxes #{})))

(rf/reg-event-db
 ::show-modal
 (fn [db [_ modal-id]]
   (assoc-in db [:active-modals modal-id] true)))

(rf/reg-event-db
 ::hide-modal
 (fn [db [_ modal-id]]
   (update db :active-modals dissoc modal-id)))


(rf/reg-event-fx
 ::format-formula-internal
 (fn [_ [_ formula]]
   {:http-xhrio {:uri (str "http://" LOGOS_SERVER ":4000/format?formula="
                           (gstring/urlEncode formula "UTF-8"))
                 :method :post
                 :format (ajax/transit-request-format)
                 :response-format (ajax/json-response-format {:keywords? true})
                 :on-success [::add-format-formula]
                 :on-failure [::set-error]}}))

(rf/reg-event-fx
 ::format-formula
 (fn [_ [_ formula]]
   {:dispatch [::format-formula-internal formula]}))

(rf/reg-event-db
 ::add-format-formula
 (fn [db [_ formula]]
   (assoc db :formatted-formula formula)))

(rf/reg-event-db
 ::store-theorem
 (fn [db [_ name formula justification]]
   (update db
           :theorems
           (fn [theorem-map]
             (assoc theorem-map name [formula justification])))))

(rf/reg-event-db
 :set-proof-section
 (fn [db _]
   (assoc db :proof-section "")))

(rf/reg-event-fx
 ::init-proof-section
 (fn [_ _]
   {:dispatch [:fetch-proof-section]}))

(rf/reg-event-fx
 :fetch-proof-section
 (fn [_ _]
   {:http-xhrio {:method :get
                 :uri    (str "http://" LOGOS_SERVER ":4000/health-check")
                 :response-format
                 (ajax/raw-response-format)
                 :on-success [:set-proof-section]}}))

(rf/reg-event-fx
 ::tutorial-internal
 (fn [_ _]
   {:http-xhrio {:uri (str "http://" LOGOS_SERVER ":4000/tutorial")
                 :method :get
                 :format (ajax/transit-request-format)
                 :response-format (ajax/json-response-format
                                   {:keywords? true})
                 :on-success [::store-tutorial]
                 :on-failure [::set-error]}}))

(rf/reg-event-fx
 ::init-tutorial-section
 (fn [_ _]
   {:dispatch [::tutorial-internal]}))

(rf/reg-event-db
 ::store-tutorial
 (fn [db [_ tutorial-md]]
   (assoc db :tutorial tutorial-md)))

(rf/reg-sub
 ::tutorial-page
 (fn [db _]
   (-> db :tutorial)))

(rf/reg-event-fx
 ::formulas-internal
 (fn [_ _]
   {:http-xhrio {:uri (str "http://" LOGOS_SERVER ":4000/formulas")
                 :method :get
                 :format (ajax/transit-request-format)
                 :response-format (ajax/json-response-format
                                   {:keywords? true})
                 :on-success [::store-formulas]
                 :on-failure [::set-error]}}))

(rf/reg-event-fx
 ::init-formulas-section
 (fn [_ _]
   {:dispatch [::formulas-internal]}))

(rf/reg-event-db
 ::store-formulas
 (fn [db [_ formulas-md]]
   (assoc db :formulas formulas-md)))

(rf/reg-sub
 ::formulas-page
 (fn [db _]
   (-> db :formulas)))

(rf/reg-sub
  :common/route
  (fn [db _]
    (-> db :common/route)))

(rf/reg-sub
  :common/page-id
  :<- [:common/route]
  (fn [route _]
    (-> route :data :name)))

(rf/reg-sub
  :common/page
  :<- [:common/route]
  (fn [route _]
    (-> route :data :view)))

(rf/reg-event-db
 ::home
 (fn [db _]
   (assoc db :home "")))
