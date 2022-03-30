(ns logos.events
  (:require
   [ajax.core     :as ajax]
   [day8.re-frame.http-fx]
   [goog.string   :as gstring]
   [logos.db      :as db]
   [re-frame.core :as rf]))

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
 (fn [_ [_ theorem-name formula]]
   (let [encoded-command (-> "Theorem "
                             (str theorem-name " " formula)
                             (gstring/urlEncode "UTF-8"))]
     {:http-xhrio {:uri (str
                         "http://10.0.0.130:4000/start-proof?theorem="
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
     {:http-xhrio {:uri "http://10.0.0.130:4000/one-step"
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

   (println proof-map)
   (assoc db
          :proof-string (:proof-string proof-map)
          :proof  (:proof proof-map)
          :proof-formulas (:proof-formulas proof-map))))

(rf/reg-event-fx
 ::proof-init
 (fn [db [_ theorem-name formula]]
   (println db)
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
   (assoc db :error error)))

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
 ::format-formula
 (fn [_ [_ formula]]
   {:dispatch [::format-formula-internal formula]}))

(rf/reg-event-fx
 ::format-formula-internal
 (fn [_ [_ formula]]
   {:http-xhrio {:uri (str "http://10.0.0.130:4000/format?formula="
                           (gstring/urlEncode formula "UTF-8"))
                 :method :post
                 :format (ajax/transit-request-format)
                 :response-format (ajax/json-response-format {:keywords? true})
                 :on-success [::add-format-formula]
                 :on-failure [::set-error]}}))

(rf/reg-event-db
 ::add-format-formula
 (fn [db [_ formula]]
   (assoc db :formatted-formula formula)))

(rf/reg-event-db
 ::store-theorem
 (fn [db [_ name formula]]
   (update db
           :theorems
           (fn [theorem-map]
                (assoc theorem-map name formula)))))
