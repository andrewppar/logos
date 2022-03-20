(ns logos.subs
  (:require [re-frame.core :as rf]))

(rf/reg-sub
 ::name
 (fn [db]
   (get db :name)))

(rf/reg-sub
 ::test
 (fn [db]
   (get db :test)))

(rf/reg-sub
 ::proof
 (fn [db]
   (get db :proof)))

(rf/reg-sub
 ::proof-string
 (fn [db]
   (get db :proof-string)))

(rf/reg-sub
 ::proof-formulas
 (fn [db]
   (get db :proof-formulas)))

(rf/reg-sub
 ::error
 (fn [db]
   (get db :error)))

(rf/reg-sub
 ::active-modals
 (fn [db _]
   (get db :active-modals {})))

(rf/reg-sub
 ::modal-showing?
 :<- [::active-modals]
 (fn [modals [_ modal-id]]
   (get modals modal-id false)))

(rf/reg-sub
 ::checked-boxes
 (fn [db]
   (get db :checked-boxes)))
