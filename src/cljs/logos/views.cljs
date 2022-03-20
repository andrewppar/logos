(ns logos.views
  (:require
   [logos.subs      :as subs]
   [logos.events    :as events]
   [reagent.core    :as r]
   [re-frame.core   :as rf]
   [clojure.string  :as string]))

(defn toggle-check-box
  [id]
  (rf/dispatch [::events/toggle-check-box id]))

(defn print-proof-formulas
  [proof-formulas proof-string]
  (let [premises (filter #(number? (:idx %)) proof-formulas)
        goals    (filter #(string? (:idx %)) proof-formulas)
        sorted-formulas (concat (sort-by < :idx premises) goals)]
  (if (= proof-string "QED")
    [:div proof-string]
    (loop [proof-formula (first sorted-formulas)
           todo          (rest sorted-formulas)
           result        [:table {:class "table is-bordered"}]]
      (let [justification (get proof-formula :justification)
            id            (get proof-formula :idx)
            new-result    (conj result
                                [:tr
                                 [:td
                                  (if (= justification "")
                                    ""
                                    [:label.checkbox
                                     [:input
                                      {:type "checkbox"
                                       :on-change
                                       #(toggle-check-box id)}]])]
                                 [:td id]
                                 [:td (get proof-formula :formula)]
                                 [:td (if (= justification "")
                                        "λoɣos"
                                        (if (string? justification)
                                          justification
                                          (string/join "," justification)))]])]
        (if (seq todo)
          (recur (first todo) (rest todo) new-result)
          new-result))))))

(defn start-proof
  [theorem-name formula]
  (rf/dispatch [::events/proof-init theorem-name formula]))

(defn next-proof
  [command]
  (println command)
  (rf/dispatch [::events/next-command command]))

(defn clear-all-checkboxes
  []
  (mapv #(set! (.-checked %) false)
        (.getElementsByTagName js/document "input"))
  (rf/dispatch [::events/clear-checked-boxes]))

(defn premise-operation
  [command label]
  ;; @TODO make button error if wrong number of premises
  ;; are selected for command
  [:button
   {:class "button is-info is-outlined is-rounded"
    :on-click
    (fn []
      (let [checked-boxes  @(rf/subscribe [::subs/checked-boxes])
            premise-strings (string/join " " checked-boxes)
            new-command (str command " " premise-strings)]
        (clear-all-checkboxes)
        (next-proof new-command)))}
   label])

(def theorem-name (r/atom ""))
(def formula      (r/atom ""))

(defn clear-proof-button []
  [:button
   {:class "button is-danger"
    :on-click
    (fn []
      (reset! theorem-name "")
      (reset! formula "")
      (clear-all-checkboxes)
      (rf/dispatch [::events/clear-proof]))}
   "Clear Proof"])

(defn start-proof-section
  []
  [:div.container
   [:table.table
    [:th] [:th]
    [:tr
     [:td
      "Theorem Name: "]
     [:td
      [:input.input
       {:id "theorem-name"
        :type "text"
        :value @theorem-name
        :on-change #(reset! theorem-name (.-value (.-target %)))}]]]
    [:tr
     [:td
      "Formula: "]
     [:td
      [:textarea.textarea
       {:id "theorem-formula"
        :type "text"
        :cols "100"
        :value @formula
        :on-change #(reset! formula (.-value (.-target %)))}]]]]
   [:br]
   [:div.buttons
    [:button {:class "button is-info"
              :on-click #(start-proof @theorem-name @formula)}
     "Start Proof"]
    [clear-proof-button]]])

;;;;;;;;;;
;;; TODO:
;;  Add Theorems Section

(defn modal-card
  [id title body footer]
  [:div.modal
   {:class (when @(rf/subscribe [::subs/modal-showing? id])
             "is-active")}
   [:div.modal-background
    {:on-click #(rf/dispatch [::events/hide-modal id])}]
   [:div.modal-card
    [:header.modal-card-head
     [:p.modal-card-title title]
     [:button.delete
      {:on-click #(rf/dispatch [::events/hide-modal id])}]]
    [:section.modal-card-body body]
    [:footer.modal-card-foot footer]]])

(defn modal-button
  [id title body footer]
  [:div
   [:button
    {:class "button is-info is-outlined is-rounded"
     :on-click #(rf/dispatch [::events/show-modal id])}
    title]
   [modal-card id title body footer]])

(defn button-with-input
  [label id command input-atom with-premises?]
  (let [input-id (str id "-input")]
    [modal-button
     id label
     [:div.container
      "Enter a list of values for variable"
      [:input.input
       {:id input-id
        :type "text"
        :value @input-atom
        :on-change #(reset! input-atom (.-value (.-target %)))}]
      [:button
       {:class "button is-info is-outlined is-rounded"
        :on-click
        (fn []
          (let [input (-> js/document
                          (.getElementById input-id)
                          (.-value))
                command (str
                         command
                         " "
                         (when with-premises?
                           (->> @(rf/subscribe
                                  [::subs/checked-boxes])
                                (string/join " ")))
                         " "
                         input)]
            (rf/dispatch [::events/hide-modal id])
            (clear-all-checkboxes)
            (next-proof command)))}
       "Submit Command"]
      ]]))

(def ep-atom (r/atom ""))
(def ue-atom (r/atom ""))

(defn next-command-section
  [proof-formulas proof-string]
  [:div
   [:div.columns
    [:div.column
     {:class "column is-half"}
     [print-proof-formulas proof-formulas proof-string]]
    [:div
     {:class "column is-one-quarter"}
     [:h3 {:class "title is-h3"} "Goal Operations"]
     [:div.buttons
      (map (fn [[command label]]
             [:div
              [:button
               {:class
                "button is-info is-outlined is-rounded"
                :on-click
                #(next-proof command)}
               label]])
           [["DD"  "Direct Proof"]
            ["->P" "Conditional Proof"]
            ["&P"  "Conjunctive Proof"]
            ["VP"  "Disjunctive Proof"]
            ["~P"  "Negative Proof"]
            ["UP"  "Universal Proof"]])
      [button-with-input "Existential Proof" "ep" "EP" ep-atom false]]]
    [:div
     {:class "column is-one-quarter"}
     [:h3 {:class "title is-h3"} "Premise Operations"]
     [:div.buttons
      (map (fn [[command label]]
             [premise-operation command label])
           [["->E" "Conditional Elimination"]
            ["&E" "Conjunction Elimination"]
            ["VE" "Disjunction Elimination"]
            ["BI" "Bottom Introduction"]
            ["EE" "Existential Elimination"]])
      [button-with-input
       "Universal Elimination" "ue" "UE" ue-atom true]
      ]]]
   [clear-proof-button]])

(defn proof-section
  []
  (let [proof-formulas (rf/subscribe [::subs/proof-formulas])
        proof-string   (rf/subscribe [::subs/proof-string])]

    [:div
     [:br]
     [:h3
      {:class "title is-3 is-spaced"}
      "Proof"]
     [:div
    (if (nil? @proof-formulas)
      [start-proof-section]
      [next-command-section @proof-formulas @proof-string])]]))


(defn main-panel []
  (let [error (rf/subscribe [::subs/error])]
    (when @error
      (rf/dispatch [::events/show-modal "error"]))
  [:div.container
   [:section
    {:class "hero is-info"}
    [:div.hero-body
     [:p.title "Logos"]]]
   [proof-section]
   [modal-card "error" "Error"
    [:div (str @error)]]]))
