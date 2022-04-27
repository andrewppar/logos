(ns logos.views
  (:require
   [clojure.string       :as string]
   [cljs-http.client     :as http]
   [cljs.core.async      :refer [<!]]
   [goog.string          :as gstring]
   [logos.events         :as events]
   [logos.subs           :as subs]
   [markdown.core        :as md]
   [reagent.core         :as r]
   [re-frame.core        :as rf])
  (:require-macros [cljs.core.async.macros :refer [go]]))

(defn nav-link [uri title page]
  [:a.navbar-item
   {:href   uri
    :class (when (= page @(rf/subscribe [:common/page-id])) :is-active)}
   title])

(defn navbar []
  (r/with-let [expanded? (r/atom false)]
    [:nav.navbar.is-info
     {:class "navbar is-info is-fixed-top"}
     [:div.container
      [:div.navbar-brand
       [:a.navbar-item {:href "/" :style {:font-weight :bold}} "λogos"]
       [:span.navbar-burger.burger
        {:data-target :nav-menu
         :on-click #(swap! expanded? not)
         :class (when @expanded? :is-active)}
        [:span][:span][:span]]]
      [:div#nav-menu.navbar-menu
       {:class (when @expanded? :is-active)}
       [:div.navbar-start
        [nav-link "#/tutorial" "Tutorial" :about]
        [nav-link "#/formulas" "Formulas" :formulas]
        ]]]]))

(defn toggle-check-box
  [id]
  (rf/dispatch [::events/toggle-check-box id]))

(defn print-proof-formulas
  [proof-formulas proof-string with-checkboxes? show-goal?]
  (let [premises (filter #(number? (:idx %)) proof-formulas)
        goals    (filter #(string? (:idx %)) proof-formulas)
        sorted-formulas (concat (sort-by :idx < premises) goals)]
    (if (= proof-string "QED")
      [:div proof-string]
      (loop [proof-formula (first sorted-formulas)
             todo          (rest sorted-formulas)
             result        [:table {:class "table is-bordered"}
                            [:tr
                             [:td "Number"]
                             [:td "Formula"]
                             [:td "Justification"]]]]
        (let [justification (get proof-formula :justification)
              id            (get proof-formula :idx)
              new-item      [:tr
                             [:td id

                              ;; TODO This is a little too close
                              ;; figure out how to get things spaced
                              ;; better
                              (when with-checkboxes?
                                (if (= justification "")
                                  ""
                                  [:label.checkbox
                                   {:style
                                     {:margin-left "0.5rem"}}
                                   [:input
                                    {:type "checkbox"
                                     :on-change
                                     #(toggle-check-box id)}]])
                                )]
                             [:td [:div
                                   {:style {:white-space "pre"}}
                                   (get proof-formula :formula)]]
                             [:td (if (= justification "")
                                    "λogos"
                                    (if (string? justification)
                                      justification
                                      (string/join
                                       "," justification)))]]
              new-result  (if (= justification "")
                            (if show-goal?
                              (conj result new-item)
                              result)
                            (conj result new-item))]
          (if (seq todo)
            (recur (first todo) (rest todo) new-result)
            new-result))))))

(defn start-proof
  [theorem-name formula]
  (rf/dispatch [::events/proof-init theorem-name formula]))

(defn next-proof
  [command proof]
  (rf/dispatch [::events/store-command command])
  (rf/dispatch [::events/next-command command proof]))

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
            proof          @(rf/subscribe [::subs/proof])
            premise-strings (string/join " " checked-boxes)
            new-command (str command " " premise-strings)]
        (clear-all-checkboxes)
        (next-proof new-command proof)))}
   label])

(def theorem-name (r/atom ""))
(def formula      (r/atom ""))

(defn clear-proof-button [clear-type]
  (let [style (case clear-type
                :clear  "button is-danger"
                :done   "button is-success")
        text  (case clear-type
                :clear "Clear Proof"
                :done  "Next Proof")
        justification (rf/subscribe [::subs/justification])]
    [:button
     {:class style
      :on-click
      (fn []
        (when (= clear-type :done)
          (rf/dispatch [::events/store-theorem
                        @theorem-name @formula @justification]))
        (reset! theorem-name "")
        (reset! formula "")
        (clear-all-checkboxes)
        (rf/dispatch [::events/clear-proof]))}
     text]))

(defn format-formula
  [old-formula]
    (go 
      (let [encoded-formula (gstring/urlEncode old-formula "UTF-8")
            base-url        "http://10.0.0.130:4000/format?formula="
            result (:body (<! (http/post (str base-url encoded-formula))))]
        (reset! formula (string/replace (->> 1
                                             (- (count result))
                                             (subs result 1))
                                        #"\\n" "\r\n")))))

(defn start-proof-section
  []
  [:section.section
   [:div
    {:class "container has-tooltip-multiline has-tooltip-bottom"
     :data-tooltip
     "Name your theorem and enter a formula to prove, e.g. \"(implies p p)\" or \"(not (and p (not p)))\""}
    [:table.table
     [:th]
     [:th]
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
         :rows "10"
         :value @formula
         :on-change #(reset! formula (.-value (.-target %)))}]]]]
    [:br]
    [:div.buttons
     [:button {:class "button is-info"
               :on-click #(start-proof @theorem-name @formula)}
      "Start Proof"]
     [:button {:class "button is-info"
               :on-click (fn [] (format-formula @formula))}
      "Format Formula"]
     [clear-proof-button :clear]]]])

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
  [id title tooltip body footer]
  (let [base-attr {:on-click #(rf/dispatch [::events/show-modal id])}
        class-str (if (= tooltip "")
                    "button is-info is-outlined is-rounded"
                    "button is-info is-outlined is-rounded has-tooltip-left has-tooltip-multiline")
        class-attr (assoc base-attr :class class-str)
        attr (if (= tooltip "")
               class-attr
               (assoc class-attr :data-tooltip tooltip))]
  [:div
   [:button
    attr
    title]
   [modal-card id title body footer]]))

(defn proof-step-input-button
  [label id command input-atom with-premises? with-vars? proof-formulas proof-string tooltip]
  (let [input-id (str id "-input")]
    [modal-button
     id label tooltip
     [:div.container
      (when with-premises?
        [print-proof-formulas proof-formulas proof-string true false])
      (when with-vars?
        [:div
         "Enter a list of values for variable"
         [:input.input
          {:id input-id
           :type "text"
           :value @input-atom
           :on-change #(reset! input-atom (.-value (.-target %)))}]])
      [:button
       {:class "button is-info is-outlined is-rounded"
        :on-click
        (fn []
          (let [input (when with-vars?
                        (-> js/document
                          (.getElementById input-id)
                          (.-value)))
                command (str
                         command
                         " "
                         (when with-premises?
                           (->> @(rf/subscribe
                                  [::subs/checked-boxes])
                                (string/join " ")))
                         " "
                         (when with-vars?
                           input))
                proof @(rf/subscribe [::subs/proof])]
            (rf/dispatch [::events/hide-modal id])
            (clear-all-checkboxes)
            (reset! input-atom "")
            (next-proof command proof)))}
       "Submit Command"]
      ]]))

(def ep-atom (r/atom ""))
(def conditional-elimination-atom (r/atom ""))
(def conjunction-elimination-atom (r/atom ""))
(def disjunction-elimination-atom (r/atom ""))
(def bottom-introduction-atom (r/atom ""))
(def existential-elimination-atom (r/atom ""))
(def assert-atom (r/atom ""))
(def universal-elimination-atom (r/atom ""))

(defn next-command-section
  [proof-formulas proof-string proof]
  (let [clear-type (if (= proof-string "QED") :done :clear)]
    [:div
     [:div.columns
      [:div.column
       {:class "column is-half"}
       [print-proof-formulas
        proof-formulas proof-string false true]]
      [:div
       {:class "column is-one-quarter"}
       [:div
        {:class "tile is-vertical is-child box"}
        [:h3 {:class "title is-h3"} "Goal Operations"]
        (map (fn [[command label tooltip]]
               [:div
                [:button
                 {:class
                  "button is-info is-outlined is-rounded has-tooltip-left has-tooltip-multiline"
                  :data-tooltip tooltip
                  :on-click
                  #(next-proof command proof)}
                 label]])
             [["DD"  "Direct Proof"
               "Finish the current proof because one of the premises
                matches the goal or bottom is in the premises."]
              ["->P" "Conditional Proof"
               "Prove a conditional formula by hypothesizing the
                antecedent and proving the consequent with a subproof."]
              ["&P"  "Conjunctive Proof"
               "Prove a conjunction by creating a subproof for each of
                the conjuncts."]
              ["VP"  "Disjunctive Proof"
               "Prove a disjunction because on of the disjuncts is in
                the premises."]
              ["~P"  "Negative Proof"
               "Prove a negation by hypothesizing the negatum
                and proving bottom with a subproof."]
              ["UP"  "Universal Proof"
               "Prove a universal formula by proving the quantified
                subformula with new constants substituted for all
                bound variables"]])
        [proof-step-input-button
         "Existential Proof" "ep" "EP" ep-atom false true
         proof-formulas proof-string
         "Open a dialog box that prompts for constants to substitute
         for variables. The quantified subformula with the specified
         substitutions is the next subproof. When it is closed the
         current proof is closed."]
        [proof-step-input-button
         "Assert" "assert" "ASSERT" assert-atom false true
         proof-formulas proof-string
         "Open a dialog box for a formula. This formula becomes the goal
         of a new subproof. Once proved the formula is available in the
         current proof as a premise."]]]
      [:div
       {:class "column is-one-quarter"}
       [:div
        {:class "tile is-vertical is-child box"}
        [:h3 {:class "title is-h3"} "Premise Operations"]
        (map (fn [[label id command atom with-vars? tooltip]]
               [proof-step-input-button
                label id command atom true with-vars? proof-formulas proof-string tooltip])
             [["Conditional Elimination" "conditional-elim" "->E"
               conditional-elimination-atom false
               "Open a dialog to select a conditional premise
                and its antecedent. The consequent of the conditional
                will be added to the premises of the proof."]
              ["Conjunction Elimination" "conjunction-elim" "&E"
               conjunction-elimination-atom false
               "Open a dialog to select a conjunction from the
                premises. The conjuncts of this premise will
                be added to the proof as premises."]
              ["Disjunction Elimination" "disjunction-elim" "VE"
               disjunction-elimination-atom false
               "Open a dialog to select a premise that is a
                disjunction from the premises. Each disjunct generates
                a new subproof, with the same goal as the current goal.
                If all subproofs are proved the current subproof closes."]
              ["Bottom Introduction"  "bottom-intro" "BI"
               bottom-introduction-atom false
               "Open a dialog box to select a premise and its negation.
               Bottom is added to the premises as a result."]
              ["Universal Elimination" "universal-elim" "UE"
               universal-elimination-atom true
               "Open a dialog box to select a universal premise and
                specify values for the variables. The quantified
                subformula with the specified substitutions will be
                added to the premises."]
              ["Existential Elimination" "existential-elim" "EE"
               existential-elimination-atom false
               "Select an existential premise. The quantified subformula
               will be added to the premises with all variables in the
               scope of the existential substituted for new constants."]
              ])
        ]]]
     [clear-proof-button clear-type]]))

(defn theorems-section
  [theorems]
  (r/with-let [theorems-hidden? (r/atom true)]
    [:section.section
     [:span {:class "arrow"
             :on-click #(swap! theorems-hidden? not)}
      [:font {:size "+2"} "Theorems "]
      [:font {:color "#209CEE"} (if @theorems-hidden? "show" "hide")]]
     (let [start [:table
                  {:class (str "table" " " (when @theorems-hidden?
                                             "is-hidden"))}
                  [:tr
                   [:td "Theorem Name"] [:td "Formula"] [:td "Justification"]]]
           names (keys theorems)]
       (loop [name (first names)
              todo (rest names)
              result start]
         (let [formula       (first  (get theorems name))
               justification (second (get theorems name))
               row [:tr [:td name] [:td [:div
                                         {:style {:white-space "pre"}}
                                         formula]] [:td justification]]
               updated-result (conj result row)]
           (if (seq todo)
             (recur (first todo) (rest todo) updated-result)
             updated-result))))
     ]))

(defn proof-section
  []
  (let [proof-formulas (rf/subscribe [::subs/proof-formulas])
        proof-string   (rf/subscribe [::subs/proof-string])
        proof          (rf/subscribe [::subs/proof])
        theorems       (rf/subscribe [::subs/theorems])]
    [:section.section
     (if (nil? @proof-formulas)
       [start-proof-section]
       [next-command-section @proof-formulas @proof-string @proof])
     [theorems-section @theorems]]))

(defn tutorial-page
  []
  [:section.section>div.container>dev.content
   (when-let [tutorial @(rf/subscribe [::events/tutorial-page])]
     [:div {:dangerouslySetInnerHTML {:__html (md/md->html tutorial)}}])])

(defn formulas-page
  []
  [:section.section>div.container>dev.content
   (when-let [formulas @(rf/subscribe [::events/formulas-page])]
     [:div {:dangerouslySetInnerHTML {:__html (md/md->html formulas)}}])])

(defn page []
  (when-let [page @(rf/subscribe [:common/page])]
    [:div
     {:class "has-navbar-fixed-top"}
     [navbar]
     [:div.section
      [page]]]))

(defn navigate! [match _]
  (rf/dispatch [:common/navigate match]))
