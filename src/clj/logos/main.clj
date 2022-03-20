(ns logos.main
  (:require [logos.formula :as f]
            [logos.proof.proof :as proof]
            [logos.proof.goal :as goal]
            [logos.proof.premise :as premise]
            [logos.proof.show :refer [show-proof]]
            [clojure.string :as string]))

(def current-proof (atom nil))
(def theorems      (atom (set [])))

(defn serialize-proof-formulas
  [proof]
  (let [relevant-premise-idxs (proof/relevant-premise-idxs proof)
        premises (mapv
                  (fn [idx]
                    (let [premise (-> proof
                                      (get ::proof/premises)
                                      (get idx))]
                      {:idx idx
                       :formula (-> premise
                                    (get ::proof/formula)
                                    f/to-string)
                       :justification
                       (get premise ::proof/justification)}))
                  relevant-premise-idxs)
        goal {:idx "SHOW: "
              :formula
              (-> proof
                  (get ::proof/problems)
                  (get (get
                        proof ::proof/current-problem))
                  (get ::proof/goal)
                  f/to-string)
              :justification ""}]
    (conj premises goal)))

(defn clear-current-proof
  []
  (reset! current-proof nil))

(defn start-proof
  [goal & {:keys [premises theorem-name]}]
  (let [args (cond-> {}
               (some? premises)
               (assoc :premises premises)
               (some? theorem-name)
               (assoc :theorem-name theorem-name))
        new-proof (apply proof/new-proof goal (-> args vec flatten))]
    (reset! current-proof new-proof)
        new-proof))



(defn one-step
  "Given a `proof` and an operation-`function`
  perform the corresponding operation on the proof.
  `args` is for the most part premise-ids, with the
  exception of the case of existential proof. In that case
  it is a list of substituents."
  [proof function & {:keys [args]}]
  (let [new-proof (if (nil? args)
                    (function proof)
                    (function proof args))]
    new-proof))

(defn parse-goal
  [string]
  (let [theorem-array   (-> string
                            string/trim
                            (string/split #" "))
        clean-array (filter #(not= "" %) theorem-array)
        theorem-declaration (string/lower-case
                             (first clean-array))
        title               (second clean-array)
        formula-string      (->> clean-array
                                 rest
                                 rest
                                 (string/join " "))]
    (when-not (= theorem-declaration "theorem")
      (throw
       (ex-info "First proof action must be \"theorem\""
                {:caused-by string})))
    (let [formula (read-string formula-string)]
      (if-not (f/formula? formula)
        (throw
         (ex-info
          (format "Cannot parse %s as a formula" formula)
          {:caused-by formula}))
        (start-proof formula :theorem-name title)))))

(defn execute-goal-operation [proof operation-string]
  (let [operation (-> operation-string
                      string/upper-case)
        function  (fn [fnct] (one-step proof fnct))]
    (case operation
      "DD"
      (function #'goal/direct-proof)
      "->P"
      (function #'goal/conditional-proof)
      "&P"
      (function #'goal/conjunctive-proof)
      "VP"
      (function #'goal/disjunctive-proof)
      "~P"
      (function #'goal/negative-proof)
      "UP"
      (function #'goal/universal-proof))))

(defn execute-premise-operation
  [proof command]
  (let [raw-operation (first command)
        operation (string/upper-case raw-operation)
        premise-idxs (mapv #'read-string (rest command))
        function  (fn [fnct]
                    (one-step
                     proof
                     fnct
                     :args premise-idxs))]
    (case operation
      "assert"
      (function #'premise/add-premise)
      "->E"
      (function #'premise/conditional-elimination)
      "&E"
      (function #'premise/conjunction-elimination)
      "VE"
      (function #'premise/disjunction-elimination)
      "BI"
      (function #'premise/bottom-introduction)
      "UE"
      (function #'premise/universal-elimination)
      "EE"
      (function #'premise/existential-elimination))))

(defn execute-existential-proof [proof substituent-string]
  (let [substituents (read-string (format "[%s]" substituent-string))]
    (one-step
     proof #'goal/existential-proof :args substituents)))

(defn execute-command
  [proof command]
  (let [remove-newlines (string/trim command)
        split-command   (->> #" "
                           (string/split remove-newlines)
                           (filter #(not= "" %)))]
    (cond (= (count split-command) 1)
          (execute-goal-operation proof (first split-command))
          (= (first split-command) "EP")
          ;; Existential Proof
          (execute-existential-proof
           proof (string/join " " (rest split-command)))
          (> (count split-command) 1)
          (execute-premise-operation proof split-command)
          :else
          (throw
           (ex-info
            (format "Could not parse %s as a command" command)
            {:caused-by command})))))

(defn next-step! [string]
  (if (nil? @current-proof)
    (let [proof (parse-goal string)]
      {:proof-string
       (show-proof proof)
       :proof proof
       :proof-formulas (serialize-proof-formulas proof)})
    (let [new-proof (execute-command @current-proof string)]
      (if (proof/proof-done? new-proof)
        (do
          (clear-current-proof)
          (swap! theorems (fn [ts] (conj ts new-proof))))
        (reset! current-proof new-proof))
      {:proof-string
       (show-proof new-proof)
       :proof new-proof
       :proof-formulas (serialize-proof-formulas new-proof)})))

(defn eval-commands!
  "Takes a string representing a set of commands
  and executes next-step! on them"
  [string]
  (let [commands (string/split string #"\.")]
    (loop [command (first commands)
           todo    (rest  commands)]
      (next-step! command)
      (when (seq todo)
        (recur (first todo)
               (rest  todo))))
    {:proof-string (show-proof @current-proof)
     :proof @current-proof
     :proof-formula (serialize-proof-formulas @current-proof)}))