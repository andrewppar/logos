(ns logos.main
  (:require [logos.formula :as f]
            [logos.proof.proof :as proof]
            [logos.proof.goal :as goal]
            [logos.proof.premise :as premise]
            [logos.proof.show :refer [show-proof]]
            [clojure.string :as string]))

(def current-proof (atom nil))
(def theorems      (atom (set [])))

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
  [proof function & {:keys [premise-idxs]}]
  (let [new-proof (if (nil? premise-idxs)
                    (function proof)
                    (function proof premise-idxs))]
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
                     :premise-idxs premise-idxs))]
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
      (function #'premise/universal-elimination))))


(defn execute-command
  [proof command]
  (let [remove-newlines (string/trim command)
        split-command   (->> #" "
                           (string/split remove-newlines)
                           (filter #(not= "" %)))]
    (cond (= (count split-command) 1)
          (execute-goal-operation proof (first split-command))
          (> (count split-command) 1)
          (execute-premise-operation proof split-command)
          :else
          (throw
           (ex-info
            (format "Could not parse %s as a command" command)
            {:caused-by command})))))

(defn next-step! [string]
  (if (nil? @current-proof)
    (show-proof (parse-goal string))
    (let [new-proof (execute-command @current-proof string)]
      (if (proof/proof-done? new-proof)
        (do
          (clear-current-proof)
          (swap! theorems (fn [ts] (conj ts new-proof))))
        (reset! current-proof new-proof))
      (show-proof new-proof))))

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
    (show-proof @current-proof)))
