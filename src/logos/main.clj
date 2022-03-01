(ns logos.main
  (:require [logos.formula :as f]
            [logos.proof.proof :as proof]
            [logos.proof.goal :as goal]
            [logos.proof.premise :as premise]
            [logos.proof.show :refer [show-proof]]
            [clojure.string :as string]))

(def current-proof (atom nil))

(defn start-proof
  [goal & {:keys [premises]}]
  (let [new-proof (if (nil? premises)
                    (proof/new-proof premises goal)
                    (proof/new-proof goal))]
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
  (let [theorem-array   (string/split string #" ")
        clean-array (filter #(not= "" %) theorem-array)
        theorem-declaration (string/lower-case
                             (first clean-array))
        formula-string      (->> clean-array
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
        (start-proof formula)))))

(defn execute-goal-operation [proof operation-string]
  (let [operation (-> operation-string
                      string/upper-case
                      string/trim)
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
      (function #'goal/negative-proof))))

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
      (function #'premise/bottom-introduction))))


(defn execute-command
  [proof command]
  (let [split-command (->> #" "
                           (string/split command)
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

(defn prove
  "Takes a string representing a proof specification and runs
  executes that proof"
  [string]
  (let [commands (string/split string #"\.")
        proof    (parse-goal (first commands))]
    (reduce
     (fn [proof command]
       (execute-command proof command))
     proof (rest commands))))

(defn next-step [string]
  (if (nil? @current-proof)
    (show-proof (parse-goal string))
    (let [new-proof (execute-command @current-proof string)]
      (reset! current-proof new-proof)
      (show-proof new-proof))))
