(ns logos.main
  (:require [clojure.edn :as edn]
            [clojure.pprint :as pp]
            [clojure.string :as string]
            [logos.formula :as f]
            [logos.proof.goal :as goal]
            [logos.proof.premise :as premise]
            [logos.proof.proof :as proof]
            [logos.proof.show :refer [show-proof]])
  (:import java.util.Base64))

(defn encode [to-encode]
  (.encodeToString (Base64/getEncoder) (.getBytes to-encode)))

(defn decode [to-decode]
  (String. (.decode (Base64/getDecoder) to-decode)))

(defn format-formula [formula]
  (if-not (string? formula)
    formula
    (binding [pp/*print-pretty* true
              pp/*print-miser-width* nil
              pp/*print-right-margin* 60]
      (with-out-str
        (pp/pprint (f/read-formula-string formula))))))

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
                                    f/to-string
                                    format-formula)
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
                  f/to-string
                  format-formula)
              :justification ""}]
    (conj premises goal)))

;;;;;;;;;;;;;
;;; New Proof

(defn start-proof-internal
  [goal & {:keys [premises theorem-name]}]
  (let [args (cond-> {}
               (some? premises)
               (assoc :premises premises)
               (some? theorem-name)
               (assoc :theorem-name theorem-name))
        new-proof (apply proof/new-proof goal (-> args vec flatten))]
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
    (let [formula (f/read-formula formula-string)]
      (if-not (f/formula? formula)
        (throw
         (ex-info
          (format "Cannot parse %s as a formula" formula)
          {:caused-by formula}))
        (start-proof-internal formula :theorem-name title)))))

(defn start-proof
  [string]
  (let [proof (parse-goal string)]
      {:proof-string
       (show-proof proof)
       :proof (encode (str proof))
       :proof-formulas (serialize-proof-formulas proof)}))

;;;;;;;;;;;;;;;;
;;; Within Proof

(defn one-step
  "Given a `proof` and an operation-`function`
  perform the corresponding operation on the proof.
  `args` is for the most part premise-ids, with the
  exception of the case of existential proof. In that case
  it is a list of substituents."
  [raw-proof function & {:keys [args]}]
  (let [proof (edn/read-string (decode raw-proof))
        new-proof (if (nil? args)
                    (function proof)
                    (function proof args))]
    new-proof))

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
        premise-idxs (map str (edn/read-string (format "[%s]" (clojure.string/join " " (rest command)))))
        function  (fn [fnct]
                    (one-step
                     proof
                     fnct
                     :args premise-idxs))]
    (case operation
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
      (function #'premise/existential-elimination)
      "S"
      (function #'premise/substitute-equality))))

(defn assert-formula-to-proof [proof formula-string]
  (let [formula (f/read-formula formula-string)]
    (one-step proof #'goal/assert :args formula)))

(defn execute-existential-proof [proof substituent-string]
  (let [substituents (->> substituent-string
                          (format "[%s]")
                          edn/read-string
                          (map str))]
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
          ;; Existential Proof
          (= (first split-command) "EP")
          (execute-existential-proof
           proof (string/join " " (rest split-command)))
          ;; Assert
          (= (first split-command) "ASSERT")
          (assert-formula-to-proof
           proof (string/join " " (rest split-command)))
          (> (count split-command) 1)
          (execute-premise-operation proof split-command)
          :else
          (throw
           (ex-info
            (format "Could not parse %s as a command" command)
            {:caused-by command})))))

(defn next-step [string proof]
  (let [new-proof (execute-command proof string)]
    {:proof-string
     (show-proof new-proof)
     :proof (encode (str new-proof))
     :proof-formulas (serialize-proof-formulas new-proof)}))

(defn eval-commands!
  "Takes a string representing a set of commands
  and executes next-step! on them"
  [string proof]
  (let [commands (string/split string #"\.")
        new-proof (loop [command (first commands)
                         todo    (rest  commands)
                         last-proof  proof]
                    (let [new-proof (next-step command last-proof)]
                      (if (seq todo)
                        (recur (first todo)
                               (rest  todo)
                               new-proof)
                        new-proof)))]
    {:proof-string (show-proof new-proof)
     :proof (encode (str new-proof))
     :proof-formula (serialize-proof-formulas new-proof)}))
