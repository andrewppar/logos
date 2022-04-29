(ns logos.proof.premise
  (:require [logos.formula :as formula]
            [logos.proof.proof :as proof]))

(defn add-premise
  ([proof formula]
   (add-premise proof formula ::proof/assertion))
  ([proof formula justification]
   (let [problem-index (get proof ::proof/problems)
         current-problem-idx (get proof ::proof/current-problem)
         current-problem (get problem-index current-problem-idx)
         premise         (proof/new-premise formula justification)
         new-idx         (proof/get-new-premise-idx proof)
         new-problem     (update current-problem
                                 ::proof/premises (fn [x] (conj x new-idx)))
         new-premise-index (assoc (get proof ::proof/premises) new-idx premise)
         new-problem-index (assoc problem-index
                                  current-problem-idx new-problem)]
     (assoc proof
            ::proof/premises new-premise-index
            ::proof/problems new-problem-index))))

(defmacro ^:private ensure-premise-count
  [premise-vector number]
  `(when (not (= (count ~premise-vector) ~number))
     (throw (ex-info
             "Incorrect number of premises passed to rule"
             {:caused-by ~premise-vector}))))

(defn ^:private ensure-premises-relevant
  [proof premise-numbers]
  (let [relevant-premises (proof/relevant-premise-idxs proof)]
    (map (fn [idx]
           (when (not (some #{idx} relevant-premises))
             (throw
              (ex-info (format
                        "Premise %s is not relevant for current proof"
                        idx)
                       {:caused-by idx}))))
         premise-numbers)))

(defn conditional-elimination-internal
  [conditional proof premise-numbers premise-index]
   (let [consequent (formula/consequent conditional)
         problem-index   (get proof ::proof/problems)
         current-problem-idx (get proof ::proof/current-problem)
         current-problem (get problem-index current-problem-idx)
         current-premises (get current-problem ::proof/premises)
         new-premise-idx (proof/get-new-premise-idx proof)
         new-premise     (proof/new-premise
                          consequent premise-numbers)
         new-premise-index (assoc premise-index
                                  new-premise-idx new-premise)
         new-problem (->> new-premise-idx
                          (conj current-premises)
                          (assoc
                           current-problem ::proof/premises))
         new-problem-idx (assoc
                          problem-index
                          current-problem-idx
                          new-problem)]
     (assoc proof
            ::proof/premises new-premise-index
            ::proof/problems new-problem-idx)))


(defn conditional-elimination
  [proof premise-numbers]
  (ensure-premise-count premise-numbers 2)
  (ensure-premises-relevant proof premise-numbers)
  (let [premise-index (get proof ::proof/premises)
        premise-one   (-> premise-index
                          (get (Integer/parseInt (first premise-numbers)))
                          (get ::proof/formula))
        premise-two   (-> premise-index
                          (get (Integer/parseInt (second premise-numbers)))
                          (get ::proof/formula))]
    (cond (and (formula/implication? premise-one)
               (= (formula/antecedent premise-one) premise-two))
          (conditional-elimination-internal
           premise-one proof premise-numbers premise-index)
          (and (formula/implication? premise-two)
               (= (formula/antecedent premise-two) premise-one))
          (conditional-elimination-internal
           premise-two proof premise-numbers premise-index)
          :else
          proof)))

(defn ^:private add-formulas-to-premise-index
  [premise-index formulas start-idx justification]
  (loop [formula (first formulas)
         todo    (rest formulas)
         result  [premise-index []]
         idx     start-idx]
    (let [new-premise (proof/new-premise formula justification)]
      (if (seq todo)
        (recur (first todo)
               (rest todo)
               [(assoc (first result) idx new-premise)
                (conj (second result) idx)]
               (inc idx))
        [(assoc (first result) idx new-premise)
         (conj (second result) idx)]))))

(defn conjunction-elimination
  [proof premise-numbers]
  (ensure-premise-count premise-numbers 1)
  (ensure-premises-relevant proof premise-numbers)
  (let [premise-index (get proof ::proof/premises)
        formula       (-> premise-index
                          (get (Integer/parseInt (first premise-numbers)))
                          (get ::proof/formula))]
    (if (formula/conjunction? formula)
      (let [conjuncts (formula/conjuncts formula)
            idx       (proof/get-new-premise-idx proof)
            [new-premise-idx new-idxs]
            (add-formulas-to-premise-index
             premise-index conjuncts idx premise-numbers)
            problem-id    (get proof ::proof/current-problem)
            problem-index (get proof ::proof/problems)
            problem (get problem-index problem-id)
            new-problem (update problem
                                ::proof/premises
                                (fn [x] (concat x new-idxs)))
            new-problem-index (assoc problem-index problem-id new-problem)]
        (assoc proof
               ::proof/premises new-premise-idx
               ::proof/problems new-problem-index))
      proof)))

(defn disjunction-elimination
  [proof premise-numbers]
  (ensure-premise-count premise-numbers 1)
  (ensure-premises-relevant proof premise-numbers)
  (let [problem-index (get proof ::proof/problems)
        premise-index (get proof ::proof/premises)
        current-problem-idx (get proof ::proof/current-problem)
        current-goal  (-> problem-index
                          (get current-problem-idx)
                          (get ::proof/goal))
        target-premise      (->> premise-numbers
                                 first
                                 Integer/parseInt 
                                 (get premise-index)
                                 ::proof/formula)]
    (if (formula/disjunction? target-premise)
      (let [disjuncts         (formula/disjuncts target-premise)
            premise-idx-start (->> premise-index
                                   keys
                                   (apply max)
                                   inc)
            premise-idxs      (->> disjuncts
                                   count
                                   range
                                   (map
                                    (fn [offset]
                                      (+ premise-idx-start offset))))
            new-premise-index (proof/add-hypotheses-to-index
                               premise-index disjuncts)
            new-problems      (map
                               (fn [premise-idx]
                                 (proof/new-problem
                                  [premise-idx] current-goal nil))
                               premise-idxs)
            last-problem-id (apply max (keys problem-index))]
        (-> proof
            (assoc ::proof/current-problem (inc last-problem-id))
            (assoc ::proof/premises new-premise-index)
            (proof/add-new-problems-to-proof
             new-problems current-problem-idx)))
      proof)))

(defn bottom-introduction
  [proof premise-numbers]
  (ensure-premise-count premise-numbers 2)
  (ensure-premises-relevant proof premise-numbers)
  (let [current-prblm-idx (get proof ::proof/current-problem)
        problem-index     (get proof ::proof/problems)
        problem           (get problem-index current-prblm-idx)
        premise-index     (get proof ::proof/premises)
        premise-one-idx   (Integer/parseInt (first premise-numbers))
        premise-two-idx   (Integer/parseInt (second premise-numbers))
        premise-one       (-> premise-index
                              (get premise-one-idx)
                              (get ::proof/formula))
        premise-two       (-> premise-index
                              (get premise-two-idx)
                              (get ::proof/formula))]
    (if (or (= premise-one (formula/negatum premise-two))
            (= premise-two (formula/negatum premise-one)))
      (add-premise proof ::formula/bottom premise-numbers)
      proof)))

(defn create-variable-map
  [variables constants]
  (if (not= (count variables)
            (count constants))
    (throw
     (ex-info
      "Cannot  create variable map for different number of variables and constants"
      {:caused-by `(not= (count ~variables)
                         (count ~constants))}))
    (loop [var            (first variables)
           todo-vars      (rest variables)
           constant       (first constants)
           todo-constants (rest constants)
           result         {}]
      (let [new-result (assoc result var constant)]
        (if (seq todo-vars)
          (recur (first todo-vars)
                 (rest todo-vars)
                 (first todo-constants)
                 (rest todo-constants)
                 new-result)
          new-result)))))

(defn universal-elimination [proof args]
  (ensure-premises-relevant proof [(first args)])
  (let [premise-index (get proof ::proof/premises)
        formula       (-> premise-index
                          (get (Integer/parseInt (first args)))
                          (get ::proof/formula))
        new-constants (map formula/read-formula (rest args))]
    (if (formula/universal? formula)
      (let [bound-vars        (formula/bound-variables formula)
            variable-map      (create-variable-map
                               bound-vars new-constants)
            new-formula       (formula/substitute-free-variables
                               (formula/quantified-subformula formula)
                               variable-map)
            new-premise       (proof/new-premise new-formula args)
            new-premise-idx   (proof/get-new-premise-idx proof)
            new-premise-index (assoc premise-index
                                     new-premise-idx new-premise)
            current-problems  (get proof ::proof/problems)
            current-prblm-idx (get proof ::proof/current-problem)
            current-problem   (get current-problems current-prblm-idx)
            new-problem       (update current-problem
                                      ::proof/premises
                                      (fn [premises]
                                        (conj
                                         premises new-premise-idx)))
            new-problems      (assoc current-problems
                                     current-prblm-idx new-problem)]
        (assoc proof
               ::proof/premises new-premise-index
               ::proof/problems new-problems))
      proof)))

(defn existential-elimination
  [proof premise-numbers]
  (ensure-premise-count premise-numbers 1)
  (ensure-premises-relevant proof premise-numbers)
  (let [premises (get proof ::proof/premises)
        premise  (->> premise-numbers
                      first
                      Integer/parseInt
                      (get premises))
        formula  (get premise ::proof/formula)]
    (if (formula/existential? formula)
      (let [problem-idx (get proof ::proof/current-problem)
            problem     (-> proof
                            (get ::proof/problems)
                            (get problem-idx))
            used-constants  (->> proof
                                 proof/relevant-premises
                                 (mapcat
                                  (fn [formula]
                                    (formula/formula-gather
                                     formula
                                     (fn [obj]
                                       (or
                                        (formula/constant? obj)
                                        (formula/atomic-predicate? obj)))))))
            new-formula     (formula/instantiate-new-variables
                             formula used-constants)
            new-premise     (proof/new-premise
                             new-formula premise-numbers)
            new-premise-idx (proof/get-new-premise-idx proof)
            new-problem     (update problem
                                    ::proof/premises
                                    (fn [premises]
                                      (conj premises new-premise-idx)))
            new-problems    (assoc (get proof ::proof/problems)
                                   problem-idx new-problem)
            new-premises    (assoc premises
                                   new-premise-idx new-premise)]
        (assoc proof
               ::proof/problems new-problems
               ::proof/premises new-premises))
      proof)))


(defn id-substitute [proof premise-numbers] nil)

(defn beta-reduction [proof premise-numbers] nil)

;;; Lambdas...
