(ns logos.proof.premise
  (:require [logos.formula :as formula]
            [logos.proof.proof :as proof]))

(defn add-premise
  "Add `formula` to `proof` as a new premise, optionally supply a
  justification. The default justification is ::proof/assertion"
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

(defn add-premises
  "Note: This functin assumes that formulas are strings representing
   formulas
  TODO: This parse-formula should be moved up to main so it can be
  used more generally."
  [proof formulas]
  (reduce
   (fn [result formula]
     (add-premise result (formula/read-formula formula) ::proof/premise))
   proof formulas))

(defn ^:private ensure-premise-count
  "Ensure that the number of premises for the rule matches the expected
   number of premises for that rule"
  [rule-name premise-vector number]
  (when (not (= (count premise-vector) number))
    (throw (ex-info
            "Incorrect number of premises passed to rule"
            {:caused-by (format "%s requires %s premises but given %s"
                                rule-name number (count premise-vector))}))))

(defn ^:private ensure-premises-relevant
  "Ensure that the premise numbers supplied to a proof are actually
   premises in that proof"
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

(defn ^:private conditional-elimination-internal
  "Handle conditinal elimination having determined the major and minor
  premises."
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
  "Given a conditional and it's antecedent as premises add the
  consequent of the conditional as a new premise."
  [proof premise-numbers]
  (ensure-premise-count "Conditional elimination" premise-numbers 2)
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
  "Given a premise index, a list of formulas, a number to start from,
  and a justification - add numbered premises to the premise index
  starting from the start index"
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
  "Given a proof with a conjunction add the conjuncts of that conjunction
  to the premises of the proof."
  [proof premise-numbers]
  (ensure-premise-count "Conjunction elimination" premise-numbers 1)
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
  "Given a proof with a disjunction, for each disjunct create a new
  subproof that has that disjunct as a premise and the same goal as
  the original proof"
  [proof premise-numbers]
  (ensure-premise-count "Disjunction elimination" premise-numbers 1)
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
  "Given a contradiction introduce the special formula ::formula/bottom
  to a proof as a premise."
  [proof premise-numbers]
  (ensure-premise-count "Bottom introduction" premise-numbers 2)
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

(defn ^:private create-variable-map
  "Given a list of variables and constants create a map whose keys are
  variables and whose values are constants in the original order."
  [variables constants]
  (if (not= (count variables)
            (count constants))
    (throw
     (ex-info
      "Cannot  create variable map for different number of variables and constants"
      {:caused-by `(not= (count ~variables)
                         (count ~constants))}))
    ;; TODO - Couldn't this just be zipmap - make sure we have test
    ;; coverage before updating
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

(defn universal-elimination
  "Given a universal formula in a proof subsitute the constants
  specified by `args` for the variables in that formula bound by the
  universal quantifier."
  [proof args]
  (ensure-premise-count "Universal Elimination" [(first args)] 1)
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
  "Given an existential premise, add as another premise the immediate
  subformula of that premise with all the bound varables substituted
  for new constants."
  [proof premise-numbers]
  (ensure-premise-count "Existential elimination" premise-numbers 1)
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

;; TODO: Instead of positions consider using a premise number to identify
;; the substitued and the equalities - then use formula-gather?
(defn substitute-equality-internal
  "Substitute one term in an atomic formula for another as definted
  by an !equals statement."
  [proof equality-id atomic-id init-substitution-positions]
  (let [premises               (get proof ::proof/premises)
        substitution-positions (map (fn [pos]
                                      (Integer/parseInt pos))
                                    init-substitution-positions)
        equality-premise       (-> premises
                                   (get equality-id)
                                   (get ::proof/formula))
        atomic-premise         (-> premises
                                   (get atomic-id)
                                   (get ::proof/formula))
        substitution-terms    (map
                               (fn [arg-id]
                                 (if (= arg-id 0)
                                   atomic-premise
                                   (get atomic-premise arg-id)))
                                substitution-positions)]
    (when-not (= (count (set substitution-terms)) 1)
      (throw
       (ex-info "Cannot substitute different terms with \"!equals\""
                {:caused-by (str "Cannot substitute different terms"
                                 (format " with \"!equals\": %s"
                                         (set substitution-terms)))})))
    (let [substituted (first substitution-terms)
          substituent (first
                       (filter (fn [arg] (not= substituted arg))
                               (formula/terms equality-premise)))
          new-formula (if (= (set substitution-positions) #{0})
                        substituent
                        (loop [arg (first atomic-premise)
                               idx 0
                               todo (rest atomic-premise)
                               result []]
                          (let [new-result (if (some
                                                #{idx}
                                                substitution-positions)
                                             (conj result substituent)
                                             (conj result arg))]
                            (if (seq todo)
                              (recur (first todo) (inc idx) (rest todo) new-result)
                              new-result))))
          new-premise (proof/new-premise new-formula
                                         [atomic-id equality-id])
          new-premise-idx (proof/get-new-premise-idx proof)
          premises    (get proof ::proof/premises)
          new-premises (assoc premises new-premise-idx new-premise)
          problems        (get proof ::proof/problems)
          current-problem-idx (get proof ::proof/current-problem)
          current-problem (get problems current-problem-idx)
          new-problem     (update current-problem ::proof/premises
                                  conj new-premise-idx)
          new-problems    (assoc problems current-problem-idx new-problem)]
      (-> proof
          (assoc ::proof/premises
                 new-premises)
          (assoc ::proof/problems
                 new-problems)))))

(defn substitute-equality
  "Substitute one term in an atomic formula for another as definted
  by an !equals statement."
  [proof args]
  (ensure-premise-count "Substitution" (take 2 args) 2)
  (ensure-premises-relevant proof (take 2 args))
  (let [premises         (-> proof
                             (get ::proof/premises))
        first-premise-id (->> args
                              first
                              Integer/parseInt)
        first-premise     (-> premises
                              (get first-premise-id)
                              (get ::proof/formula))
        second-premise-id (->> args
                               second
                               Integer/parseInt)
        second-premise    (-> premises
                              (get second-premise-id)
                              (get ::proof/formula))
        positions      (rest (rest args))]
    (cond
      (and (formula/equality? first-premise)
           (formula/atom? second-premise))
      (substitute-equality-internal
       proof first-premise-id second-premise-id positions)
      (and (formula/equality? second-premise)
           (formula/atom? first-premise))
      (substitute-equality-internal
       proof second-premise first-premise positions)
      :else
      proof)))

(defn beta-reduction [proof premise-numbers] nil)

;;; Lambdas...
