(ns logos.proof.premise
  (:require [logos.formula :as formula]
            [logos.proof.proof :as proof]))

(defn add-premise
  [proof formula]
  (let [problem-index (get proof ::proof/problems)
        current-problem-idx (get proof ::proof/current-problem)
        current-problem (get problem-index current-problem-idx)
        premise         (proof/new-premise formula)
        new-idx         (proof/get-new-premise-idx proof)
        new-problem     (update current-problem
                                ::proof/premises (fn [x] (conj x new-idx)))
        new-premise-index (assoc (get proof ::proof/premises) new-idx premise)
        new-problem-index (assoc problem-index
                                 current-problem-idx new-problem)]
    (assoc proof
           ::proof/premises new-premise-index
           ::proof/problems new-problem-index)))

(defmacro ^:private ensure-premise-count
  [premise-vector number]
  `(when (not (= (count ~premise-vector) ~number))
     (throw (ex-info
             "Incorrect number of premises passed to rule"
             {:caused-by ~premise-vector}))))

(defn conditional-elimination
  [proof premise-numbers]
  (ensure-premise-count premise-numbers 2)
  (let [premise-index (get proof ::proof/premises)
        premise-one   (-> premise-index
                          (get (first premise-numbers))
                          (get ::proof/formula))
        premise-two   (-> premise-index
                          (get (second premise-numbers))
                          (get ::proof/formula))]
    (if (formula/implication? premise-one)
      (let [antecedent (formula/antecedent premise-one)
            consequent (formula/consequent premise-one)]
        (if (= premise-two antecedent)
          (let [problem-index   (get proof ::proof/problems)
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
                   ::proof/problems new-problem-idx))

          proof))
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
  (let [premise-index (get proof ::proof/premises)
        formula       (-> premise-index
                          (get (first premise-numbers))
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

(defn disjunction-elimination [prooof premise-numbers] nil)

(defn bottom-introduction [proof premise-numbers] nil)

(defn universal-elimination [proof premise-numbers] nil)

(defn existential-eliminiation [proof premise-numbers] nil)

;;; Lambdas...
