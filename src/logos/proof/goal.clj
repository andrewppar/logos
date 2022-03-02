(ns logos.proof.goal
  (:require [logos.formula :as formula]
            [logos.proof.proof :as proof]
            [clojure.set :as set]))

(defn direct-proof
  "Close the current problem by showing
  that its goal is a relevant premise"
  [proof]
  (let [current-problem-idx (get proof ::proof/current-problem)
        current-goal  (-> proof
                          (get ::proof/problems)
                          (get current-problem-idx)
                          (get ::proof/goal))
        premises            (proof/relevant-premises proof)
        proved?           (cond (some #{::formula/bottom} premises)
                                true
                                (some #{current-goal} premises)
                                true
                                (and (formula/disjunction? current-goal)
                                     (-> current-goal
                                         formula/disjuncts
                                         set
                                         (set/intersection (set premises))
                                         seq))
                                true
                                :else
                                false)]
    (if proved?
      (proof/close-problem proof current-problem-idx)
      proof)))

(defn conditional-proof
  [proof]
  (let [current-problem (get
                         (::proof/problems proof)
                         (::proof/current-problem proof))
        current-goal    (::proof/goal current-problem)]
    (if (formula/implication? current-goal)
      (let [new-problem-idx (proof/get-new-problem-idx proof)
            old-problem-idx (::proof/id current-problem)
            new-edges       [[old-problem-idx
                              new-problem-idx]]
            hypothesis     (formula/antecedent
                             current-goal)
            new-goal        (formula/consequent
                             current-goal)
            new-premise-idx (proof/get-new-premise-idx proof)
            new-problem     (proof/new-problem
                             [new-premise-idx]
                             new-goal
                             new-problem-idx)]
        (assoc proof
               ::proof/current-problem new-problem-idx
               ::proof/premises (assoc
                           (::proof/premises proof)
                           new-premise-idx
                           (proof/new-premise hypothesis ::proof/hypothesis))
               ::proof/problems (assoc
                           (::proof/problems proof)
                           new-problem-idx new-problem)
               ::proof/edges (proof/add-edges-to-edge-index
                        (::proof/edges proof) new-edges)))
      proof)))

(defn conjunctive-proof
  [proof]
  (let [current-problem-idx (get proof ::proof/current-problem)
        current-goal        (-> proof
                                (get ::proof/problems)
                                (get current-problem-idx)
                                (get ::proof/goal))]
    (if (formula/conjunction? current-goal)
      (let [new-problems (->> current-goal
                              formula/conjuncts
                              (map (fn [conjunct]
                                     (proof/new-problem [] conjunct nil))))
            new-proof (proof/add-new-problems-to-proof
                       proof new-problems  current-problem-idx)]
        (assoc new-proof
               ::proof/current-problem
               (apply max (keys (get new-proof ::proof/problems)))))
      proof)))

(defn disjunctive-proof
  [proof]
  (direct-proof proof))

(defn negative-proof
  [proof]
  (let [problem-index       (get proof ::proof/problems)
        current-problem-idx (get proof ::proof/current-problem)
        current-problem     (get problem-index current-problem-idx)
        current-goal        (get current-problem ::proof/goal)]
    (if (formula/negation? current-goal)
      (let [new-formula (formula/implies
                         (formula/negatum current-goal)
                         ::formula/bottom)
            new-problem (assoc current-problem ::proof/goal new-formula)
            new-proof   (->> new-problem
                             (assoc problem-index current-problem-idx)
                             (assoc proof ::proof/problems)
                             conditional-proof)]
        (assoc new-proof
               ::proof/problems
               (assoc (get new-proof ::proof/problems)
                      current-problem-idx
                      current-problem)))
      proof)))

(defn universal-proof [proof]
  (let [current-problem-idx (get proof ::proof/current-problem)
        problem-index       (get proof ::proof/problems)
        current-goal        (-> problem-index
                                (get current-problem-idx)
                                (get ::proof/goal))]
    (if (formula/universal? current-goal)
      (let [new-problem-idx (proof/get-new-problem-idx proof)
            used-constants  (->> proof
                                 proof/relevant-premises
                                 (mapcat
                                  (fn [formula]
                                    (formula/formula-gather
                                     formula #'formula/constant?))))
            new-goal        (formula/instantiate-new-variables
                             current-goal used-constants)
            new-problem     (proof/new-problem
                             [] new-goal new-problem-idx)
            new-problem-index (assoc problem-index
                                     new-problem-idx new-problem)
            new-edges       [[current-problem-idx new-problem-idx]]]
        (assoc proof
               ::proof/problems new-problem-index
               ::proof/current-problem new-problem-idx
               ::proof/edges
               (proof/add-edges-to-edge-index
                (::proof/edges proof) new-edges)))
      proof)))






(defn existential-proof [proof] nil)

;;; Lambdas...
