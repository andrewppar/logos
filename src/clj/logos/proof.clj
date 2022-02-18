(ns logos.proof
  (:require [logos.formula :as formula]
            [clojure.string :as string]
            [clojure.set :as set]))

;;;;;;;;;;;
;;; Problem


(defn new-problem [premise-indexes goal id]
  {::premises premise-indexes
   ::goal     goal
   ::id       id
   ::status    ::open})

;;;;;;;;;
;;; Edges

(defn ^:private add-from-edge-to-graph
  [graph from to]
  (if-let [edges (get graph from)]
    (if-let [from-edges (get edges ::to)]
      (->> (conj from-edges to)
           (assoc edges ::to)
           (assoc graph from))
      (->> [to]
           (assoc edges ::to)
           (assoc graph from)))
    (assoc graph from {::to [to]})))

(defn ^:private add-to-edge-to-graph
  [graph from to]
  (if-let [edges (get graph to)]
    (if-let [to-edges (get edges ::from)]
      (assoc graph
             to (assoc edges
                         ::from (conj (to-edges from))))
      (assoc graph to (assoc edges ::from [from])))
    (assoc graph to {::from [from]})))

(defn add-edges-to-edge-index
  [index edges]
  (reduce
   (fn [acc [from to]]
     (-> acc
      (add-from-edge-to-graph from to)
      (add-to-edge-to-graph from to)))
   index edges))

;;;;;;;;;;;;
;;; Premises

(defn new-premise
  "Create a new premise from an `id`, `formula`,
  and `justification`

  Justifications are either a keyword (one of
  `::assertion` or `::hypothesis`) or a list of
  other premise ids."
  ([formula]
   (new-premise formula ::assertion))
  ([formula justification]
   {::formula formula
    ::justification justification}))

(defn ^:private add-asserted-premises-to-index
  "Add premises to a premise index with `::assertion`
  justifications"
  [index premises]
  (if (seq premises)
    (let [old-indexes (keys index)
          new-idx     (if (nil? old-indexes)
                        0
                        (inc (apply max old-indexes)))]
      (loop [formula     (first premises)
             todo        (rest premises)
             current-idx new-idx
             result      index]
        (let [premise    (new-premise formula)
              new-result (assoc
                          result current-idx premise)]
          (if (seq todo)
            (recur (first todo)
                   (rest todo)
                   (inc current-idx)
                   new-result)
            new-result))))
    index))

(defn ^:private add-new-problems-to-proof
  [proof problems from-id]
  (let [[new-prblm-index new-edges]
        (reduce
         (fn [[prblm-index edges] problem]
           (let [new-id (inc (apply max (keys prblm-index)))]
             [(assoc prblm-index new-id (assoc problem ::id new-id))
              (conj edges [from-id new-id])]))
         [(get proof ::problems) []] problems)
        new-edge-index (-> proof
                           (get ::edges)
                           (add-edges-to-edge-index new-edges))]
    (assoc proof
           ::problems new-prblm-index
           ::edges new-edge-index)))

;;;;;;;;;
;;; Proof

(defn new-proof
  ([goal]
   (new-proof [] goal))
  ([premises goal]
   (let [premise-index (add-asserted-premises-to-index {} premises)
         indexes       (keys premise-index)
         problem       (new-problem indexes goal 0)
         problem-index {0 problem}]
     {::current-problem 0
      ::premises        premise-index
      ::problems        problem-index
      ::edges           {}})))

(defn ^:private get-new-idx-for-type
  [proof type]
  (if-let [old-idxs (keys (type proof))]
    (inc (apply max old-idxs))
    0))

(defn ^:private get-new-problem-idx
  [proof]
  (get-new-idx-for-type proof ::problems))

(defn ^:private get-new-premise-idx
  [proof]
  (get-new-idx-for-type proof ::premises))

(defn backward-gather-problem-idxs [proof idx]
  (let [edges (get proof ::edges)]
    (loop [current-idx idx
           todo        []
           result      []]
      (let [maybe-new-idxs (-> edges
                               (get current-idx)
                               (get ::from))
            new-idxs (filter (fn [idx]
                               (not (some #{idx} result)))
                             maybe-new-idxs)
            new-todos (concat todo new-idxs)]
        (if (seq new-todos)
          ;; TODO: Maybe treat this as a stack
          ;; It probably won't make a difference
          ;; at this scale though
          (recur (first new-todos)
                 (rest new-todos)
                 (conj result current-idx))
          (conj result current-idx))))))

(defn relevant-premises
  "Premises that are relevant to
  the current proof"
  [proof]
  (let [current-idx           (get
                               proof ::current-problem)
        problem-index         (get proof ::problems)
        premise-index         (get proof ::premises)
        relevant-problem-idxs (backward-gather-problem-idxs
                               proof current-idx)]
    (->> relevant-problem-idxs
         (map #(get problem-index %))
         (mapcat ::premises)
         (map #(get premise-index %))
         (map ::formula))))

(defn filter-premises-by-problem-and-status
  "Filter to premise idxs that have `status`
  and are in `problem`."
  [proof problem-idx status]
  (let [premise-index (get proof ::premises)
        problem-premise-idxs (-> proof
                                 (get ::problems)
                                 (get problem-idx)
                                 (get ::premises))
        premises (map (fn [idx]
                        (let [{::keys [justification]}
                              (get premise-index idx)]
                          {:idx idx
                           :justification justification}))
                      problem-premise-idxs)]
    (->> premises
         (filter (fn [pr-map] (= (:justification pr-map) status)))
         (map :idx))))

(defn close-problem
  "Close the problem in `proof`
  at `idx`"
  ;; Has to close as many of the prblems as
  ;; can be closed by traversing the problem tree
  ;; there's a failiing test for this
  [proof idx]
  (let [problems        (get proof ::problems)
        current-problem (get problems idx)
        problem-assertions (filter-premises-by-problem-and-status
                            proof idx ::assertion)
        upstream-problem-idxs (-> proof
                                  (get ::edges)
                                  (get idx)
                                  (get ::from))
        new-upstream-problems (reduce
                               (fn [acc upstream-idx]
                                 (let [problem (get acc upstream-idx)
                                       new-premises (concat
                                                     (get problem ::premises)
                                                     problem-assertions)
                                       new-problem (assoc
                                                    problem
                                                    ::premises
                                                    new-premises)]
                                   (assoc acc upstream-idx new-problem)))
                               problems upstream-problem-idxs)
        updated-problem (assoc current-problem ::status ::closed)]
    (->> updated-problem
         (assoc new-upstream-problems idx)
         (assoc proof ::problems))))

;;;;;;;;;;;;;;;
;;; Update Goal

(defn conditional-proof
  [proof]
  (let [current-problem (get
                         (::problems proof)
                         (::current-problem proof))
        current-goal    (::goal current-problem)]
    (if (formula/implication? current-goal)
      (let [new-problem-idx (get-new-problem-idx proof)
            old-problem-idx (::id current-problem)
            new-edges       [[old-problem-idx
                              new-problem-idx]]
            hypothesis     (formula/antecedent
                             current-goal)
            new-goal        (formula/consequent
                             current-goal)
            new-premise-idx (get-new-premise-idx proof)
            new-problem     (new-problem
                             [new-premise-idx]
                             new-goal
                             new-problem-idx)]
        (assoc proof
               ::current-problem new-problem-idx
               ::premises (assoc
                           (::premises proof)
                           new-premise-idx
                           (new-premise hypothesis ::hypothesis))
               ::problems (assoc
                           (::problems proof)
                           new-problem-idx new-problem)
               ::edges (add-edges-to-edge-index
                        (::edges proof) new-edges)))
      proof)))

(defn direct-proof
  "Close the current problem by showing
  that its goal is a relevant premise"
  [proof]
  (let [current-problem-idx (get proof ::current-problem)
        current-goal  (-> proof
                          (get ::problems)
                          (get current-problem-idx)
                          (get ::goal))
        premises            (relevant-premises proof)
        proved?       (if (some #{current-goal} premises)
                        true
                        (if (and (formula/disjunction? current-goal)
                                 (-> current-goal
                                     formula/disjuncts
                                     set
                                     (set/intersection (set premises))
                                     seq))
                          true
                          false))]
    (if proved?
      (let [new-current-problem (-> proof
                                    (get ::edges)
                                    (get current-problem-idx)
                                    (get ::from)
                                    first)]
        (-> proof
            (assoc ::current-problem new-current-problem)
            (close-problem current-problem-idx)))
      proof)))

(defn conjunctive-proof
  [proof]
  (let [current-problem-idx (get proof ::current-problem)
        current-goal        (-> proof
                                (get ::problems)
                                (get current-problem-idx)
                                (get ::goal))]
    (if (formula/conjunction? current-goal)
      (let [new-problems (->> current-goal
                              formula/conjuncts
                              (map (fn [conjunct]
                                     (new-problem [] conjunct nil))))
            new-proof (add-new-problems-to-proof
                       proof new-problems  current-problem-idx)]
        (assoc new-proof
               ::current-problem
               (apply max (keys (get new-proof ::problems)))))
      proof)))

(defn disjunctive-proof
  [proof]
  (direct-proof proof))
