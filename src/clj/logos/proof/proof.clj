(ns logos.proof.proof
  (:require [logos.formula :as formula]
            [clojure.string :as string]
            [clojure.set :as set]))

;;;;;;;;;;;
;;; Problem

(defmacro dpf [form]
  (let [function    (-> form first name)
        args        (rest form)]
    `(let [arg-vals#    (map str '~args)
           arg-strings# (clojure.string/join " " arg-vals#)
           result#       (eval ~form)]
       (println (format "(%s %s) => %s" ~function arg-strings# result#))
       result#)))

(defn new-problem
  [premise-indexes goal id &
   {:keys [type] :or {type ::proof}}]
  {::premises premise-indexes
   ::goal     goal
   ::id       id
   ::status   ::open
   ::type     type})

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

(defn ^:private add-premises-to-index-with-justification
  [index premises justification]
  (if (seq premises)
    (let [old-indexes (keys index)
          new-idx     (if (nil? old-indexes)
                        0
                        (inc (apply max old-indexes)))]
      (loop [formula     (first premises)
             todo        (rest premises)
             current-idx new-idx
             result      index]
        (let [premise    (new-premise formula justification)
              new-result (assoc
                          result current-idx premise)]
          (if (seq todo)
            (recur (first todo)
                   (rest todo)
                   (inc current-idx)
                   new-result)
            new-result))))
    index))

(defn add-asserted-premises-to-index
  "Add premises to a premise index with `::assertion`
  justifications"
  [index premises]
  (add-premises-to-index-with-justification
   index premises ::assertion))

(defn add-hypotheses-to-index
  "Add premises to a premise index with `::hypothesis`
  justifications"
  [index premises]
  (add-premises-to-index-with-justification
   index premises ::hypothesis))

(defn add-new-problems-to-proof
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
  [goal
   & {:keys [premises theorem-name],
      :or {premises []}}]
  (when (or (not (formula/formula? goal))
            (not (every? formula/formula? premises)))
    (ex-info "Cannot make a proof with non-formulas"
             {:caused-by [goal premises]}))
  (let [premise-index (add-asserted-premises-to-index {} premises)
        indexes       (keys premise-index)
        problem       (new-problem indexes goal 0)
        problem-index {0 problem}]
    {::current-problem 0
     ::premises        premise-index
     ::problems        problem-index
     ::theorem-name    theorem-name
     ::edges           {}}))

(defn ^:private get-new-idx-for-type
  [proof type]
  (if-let [old-idxs (keys (type proof))]
    (inc (apply max old-idxs))
    0))

(defn get-new-problem-idx
  [proof]
  (get-new-idx-for-type proof ::problems))

(defn get-new-premise-idx
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

(defn relevant-premise-idxs
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
         (mapcat ::premises))))

(defn relevant-premises
  [proof]
  (let [premise-index (get proof ::premises)
        premise-idxs  (relevant-premise-idxs proof)]
    (->> premise-idxs
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

(defn ^:private close-assertion-problem
  [proof]
  (let [{::keys [current-problem problems premises edges]} proof
        focal-problem  (get problems current-problem)
        new-problem    (assoc focal-problem ::status ::closed)
        premise-idxs   (get current-problem ::premises)
        new-premise    (-> focal-problem
                           (get ::goal)
                           (new-premise ::proved))
        new-premise-idx (get-new-premise-idx proof)
        new-premises    (assoc premises new-premise-idx new-premise)
        assertion-idxs (reduce
                        (fn [idxs idx]
                          (if (= ::assert (get premises idx))
                            (conj idxs idx)
                            idxs))
                        [new-premise-idx] premise-idxs)
        upstream-problem (-> edges
                             (get current-problem)
                             (get ::from)
                             first)
        siblings          (filter
                           (fn [idx]
                             (not (= idx current-problem)))
                           (-> edges
                               (get upstream-problem)
                               (get ::to)))
        open-sibling     (reduce
                          (fn [acc idx]
                            (when (= ::open (-> problems
                                                (get idx)
                                                (get ::status)))
                              (reduced idx)))
                          nil siblings)
        new-upstream     (update (get problems upstream-problem)
                                 ::premises
                                 (fn [premises]
                                   (concat premises assertion-idxs)))
        new-proof        (assoc proof
                                ::premises new-premises
                                ::problems
                                (assoc problems
                                       upstream-problem new-upstream
                                       current-problem new-problem))
        ]
    (if (nil? open-sibling)
      (assoc new-proof ::current-problem upstream-problem)
      (assoc new-proof ::current-problem open-sibling))))

(defn ^:private reset-current-problem
  [proof]
  (let [edges (get proof ::edges)]
    (if (= ::open (-> proof
                      (get ::problems)
                      (get (get proof ::current-problem))
                      (get ::status)))
      proof
      (loop [result proof]
        ;; We know the current problem is closed
        (let [problem-index (get result ::problems)
              current-problem (get result ::current-problem)]
          ;; We take the first because a proof is a
          ;; tree, i.e. no problem can have more than
          ;; one root.
          (if-let [upstream-id (-> edges
                                   (get current-problem)
                                   (get ::from)
                                   first)]
            ;; There is an upstream problems,
            ;; so we need to migrate assertions down
            ;; and see if that problem can be closed

            (if (= ::assert (get (get problem-index current-problem) ::type))
              (close-assertion-problem result)
              (let [upstream-problem (-> result
                                         (get ::problems)
                                         (get upstream-id))
                    assertions (filter-premises-by-problem-and-status
                                result current-problem ::assertion)
                    new-upstream (assoc upstream-problem
                                        ::premises
                                        (concat
                                         (get upstream-problem ::premises)
                                         assertions))
                    ;; Check for open siblings to make
                    ;; the current problem
                    siblings (-> edges
                                 (get upstream-id)
                                 (get ::to))
                    open-siblings (->> siblings
                                       (map #(get problem-index %))
                                       (filter #(= (::status %) ::open)))]
                (if (seq open-siblings)
                  (assoc result
                         ::current-problem (-> open-siblings
                                               first
                                               (get ::id))
                         ::problems
                         (assoc problem-index
                                upstream-id new-upstream))
                  (let [closed-upstream (assoc new-upstream
                                               ::status ::closed)]
                    (recur  (assoc result
                                   ::current-problem upstream-id
                                   ::problems (assoc
                                               problem-index
                                               upstream-id
                                               closed-upstream)))))))
            ;; There are no upstream problems
            (assoc result ::current-problem nil)))))))

(defn close-problem
  "Close the problem in `proof`
  at `idx`"
  ;; There are no tests for this yet
  ;; hypothetical premises are forgotten
  ;; we'll also need to tear down premises
  ;; that rely on hypotheses being removed
  [proof idx]
  (let [problems        (get proof ::problems)
        current-problem (get problems idx)
        updated-problem (assoc current-problem ::status ::closed)]
    (->> updated-problem
         (assoc problems idx)
         (assoc proof ::problems)
         reset-current-problem)))

(defn get-proof-root-idx
  [proof]
  ;; This assumes that there is a unique
  ;; root problem to which all other problems
  ;; eventually bottom out
  (let [start (->> ::problems
                   (get proof)
                   keys
                   (apply min))]
    (loop [current-idx start]
      (let [upstream (-> proof
                         (get ::edges)
                         (get current-idx)
                         (get ::from))]
        (if (seq upstream)
          (recur (first upstream))
          current-idx)))))

(defn proof-done?
  [proof]
  (let [root-problem (-> proof
                         (get ::problems)
                         (get (get-proof-root-idx proof)))
        premise-idxs     (get root-problem ::premises)
        premise-index (get proof ::premises)
        premises      (->> premise-idxs
                           (map (fn [idx] (get premise-index idx))))
        assertions    (filter (fn [premise] (-> premise
                                                (get ::justification)
                                                (= ::assertion))) premises)]
    (if (seq assertions)
      false
      (let [problems (get proof ::problems)]
        (reduce
         (fn [done? problem-idx]
           (let [problem (get problems problem-idx)]
             (if (= (get problem ::status) ::closed)
               done?
               (reduced false))))
         true (keys problems))))))
