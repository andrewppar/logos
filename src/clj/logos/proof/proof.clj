(ns logos.proof.proof
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

(defn ^:private reset-current-problem
  [proof]
  (let [problem-index (get proof ::problems)
        edges         (get proof ::edges)]
    (if (= ::open (-> problem-index
                      (get (get proof ::current-problem))
                      (get ::status)))
      proof
      (loop [result proof]
        ;; We know the current problem is closed
        (let [current-problem (get result ::current-problem)]
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
                                             closed-upstream))))))
            ;; There are no upstream problems
            (assoc result ::current-problem nil)))))))

(defn close-problem
  "Close the problem in `proof`
  at `idx`"
  ;; Has to close as many of the prblems as
  ;; can be closed by traversing the problem tree
  ;; there's a failiing test for this
  [proof idx]
  (let [problems        (get proof ::problems)
        current-problem (get problems idx)
        updated-problem (assoc current-problem ::status ::closed)]
    (->> updated-problem
         (assoc problems idx)
         (assoc proof ::problems)
         reset-current-problem)))
