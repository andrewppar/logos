(ns logos.proof
  (:require [logos.formula :as formula]
            [clojure.string :as string]))

;;;;;;;;;;;;;;
;; Proof Utils

(defn proof-reduce
  ([gather-fn proof]
   (proof-reduce
    gather-fn (fn [_] false) proof))
  ([gather-fn stop-fn proof]
   (loop [current-proof proof
          todos         []
          result        []]
     (let [new-todos  (concat todos
                              (:subproofs current-proof))
           new-result (gather-fn result current-proof)]
       (if (and (seq new-todos)
                (not (stop-fn new-result)))
         (recur (first new-todos)
                (rest new-todos)
                new-result)
         new-result)))))

;;;;;;;;;;;;;;;;;;;;;
;;; Indexing Premises

(defn ^:private next-unused-index
  [proof]
  (let [indices (proof-reduce
                 (fn [acc subproof]
                   (concat
                    acc
                    (->> subproof
                         :premises
                         (map :index))))
                 proof)]
    (inc (apply max indices))))


;; TODO: Abstract some of this out to make it readable
(declare reindex-proof-internal)

(defn index-subproofs [used-idxs subproofs]
  (reduce (fn [[subproofs-acc used-idxs-acc] subproof]
            (let [[new-proof new-used-idxs]
                  (reindex-proof-internal subproof used-idxs-acc)]
              [(conj subproofs-acc new-proof)
               (concat used-idxs-acc new-used-idxs)]))
          [[] used-idxs] subproofs))

(defn add-premises-to-proof
  [premises used-idxs result-proof]
  (loop [idx 0
         premise          (first premises)
         to-index         (rest premises)
         result-used-idxs used-idxs
         result   result-proof]
    (let [form (if-let [f (get premise :formula)]
                 f
                 premise)]
      (cond (and
             (not (seq to-index))
             (not (some #{idx} used-idxs)))
            [(assoc result
                   :premises
                   (conj (:premises result-proof)
                         {:formula form
                          :index idx}))
             (conj result-used-idxs idx)]
            (not (some #{idx} used-idxs))
            (recur
             (+ idx 1)
             (first to-index)
             (rest to-index)
             (conj result-used-idxs idx)
             (assoc result
                    :premises
                    (conj (:premises result-proof)
                          {:formula form
                           :index idx})))
            :else
            (recur
             (+ idx 1)
             form
             to-index
             result-used-idxs
             result)))))

(defn reindex-proof-internal
  [proof used-idxs]
  (let [{:keys [premises goal status subproofs]} proof
        [indexed-subproofs
         new-used-idxs]
        (index-subproofs used-idxs subproofs)
        result-proof {:premises []
                      :goal goal
                      :status status
                      :subproofs indexed-subproofs}]
    (if (seq premises)
      (add-premises-to-proof
       premises new-used-idxs result-proof)
      [result-proof new-used-idxs])))

(defn reindex-proof [proof]
  (first (reindex-proof-internal proof [])))

(defn new-proof
  ([premises goal subproofs]
   (new-proof premises goal subproofs :open))
  ([premises goal subproofs status]
   (reindex-proof
    {:premises premises
     :goal goal
     :status status
     :subproofs subproofs})))


(defn proof-done? [proof]
  (= (:status proof) :closed))

;;;;;;;;;;;;;;;
;; Finish Proof

(defn prune-proof [proof]
  (let [new-subproofs (map prune-proof (:subproofs proof))
        new-proof     (assoc proof :subproofs new-subproofs)]
    (if (every? proof-done? new-subproofs)
      (assoc new-proof :status :closed)
      new-proof)))

;;;;;;;;;;;;;;;;;;
;; Goal Operations

(def goal-operations (atom nil))

(defmacro def-goal-operation [operation docstring args & body]
  (do
    (when (not (string? docstring))
      (throw
       (ex-info "All goal operations require a docstring"
                {:caused-by operation})))
    `(defn ~operation ~docstring ~args (do ~@body))
   ;; (reset! goal-operations
   ;;         (conj @goal-operations operation))
    ))

(defn ^:private get-spotlight-proof
  "Given a list of proofs separate into
  those that are done, the one to focus on,
  and those that are still to be worked on."
  [proofs]
  (loop [current (first proofs)
         done []
         todo (rest proofs)]
    (if-not (proof-done? current)
      [done current todo]
      (recur (first todo)
             (cons current done)
             (rest todo)))))

(defn apply-proof-rule
  ""
  [premises rule proof used-constants]
  (let [new-premises (concat premises (:premises proof))
        new-constants (set (concat
                            used-constants
                            (mapcat
                             formula/constants
                             new-premises)))]
    (reindex-proof
     (if (seq (:subproofs proof))
       (let [subproofs  (:subproofs proof)
             [finished spotlight todo]
             (get-spotlight-proof subproofs)]
         (assoc proof
                :subproofs
                (concat finished
                        [(apply-proof-rule
                          new-premises
                          rule
                          spotlight
                          new-constants)]
                        todo)))
       (rule new-premises proof new-constants)))))

(defn conditional-proof-internal
  [_ proof _]
  (let [goal (:goal proof)]
    (if (formula/implication? goal)
      (let [ant       (formula/antecedent goal)
            cons      (formula/consequent goal)
            subproof  (new-proof [ant] cons [])]
        (assoc proof
               :subproofs
               (conj (:subproofs proof) subproof)))
      proof)))

(defn conditional-proof
  "Given a proof whose
  primary goal is a conditional,
  assumes the antecedent and
  changes the goal to the consequent."
  [proof]
  (apply-proof-rule [] conditional-proof-internal proof []))

(defn universal-proof-internal [_ proof old-constants]
  (let [goal (:goal proof)]
    (if (formula/universal? goal)
      (let [;;old-constants (gather-proof-constants proof)
            new-goal
            (formula/instantiate-new-variables goal old-constants)]
        (assoc proof :subproofs
               (conj (:subproofs proof) (new-proof [] new-goal []))))
      proof)))

(defn universal-proof
  "Instantiate a new constant for a Universal
   and create a new proof for the resulting formula"
  [proof]
  (apply-proof-rule [] universal-proof-internal proof []))

(defn direct-proof-internal [old-premises proof _]
  (let [{:keys [premises goal subproofs]} proof]
    (if (some #{goal} (concat premises old-premises))
      (new-proof premises goal subproofs :closed)
      proof)))

(defn direct-proof [proof]
  "Close a proof by showing that the conclusion is one of the
   available premises."
  (prune-proof
   (apply-proof-rule [] direct-proof-internal proof [])))


(defn conjunctive-proof-internal [_ proof _]
  (let [{:keys [goal subproofs]} proof]
    (if-not (formula/conjunction? goal)
      proof
      (let [conjuncts  (formula/conjuncts goal)
            new-proofs (map #(new-proof [] % []) conjuncts)]
        (assoc proof
               :subproofs
               (concat subproofs new-proofs))))))

(defn conjunctive-proof [proof]
  "Prove a conjunction by generating
   subproofs for each conjunct."
  (apply-proof-rule [] conjunctive-proof-internal proof []))

;;;;;;;;;;;;;;;;;;;;;;
;;; Premise Operations


(defn get-formula-at-index
  [proof index]
  (loop [current-proof proof
         todo          nil]
    (let [new-todos (concat todo
                            (:subproofs current-proof))]
      (if-let [premise (->> current-proof
                            :premises
                            (filter (fn [premise] (= (:index premise) index)))
                            first)]
        (:formula premise)
        (when (seq new-todos)
          (recur (first new-todos)
                 (rest new-todos)))))))

(defn ^:private generate-new-premises
  [formulas index]
  (loop [formula (first formulas)
         todo    (rest  formulas)
         current-index index
         result  []]
    (let [new-result (conj result
                           {:formula formula
                            :index current-index})]
      (if (seq todo)
        (recur (first todo)
               (rest todo)
               (inc current-index)
               new-result)
        new-result))))

(defn insert-formulas-near-index
  [proof index formulas outer-proof]
  (let [premises (:premises proof)]
    (if (some #{index} (map :index premises))
      (let [new-index (next-unused-index outer-proof)
            new-premises (generate-new-premises
                          formulas new-index)]
        (assoc proof
               :premises (concat premises new-premises)))
      (let [new-subproofs (map
                           (fn [subproof]
                             (insert-formulas-near-index
                              subproof
                              index formulas outer-proof))
                           (:subproofs proof))]
        (assoc proof :subproofs new-subproofs)))))

;; There's a difference between destructive and
;; additive use of premises that might be useful.
;; we could also have one call -- eliminate -- or
;; something like that that automatically chooses
;; the right rule to apply in some cases

(defn conjunction-elimination
  "Given a premise in a proof that is
   a conjunction convert that into multiple
   premises."
  [proof index]
  (let [premise (get-formula-at-index proof index)]
    (if (formula/conjunction? premise)
      (insert-formulas-near-index
       proof index (formula/conjuncts premise) proof)
      proof)))

(defn disjunction-elimination
  "Given a premise that's a disjunction
  create subproofs for each of the disjuncts"
  [proof index]
  (let [premise (get-formula-at-index proof index)]
    (if (formula/disjunction? premise)
      (let [current-subproof (get-current-proof ;;spotlight?
                              )]
        (update-current-proof...)))))


;;;;;;;;;;;;;;;;;
;;; Serialization

(defn serialize-premises [premises]
  (reduce (fn [result-acc premise]
            (format "%s. %s\n %s"
                    (:index premise)
                    (formula/to-string (:formula premise))
                    result-acc))
          "" premises))

(defn to-string-internal [proof acc]
  (let [{:keys [premises goal subproofs]} proof
        premise-serialized (serialize-premises premises)]
    (if-not (seq subproofs)
      (format "%s  GOAL: %s"
              premise-serialized
              (formula/to-string goal))
      (let [recursive-case (to-string-internal (first subproofs) acc)]
        (format "%s\n%s"
                premise-serialized
                recursive-case)))))

(defn to-string [proof]
  (to-string-internal proof ""))

(defn show-proof [proof depth]
  (let [{:keys [premises goal subproofs]} proof
        padding (->> "| "
                     (repeat depth)
                     string/join)]
    (str
     (string/join
      (map (fn [premise]
             (let [{:keys [index formula]} premise]
               (format
                "%s%s %s \n"
                padding
                index
                (formula/to-string formula))))
           premises))
     (if (and
          (seq subproofs)
          (every? #'proof-done? subproofs))
       (format "%s|-QED: %s\n"
               padding

               (formula/to-string goal))
       (str
        (format "%s|  SHOW: %s\n"
                padding
                (formula/to-string goal))
        (string/join
         (->> subproofs
              ;; This looks like an error
              ;; shouldn't this be (not (proof-done? %))
              (filter #(not (= :closed %)))
              (map #(show-proof % (+ depth 1))))))))))
