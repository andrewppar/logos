(ns logos.proof
  (:require [logos.formula :as formula]))


;; Proofs

(defn ^:private gather-premises [subproofs]
  (reduce (fn [acc subproof]
            (let [inner-subproofs (:subproofs subproof)
                  premises        (:premises subproof)
                  new-acc         (concat acc premises)]
              (if (empty? inner-subproofs)
                new-acc
                (concat
                 new-acc
                 (gather-premises inner-subproofs)))))
            [] subproofs))

;; TODO: Abstract some of this out to make it readable
(declare reindex-proof-internal)

(defn index-subproofs [used-idxs subproofs]
  (reduce (fn [[subproofs-acc used-idxs-acc] subproof]
            (let [[new-proof new-used-idxs]
                  (reindex-proof-internal
                   subproof used-idxs-acc)]
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
  (let [{:keys [premises goal subproofs]} proof
        [indexed-subproofs
         new-used-idxs]
        (index-subproofs used-idxs subproofs)
        result-proof {:premises []
                      :goal goal
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

;;;;;;;;;;;;;;;;;;
;; Goal Operations

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
  "Given a proof whose
  primary goal is a conditional,
  assumes the antecedent and
  changes the goal to the consequent."
  [premises rule proof]
  (let [new-premises (concat premises (:premises proof))]
    (reindex-proof
     (if (seq (:subproofs proof))
       (let [subproofs  (:subproofs proof)
             [finished spotlight todo]
             (get-spotlight-proof subproofs)]
         (assoc proof
                :subproofs
                (concat finished
                        [(apply-proof-rule
                          new-premises rule spotlight)]
                      todo)))
       (rule new-premises proof)))))

(defn conditional-proof-internal
  [premises proof]
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
  (apply-proof-rule [] conditional-proof-internal proof))
  
        
(defn direct-proof-internal [old-premises proof]
  (let [{:keys [premises goal subproofs]} proof]
    (if (some #{goal} (concat premises old-premises))
      (new-proof premises goal subproofs :closed)
      proof)))

(defn direct-proof [proof]
  (apply-proof-rule [] direct-proof-internal proof))

(defn conjunctive-proof-internal [premises proof]
  (let [{:keys [goal subproofs]} proof]
    (if-not (formula/conjunction? goal)
      proof
      (let [conjuncts  (formula/conjuncts goal)
            new-proofs (map #(new-proof [] % []) conjuncts)]
        (assoc proof
               :subproofs
               (concat subproofs new-proofs))))))

(defn conjunctive-proof [proof]
  (apply-proof-rule [] conjunctive-proof-internal proof))

;;;;;;;;;;;;;;;
;; Finish Proof

(defn prune-proof [proof]
  (let [new-subproofs (map prune-proof (:subproofs proof))
        new-proof     (assoc proof :subproofs new-subproofs)]
    (if (every? proof-done? new-subproofs)
      (assoc new-proof :status :closed)
      new-proof)))

;;;;;;;;;;;;;;;;;;;;;;
;;; Premise Operations


(defn conjunction-elimination
  "Given a premise in a proof that is
   a conjunction convert that into multiple
   premises."
  [proof premise]
  nil)


;;;;;;;;;;;;;;;;;
;;; Serialization

(defn collect-premises [premises]
  (reduce (fn [result-acc premise]
            (format "%s. %s\n %s"
                    (:index premise)
                    (formula/to-string (:formula premise))
                    result-acc))
          "" premises))
  
(defn to-string-internal [proof acc]
  (let [{:keys [premises goal subproofs]} proof
        premise-serialized (collect-premises premises)]
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
                     clojure.string/join)]
    (str
     (clojure.string/join
      (map #(format "%s%s\n" padding %) premises))
     (if (and
          (seq subproofs)
          (every? #(= :done %) subproofs))
       (format "%s%s QED: %s\n"
               padding (str depth) goal)
       (str
        (format "%s%s SHOW: %s\n"
                padding (str depth) goal)
        (clojure.string/join
         (->> subproofs
              (filter #(not (= :done %)))
              (map #(show-proof % (+ depth 1))))))))))

