(ns logos.proof.show
  (:require [logos.proof.proof :as proof]
            [logos.formula :as formula]
            [clojure.string :as string]))

(defn show-proof
  [proof]
  (let [premise-idxs (proof/relevant-premise-idxs proof)
        premise-index (get proof ::proof/premises)
        problem-id (get proof ::proof/current-problem)
        problem    (get (get proof ::proof/problems) problem-id)
        size     50]
    (when (seq premise-idxs)
      (loop [premise-idx (first premise-idxs)
             todo    (rest premise-idxs)]

        (let [premise (get premise-index premise-idx)
              formula (-> premise
                          (get ::proof/formula)
                          formula/to-string)
              base    (format "%s. %s"  premise-idx formula)
              justification (-> premise
                                (get ::proof/justification)
                                name)
              padding (-> size
                          (- (+ (count base) (count justification)))
                          (repeat " ")
                          string/join)]

          (println (format "%s%s%s" base padding justification))
          (if (seq todo)
            (recur (first todo)
                   (rest todo))
            nil))))
    (println (format "SHOW: %s" (-> problem
                                    (get ::proof/goal)
                                    formula/to-string)))))
