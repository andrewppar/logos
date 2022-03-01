(ns logos.proof.show
  (:require [logos.proof.proof :as proof]
            [logos.formula :as formula]
            [clojure.string :as string]))

(def proof-width 50)

(defn generate-premise-string
  [premise-index premise-idxs]
  (loop [premise-idx (first premise-idxs)
         todo    (rest premise-idxs)
         result  ""]

    (let [premise (get premise-index premise-idx)
          formula (-> premise
                      (get ::proof/formula)
                      formula/to-string)
          base    (format "%s. %s"  premise-idx formula)
          raw_justification (-> premise
                                (get ::proof/justification))
          justification    (if (keyword? raw_justification)
                             (name raw_justification)
                             (str raw_justification))
          padding (-> proof-width
                      (- (+ (count base) (count justification)))
                      (repeat " ")
                      string/join)
          premise-string (format "%s%s%s\n" base padding justification)
          new-result (format "%s%s" result premise-string)]
      (if (seq todo)
        (recur (first todo)
               (rest todo)
               new-result)
        new-result))))

(defn show-proof
  [proof]
  (let [premise-idxs (sort (proof/relevant-premise-idxs proof))
        premise-index (get proof ::proof/premises)
        problem-id (get proof ::proof/current-problem)
        problem    (get (get proof ::proof/problems) problem-id)
        premise-string (if (seq premise-idxs)
                         (generate-premise-string
                          premise-index premise-idxs)
                         "")
        goal-string    (format "SHOW: %s\n" (-> problem
                                                (get ::proof/goal)
                                                formula/to-string))]
    (str premise-string goal-string)))
