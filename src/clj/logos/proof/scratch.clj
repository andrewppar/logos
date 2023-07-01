(ns logos.proof.scratch
  (:require [logos.proof.premise :as premise]
            [logos.proof.proof :as proof]
            [logos.formula :as formula]
            ))

(def p "p")
(def q "q")

{::proof/current-problem 0
 ::proof/premises {0 (proof/new-premise
                      (formula/conj p q))}

 ::proof/problems {0 (proof/new-problem [0] q 0)}
 ::proof/edges {}}


(proof/new-problem [0] q 0)
