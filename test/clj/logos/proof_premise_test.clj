(ns logos.proof-premise-test
  (:require [clojure.test :refer [deftest are]]
            [logos.proof.premise :as premise]
            [logos.proof.proof :as proof]
            [logos.formula :as formula]))

;;;;;;;;;;;;;;;;;;;;
;;; Useful Constants

;;; TODO: This is repeated. Factor it out

(def p       (formula/atom "@P"))
(def q       (formula/atom "@Q"))
(def r       (formula/atom "@R"))
(def p->q    (formula/implies p q))

(def assertion-hypothesis-proof
  {::proof/current-problem 1
         ::proof/premises {0 {::proof/formula p
                              ::proof/justification ::proof/assertion}
                           1 {::proof/formula q
                              ::proof/justification ::proof/hypothesis}}
         ::proof/problems {0 {::proof/premises [0]
                              ::proof/goal     r
                              ::proof/id 0
                              ::proof/status ::proof/open}
                           1 {::proof/premises [1]
                              ::proof/goal     p
                              ::proof/id 1
                              ::proof/status ::proof/open}}
         ::proof/edges {0 {::proof/to [1]}
                        1 {::proof/from [0]}}})

(deftest test-add-premise
  (are [proof formula result]
      (= (premise/add-premise proof formula)
         result)
      assertion-hypothesis-proof r
      {::proof/current-problem 1
       ::proof/premises {0 {::proof/formula p
                            ::proof/justification ::proof/assertion}
                         1 {::proof/formula q
                            ::proof/justification ::proof/hypothesis}
                         2 {::proof/formula r
                            ::proof/justification ::proof/assertion}}
       ::proof/problems {0 {::proof/premises [0]
                            ::proof/goal     r
                            ::proof/id 0
                            ::proof/status ::proof/open}
                         1 {::proof/premises [1 2]
                            ::proof/goal     p
                            ::proof/id 1
                            ::proof/status ::proof/open}}
       ::proof/edges {0 {::proof/to [1]}
                      1 {::proof/from [0]}}}))


(deftest test-conditional-elimination
  (let [pf {::proof/current-problem 0
            ::proof/premises {0 (proof/new-premise p)
                              1 (proof/new-premise p->q)}
            ::proof/problems {0 (proof/new-problem [0 1] q 0)}
            ::proof/edges {}}]
    (are [proof idxs result]
        (= (premise/conditional-elimination proof idxs)
           result)
        pf [1 0]  {::proof/current-problem 0
                   ::proof/premises {0 (proof/new-premise p)
                                     1 (proof/new-premise p->q)
                                     2 (proof/new-premise q [1 0])}
                   ::proof/problems {0 (proof/new-problem [0 1 2] q 0)}
                   ::proof/edges {}}
        pf [1 2] pf)))

(deftest test-conjunction-elimination
  (are [proof idxs result]
      (= (premise/conjunction-elimination proof idxs)
         result)
      {::proof/current-problem 0
       ::proof/premises {0 (proof/new-premise
                            (formula/conj p q))}

       ::proof/problems {0 (proof/new-problem [0] q 0)}
       ::proof/edges {}} [0]

      {::proof/current-problem 0
       ::proof/premises {0 (proof/new-premise (formula/conj p q))
                         1 (proof/new-premise p [0])
                         2 (proof/new-premise q [0])}
       ::proof/problems {0 (proof/new-problem [0 1 2] q 0)}
       ::proof/edges {}}))
