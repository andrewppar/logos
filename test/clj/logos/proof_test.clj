(ns logos.proof-test
  (:require [clojure.test :refer [deftest are]]
            [logos.proof :as proof
             :refer [add-edges-to-edge-index]]
            [logos.formula :as formula]))

;;;;;;;;;;;;;;;;;;;;
;;; Useful Constants

(def p       (formula/atom "@P"))
(def q       (formula/atom "@Q"))
(def r       (formula/atom "@R"))
(def p->q    (formula/implies p q))
(def p->q->p (formula/implies
              p (formula/implies q p)))

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

(deftest test-add-edges-to-edge-index
  (are [original edges result]
      (= (add-edges-to-edge-index
          original edges) result)
    {} [[1 2]] {1 {::proof/to [2]}
                2 {::proof/from [1]}}
    {} [[1 2]
        [1 3]] {1 {::proof/to [2 3]}
                2 {::proof/from [1]}
                3 {::proof/from [1]}}
    {} [[1 2]
        [2 3]] {1 {::proof/to [2]}
                2 {::proof/to [3]
                   ::proof/from [1]}
                3 {::proof/from [2]}}
    {1 {::proof/to [2]}
     2 {::proof/from [1]}}
    [[2 1]] {1 {::proof/to [2]
                ::proof/from [2]}
             2 {::proof/from [1]
                ::proof/to [1]}}))

(deftest test-conditional-proofs
  (are [start-proof result-proof]
      (= (proof/conditional-proof start-proof)
         result-proof)
    ;; Case 1
      {::proof/current-problem 0
       ::proof/premises        {}
       ::proof/problems        {0 {::proof/premises []
                                   ::proof/goal   p->q
                                   ::proof/id        0
                                   ::proof/status
                                   ::proof/open}}
       ::proof/edges           {}}
      {::proof/current-problem 1
       ::proof/premises        {0 {::proof/formula p
                                   ::proof/justification
                                   ::proof/hypothesis}}
       ::proof/problems       {0 {::proof/premises []
                                  ::proof/goal   p->q
                                  ::proof/id        0
                                  ::proof/status
                                  ::proof/open}
                               1 {::proof/premises [0]
                                  ::proof/goal q
                                  ::proof/id 1
                                  ::proof/status
                                  ::proof/open}}
       ::proof/edges {0 {::proof/to [1]}
                      1 {::proof/from [0]}}}
      ;;: Case 2
      {::proof/current-problem 1
       ::proof/premises        {0 {::proof/formula p
                                   ::proof/justification
                                   ::proof/assertion}}
       ::proof/problems        {0 {::proof/premises [0]
                                   ::proof/goal      q
                                   ::proof/id        0
                                   ::proof/status
                                   ::proof/open}
                                1 {::proof/premises []
                                   ::proof/goal     p->q
                                   ::proof/id        1
                                   ::proof/status
                                   ::proof/open}}
       ::proof/edges           {0 {::proof/to [1]}
                                1 {::proof/from [0]}}}
      {::proof/current-problem 2
       ::proof/premises        {0 {::proof/formula p
                                   ::proof/justification
                                   ::proof/assertion}
                                1 {::proof/formula p
                                   ::proof/justification
                                   ::proof/hypothesis}}
       ::proof/problems        {0 {::proof/premises [0]
                                   ::proof/goal      q
                                   ::proof/id        0
                                   ::proof/status
                                   ::proof/open}
                                1 {::proof/premises []
                                   ::proof/goal     p->q
                                   ::proof/id        1
                                   ::proof/status
                                   ::proof/open}
                                2 {::proof/premises [1]
                                   ::proof/goal      q
                                   ::proof/id        2
                                   ::proof/status
                                   ::proof/open}
                                }
       ::proof/edges           {0 {::proof/to [1]}
                                1 {::proof/from [0]
                                   ::proof/to   [2]}
                                2 {::proof/from [1]}}}
;;; Case 3
      {::proof/current-problem 0
       ::proof/premises        {0 {::proof/formula p
                                   ::proof/justification
                                   ::proof/assertion}}
       ::proof/problems        {0 {::proof/premises [0]
                                   ::proof/goal      q
                                   ::proof/id        0
                                   ::proof/status
                                   ::proof/open}
                                1 {::proof/premises []
                                   ::proof/goal     p->q
                                   ::proof/id        1
                                   ::proof/status
                                   ::proof/open}}
       ::proof/edges           {0 {::proof/to [1]}
                                1 {::proof/from [0]}}}
      {::proof/current-problem 0
       ::proof/premises        {0 {::proof/formula p
                                   ::proof/justification
                                   ::proof/assertion}}
       ::proof/problems        {0 {::proof/premises [0]
                                   ::proof/goal      q
                                   ::proof/id        0
                                   ::proof/status
                                   ::proof/open}
                                1 {::proof/premises []
                                   ::proof/goal     p->q
                                   ::proof/id        1
                                   ::proof/status
                                   ::proof/open}}
       ::proof/edges           {0 {::proof/to [1]}
                                1 {::proof/from [0]}}}
      ))

(deftest test-backward-gather-relevant-problem-idxs
  (let [p (formula/atom "@P")
        proof     {::proof/current-problem 2
                   ::proof/premises {}
                   ::proof/problems {0 {::proof/premises []
                                        ::proof/goal p
                                        ::proof/id 0
                                        ::proof/status ::proof/open}
                                     1 {::proof/premises []
                                        ::proof/goal p
                                        ::proof/id 0
                                        ::proof/status ::proof/open}
                                     2 {::proof/premises []
                                        ::proof/goal p
                                        ::proof/id 0
                                        ::proof/status ::proof/open}}
                   ::proof/edges {0 {::proof/to [1]}
                                  1 {::proof/to [2]
                                     ::proof/from [0]}
                                  2 {::proof/from [1]}}}]
    (are [proof problem-idx result-idxs]
        (= (proof/backward-gather-problem-idxs
            proof problem-idx)
           result-idxs)
        proof 2 [2 1 0]
        proof 1 [1 0]
        proof 0 [0])))

(deftest test-direct-proof
  (are [proof result]
      (= (proof/direct-proof proof)
         result)
    ;; Case 1
      assertion-hypothesis-proof
      {::proof/current-problem 0
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
                            ::proof/status ::proof/closed}}
       ::proof/edges {0 {::proof/to [1]}
                      1 {::proof/from [0]}}}
      ;; Case 2
      {::proof/current-problem 1
         ::proof/premises {0 {::proof/formula p
                              ::proof/justification ::proof/assertion}
                           1 {::proof/formula q
                              ::proof/justification ::proof/assertion}}
         ::proof/problems {0 {::proof/premises [0]
                              ::proof/goal     r
                              ::proof/id 0
                              ::proof/status ::proof/open}
                           1 {::proof/premises [1]
                              ::proof/goal     p
                              ::proof/id 1
                              ::proof/status ::proof/open}}
         ::proof/edges {0 {::proof/to [1]}
                        1 {::proof/from [0]}}}
      {::proof/current-problem 0
       ::proof/premises {0 {::proof/formula p
                            ::proof/justification ::proof/assertion}
                         1 {::proof/formula q
                            ::proof/justification ::proof/assertion}}
       ::proof/problems {0 {::proof/premises [0 1]
                            ::proof/goal     r
                            ::proof/id 0
                            ::proof/status ::proof/open}
                         1 {::proof/premises [1]
                            ::proof/goal     p
                            ::proof/id 1
                            ::proof/status ::proof/closed}}
       ::proof/edges {0 {::proof/to [1]}
                      1 {::proof/from [0]}}}
      ;; Case 3
      {::proof/current-problem 1
       ::proof/premises {0 (proof/new-premise p ::proof/assertion)}
       ::proof/problems {0 (proof/new-problem [0] r 0)
                         1 (proof/new-problem [] p 1)
                         2 (proof/new-problem [] q 2)}
       ::proof/edges {0 {::proof/to [1 2]}
                      1 {::proof/from [0]}
                      2 {::proof/from [0]}}}
      {::proof/current-problem 2
       ::proof/premises {0 (proof/new-premise p ::proof/assertion)}
       ::proof/problems {0 (proof/new-problem [0] r 0)
                         2 (proof/new-problem [] q 2)
                         1 {::proof/premises []
                            ::proof/goal p
                            ::proof/id  1
                            ::proof/status ::proof/closed}}
       ::proof/edges {0 {::proof/to [1 2]}
                      1 {::proof/from [0]}
                      2 {::proof/from [0]}}}))

(deftest test-filter-premises-by-problem-and-status
  (are [proof problem-idx status results]
      (= (proof/filter-premises-by-problem-and-status
          proof problem-idx status) results)
      assertion-hypothesis-proof 1 ::proof/hypothesis [1]
      assertion-hypothesis-proof 0 ::proof/assertion [0]))

(deftest test-conjunctive-proof
  (let [pandq (formula/conj p q)]
    (are [proof result]
        (= (proof/conjunctive-proof proof) result)
        {::proof/current-problem 0
         ::proof/premises {}
         ::proof/problems {0 {::proof/premises []
                              ::proof/goal pandq
                              ::proof/status ::proof/open
                              ::proof/id 0}}
         ::proof/edges {}}
        {::proof/current-problem 2
         ::proof/premises {}
         ::proof/problems {2 {::proof/premises []
                              ::proof/goal q
                              ::proof/status ::proof/open
                              ::proof/id 2}
                           1 {::proof/premises []
                              ::proof/goal p
                              ::proof/status ::proof/open
                              ::proof/id 1}
                           0 {::proof/premises []
                              ::proof/goal pandq
                              ::proof/status ::proof/open
                              ::proof/id 0}}
         ::proof/edges {0 {::proof/to [1 2]}
                        1 {::proof/from [0]}
                        2 {::proof/from [0]}}}
        assertion-hypothesis-proof assertion-hypothesis-proof)))

(deftest test-disjunctive-proof
  (let [porq (formula/disj p q)]
    (are [proof result]
        (= (proof/disjunctive-proof proof)
           result)
        {::proof/current-problem 0
         ::proof/premises {0 {::proof/formula p
                              ::proof/justification ::proof/assertion}}
         ::proof/problems {0 {::proof/premises [0]
                              ::proof/goal porq
                              ::proof/id 0
                              ::proof/status ::proof/open}}
         ::proof/edges {}}
        {::proof/current-problem nil
         ::proof/premises {0 {::proof/formula p
                              ::proof/justification ::proof/assertion}}
         ::proof/problems {0 {::proof/premises [0]
                              ::proof/goal porq
                              ::proof/id 0
                              ::proof/status ::proof/closed}}
         ::proof/edges {}})))
