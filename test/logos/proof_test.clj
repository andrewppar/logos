(ns logos.proof-test
  (:require [clojure.test :refer [deftest are]]
            [logos.proof.proof :as proof
             :refer [add-edges-to-edge-index]]
            [logos.formula :as formula]))

;;;;;;;;;;;;;;;;;;;;
;;; Useful Constants

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

(deftest test-filter-premises-by-problem-and-status
  (are [proof problem-idx status results]
      (= (proof/filter-premises-by-problem-and-status
          proof problem-idx status) results)
      assertion-hypothesis-proof 1 ::proof/hypothesis [1]
      assertion-hypothesis-proof 0 ::proof/assertion [0]))
