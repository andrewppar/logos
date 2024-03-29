(ns logos.proof-premise-test
  (:require [clojure.test :refer [deftest are]]
            [logos.proof.premise :as premise]
            [logos.proof.proof :as proof]
            [logos.formula :as formula]))

;;;;;;;;;;;;;;;;;;;;
;;; Useful Constants

;;; TODO: This is repeated. Factor it out

(def p       (formula/atom "!P"))
(def q       (formula/atom "!Q"))
(def r       (formula/atom "!R"))
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
        pf ["1" "0"]  {::proof/current-problem 0
                   ::proof/premises {0 (proof/new-premise p)
                                     1 (proof/new-premise p->q)
                                     2 (proof/new-premise q ["1" "0"])}
                   ::proof/problems {0 (proof/new-problem [0 1 2] q 0)}
                   ::proof/edges {}}
        pf ["0" "1"]  {::proof/current-problem 0
                   ::proof/premises {0 (proof/new-premise p)
                                     1 (proof/new-premise p->q)
                                     2 (proof/new-premise q ["0" "1"])}
                   ::proof/problems {0 (proof/new-problem [0 1 2] q 0)}
                   ::proof/edges {}}
        pf ["1" "2"] pf)))

(deftest test-conjunction-elimination
  (are [proof idxs result]
      (= (premise/conjunction-elimination proof idxs)
         result)
      {::proof/current-problem 0
       ::proof/premises {0 (proof/new-premise
                            (formula/conj p q))}

       ::proof/problems {0 (proof/new-problem [0] q 0)}
       ::proof/edges {}} ["0"]

      {::proof/current-problem 0
       ::proof/premises {0 (proof/new-premise (formula/conj p q))
                         1 (proof/new-premise p ["0"])
                         2 (proof/new-premise q ["0"])}
       ::proof/problems {0 (proof/new-problem [0 1 2] q 0)}
       ::proof/edges {}}))

(deftest test-disjunction-elimination
  (are [proof premise-numbers result]
      (= (premise/disjunction-elimination proof premise-numbers)
         result)
    ;; Case 1
      {::proof/current-problem 0
       ::proof/premises {0 (proof/new-premise
                            (formula/disj p q))}

       ::proof/problems {0 (proof/new-problem [0] q 0)}
       ::proof/edges {}} ["0"]
      {::proof/current-problem 1
       ::proof/premises {0 (proof/new-premise
                            (formula/disj p q))
                         1 (proof/new-premise p ::proof/hypothesis)
                         2 (proof/new-premise q ::proof/hypothesis)}
       ::proof/problems {0 (proof/new-problem [0] q 0)
                         1 (proof/new-problem [1] q 1)
                         2 (proof/new-problem [2] q 2)}
       ::proof/edges {0 {::proof/to [1 2]}
                      1 {::proof/from [0]}
                      2 {::proof/from [0]}}}
      ;; Case 2
      {::proof/current-problem 2,
       ::proof/premises {0
                         {::proof/formula [:or [:and p q] r],
                          ::proof/justification ::proof/hypothesis},
                         1 {::proof/formula [:and p q],
                            ::proof/justification ::proof/hypothesis},
                         2 {::proof/formula r,
                            ::proof/justification ::proof/hypothesis},
                         3 {::proof/formula p,
                            ::proof/justification [1]},
                         4 {::proof/formula q,
                            ::proof/justification [1]}},
       ::proof/problems {0 {::proof/premises nil,
                            ::proof/goal [:implies
                                          [:or [:and p q] r]
                                          [:and
                                           [:or p r]
                                           [:or q r]]],
                            ::proof/id 0,
                            ::proof/status
                            ::proof/open,
                            ::proof/type
                            ::proof/proof},
                         1 {::proof/premises [0],
                            ::proof/goal [:and [:or p r] [:or q r]],
                            ::proof/id 1,
                            ::proof/status ::proof/open,
                            ::proof/type ::proof/proof},
                         2 {::proof/premises [],
                            ::proof/goal [:or p r],
                            ::proof/id 2,
                            ::proof/status ::proof/open,
                            ::proof/type ::proof/proof},
                         3 {::proof/premises [],
                            ::proof/goal [:or q r],
                            ::proof/id 3,
                            ::proof/status
                            ::proof/closed,
                            ::proof/type
                            ::proof/proof},
                         4 {::proof/premises [1 3 4],
                            ::proof/goal [:or q r],
                            ::proof/id 4,
                            ::proof/status ::proof/closed,
                            ::proof/type ::proof/proof},
                         5 {::proof/premises [2],
                            ::proof/goal [:or q r],
                            ::proof/id 5,
                            ::proof/status ::proof/closed,
                            ::proof/type ::proof/proof}},
       ::proof/theorem-name "case 2",
       ::proof/edges {0 {::proof/to [1]},
                      1 {::proof/from [0], ::proof/to [2 3]},
                      2 {::proof/from [1]},
                      3 {::proof/from [1], ::proof/to [4 5]},
                      4 {::proof/from [3]},
                      5 {::proof/from [3]}}} ["0"]
      {::proof/current-problem 6,
       ::proof/premises {0 {::proof/formula [:or [:and p q] r],
                            ::proof/justification ::proof/hypothesis},
                         1 {::proof/formula [:and p q],
                            ::proof/justification ::proof/hypothesis},
                         2 {::proof/formula r,
                            ::proof/justification ::proof/hypothesis},
                         3 {::proof/formula p,
                            ::proof/justification [1]},
                         4 {::proof/formula q,
                            ::proof/justification [1]}
                         5 {::proof/formula [:and p q]
                            ::proof/justification ::proof/hypothesis}
                         6 {::proof/formula r,
                            ::proof/justification ::proof/hypothesis}},
       ::proof/problems {0
                         {::proof/premises nil,
                          ::proof/goal [:implies
                                        [:or [:and p q] r]
                                        [:and
                                         [:or p r]
                                         [:or q r]]],
                          ::proof/id 0,
                          ::proof/status
                          ::proof/open,
                          ::proof/type
                          ::proof/proof},
                         1 {::proof/premises [0],
                            ::proof/goal [:and [:or p r] [:or q r]],
                            ::proof/id 1,
                            ::proof/status ::proof/open,
                            ::proof/type ::proof/proof},
                         2 {::proof/premises [],
                            ::proof/goal [:or p r],
                            ::proof/id 2,
                            ::proof/status ::proof/open,
                            ::proof/type ::proof/proof},
                         3 {::proof/premises [],
                            ::proof/goal [:or q r],
                            ::proof/id 3,
                            ::proof/status
                            ::proof/closed,
                            ::proof/type
                            ::proof/proof},
                         4 {::proof/premises [1 3 4],
                            ::proof/goal [:or q r],
                            ::proof/id 4,
                            ::proof/status ::proof/closed,
                            ::proof/type ::proof/proof},
                         5 {::proof/premises [2],
                            ::proof/goal [:or q r],
                            ::proof/id 5,
                            ::proof/status ::proof/closed,
                            ::proof/type ::proof/proof}
                         6 {::proof/premises [5],
                            ::proof/goal [:or p r],
                            ::proof/id 6,
                            ::proof/type ::proof/proof
                            ::proof/status ::proof/open}
                         7 {::proof/premises [6]
                            ::proof/goal [:or p r]
                            ::proof/type ::proof/proof
                            ::proof/id 7
                            ::proof/status ::proof/open}},
       ::proof/theorem-name "case 2",
       ::proof/edges {0 {::proof/to [1]},
                      1 {::proof/from [0], ::proof/to [2 3]},
                      2 {::proof/from [1], ::proof/to [6 7]},
                      3 {::proof/from [1], ::proof/to [4 5]},
                      4 {::proof/from [3]},
                      5 {::proof/from [3]},
                      6 {::proof/from [2]},
                      7 {::proof/from [2]}}}


      ))

(deftest bottom-elimination-test
  (are [proof premise-idxs result]
      (= (premise/bottom-introduction proof premise-idxs)
         result)
      {::proof/current-problem 0
       ::proof/premises {0 (proof/new-premise p)
                         1 (proof/new-premise [:not p])}
       ::proof/problems {0 (proof/new-problem [0 1] q 0)}
       ::proof/edges {}} ["0" "1"]

      {::proof/current-problem 0
       ::proof/premises {0 (proof/new-premise p)
                         1 (proof/new-premise [:not p])
                         2 (proof/new-premise ::formula/bottom ["0" "1"])}
       ::proof/problems {0 (proof/new-problem [0 1 2] q 0)}
       ::proof/edges {}}))

(deftest universal-elimination-test
  (are [proof args result]
      (= (premise/universal-elimination proof args)
         result)
    ;; Case 1
      {::proof/current-problem 0
       ::proof/premises {0 (proof/new-premise
                            '[:forall [?x] ["!P" ?x]])}
       ::proof/problems {0 (proof/new-problem [0] ["!Q"] 0)}
       ::proof/edges {}} ["0" "a"]
      {::proof/current-problem 0
       ::proof/premises {0 (proof/new-premise
                            '[:forall [?x] ["!P" ?x]])
                         1 (proof/new-premise
                            ["!P" "a"] ["0" "a"])}
       ::proof/problems {0 (proof/new-problem [0 1] ["!Q"] 0)}
       ::proof/edges {}}
      ;; Case 2
      {::proof/current-problem 0
       ::proof/premises {0 (proof/new-premise
                            '[:forall [?x]
                              [:exists [?y]
                               ["!equal" ?y ["!Successor" ?x]]]])}
       ::proof/problems {0 (proof/new-problem [0] ["!Q"] 0)}
       ::proof/edges {}} ["0" "a"]
      {::proof/current-problem 0
       ::proof/premises {0 (proof/new-premise
                            '[:forall [?x]
                              [:exists [?y]
                               ["!equal" ?y ["!Successor" ?x]]]])
                         1 (proof/new-premise 
                            '[:exists [?y]
                              ["!equal" ?y ["!Successor" "a"]]] 
                            ["0" "a"])}
       ::proof/problems {0 (proof/new-problem [0 1] ["!Q"] 0)}
       ::proof/edges {}}
                                            
      ))

(deftest test-existential-elimination
  (are [proof premise-idxs result]
      (= (premise/existential-elimination proof premise-idxs)
         result)
    {::proof/current-problem 0
     ::proof/premises {0 (proof/new-premise '[:exists [?x ?y]
                                              ["!P" ?x ?y "a"]])}
     ::proof/problems {0
                       (proof/new-problem [0] ["!Q"] 0)}
     ::proof/edges {}} ["0"]
    {::proof/current-problem 0
     ::proof/premises {0 (proof/new-premise '[:exists [?x ?y]
                                              ["!P" ?x ?y "a"]])
                       1 (proof/new-premise '["!P" "b" "c" "a"] ["0"])}
     ::proof/problems {0
                       (proof/new-problem [0 1] ["!Q"] 0)}
     ::proof/edges {}}))

(deftest test-equality-substitution
  (are [proof args result]
      (= (premise/substitute-equality proof args) result)
    {::proof/current-problem 0
     ::proof/premises {0 (proof/new-premise '["!equals" "a" "b"])
                       1 (proof/new-premise '["!R" "a" "c"])}
     ::proof/problems {0 (proof/new-problem [0 1] '["!R" "b" "c"] 0)}
     ::proof/edges    {}} ["0" "1" "1"]
    {::proof/current-problem 0
     ::proof/premises {0 (proof/new-premise '["!equals" "a" "b"])
                       1 (proof/new-premise '["!R" "a" "c"])
                       2 (proof/new-premise '["!R" "b" "c"] [1 0])}
     ::proof/problems {0 (proof/new-problem [0 1 2] '["!R" "b" "c"] 0)}
     ::proof/edges    {}}))
