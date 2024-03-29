(ns logos.proof-goal-test
  (:require [clojure.test :refer [deftest are is]]
            [logos.proof.proof :as proof]
            [logos.proof.goal  :as goal]
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
                              ::proof/type ::proof/proof
                              ::proof/status ::proof/open}
                           1 {::proof/premises [1]
                              ::proof/goal     p
                              ::proof/id 1
                              ::proof/type ::proof/proof
                              ::proof/status ::proof/open}}
         ::proof/edges {0 {::proof/to [1]}
                        1 {::proof/from [0]}}})

(deftest test-conditional-proofs
  (are [start-proof result-proof]
      (= (goal/conditional-proof start-proof)
         result-proof)
    ;; Case 1
      {::proof/current-problem 0
       ::proof/premises        {}
       ::proof/problems        {0 {::proof/premises []
                                   ::proof/goal   p->q
                                   ::proof/id        0
                                   ::proof/type ::proof/proof
                                   ::proof/status ::proof/open}}
       ::proof/edges           {}}
      {::proof/current-problem 1
       ::proof/premises        {0 {::proof/formula p
                                   ::proof/justification
                                   ::proof/hypothesis}}
       ::proof/problems       {0 {::proof/premises []
                                  ::proof/goal   p->q
                                  ::proof/id        0
                                  ::proof/type ::proof/proof
                                  ::proof/status ::proof/open}
                               1 {::proof/premises [0]
                                  ::proof/goal q
                                  ::proof/id 1
                                  ::proof/type ::proof/proof
                                  ::proof/status ::proof/open}}
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
                                   ::proof/type ::proof/proof
                                   ::proof/status ::proof/open}
                                1 {::proof/premises []
                                   ::proof/goal     p->q
                                   ::proof/id        1
                                   ::proof/type ::proof/proof
                                   ::proof/status ::proof/open}}
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
                                   ::proof/type ::proof/proof
                                   ::proof/status ::proof/open}
                                1 {::proof/premises []
                                   ::proof/goal     p->q
                                   ::proof/id        1
                                   ::proof/type ::proof/proof
                                   ::proof/status ::proof/open}
                                2 {::proof/premises [1]
                                   ::proof/goal      q
                                   ::proof/id        2
                                   ::proof/type ::proof/proof
                                   ::proof/status ::proof/open}
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
                                   ::proof/type ::proof/proof
                                   ::proof/status ::proof/open}
                                1 {::proof/premises []
                                   ::proof/goal     p->q
                                   ::proof/id        1
                                   ::proof/type ::proof/proof
                                   ::proof/status ::proof/open}}
       ::proof/edges           {0 {::proof/to [1]}
                                1 {::proof/from [0]}}}
      {::proof/current-problem 0
       ::proof/premises        {0 {::proof/formula p
                                   ::proof/justification
                                   ::proof/assertion}}
       ::proof/problems        {0 {::proof/premises [0]
                                   ::proof/goal      q
                                   ::proof/id        0
                                   ::proof/type ::proof/proof
                                   ::proof/status ::proof/open}
                                1 {::proof/premises []
                                   ::proof/goal     p->q
                                   ::proof/id        1
                                   ::proof/type ::proof/proof
                                   ::proof/status ::proof/open}}
       ::proof/edges           {0 {::proof/to [1]}
                                1 {::proof/from [0]}}}))

(deftest test-direct-proof
  (are [proof result]
      (= (goal/direct-proof proof)
        result)
    ;; Case 1
      assertion-hypothesis-proof
      {::proof/current-problem nil
       ::proof/premises {0 {::proof/formula p
                            ::proof/justification ::proof/assertion}
                         1 {::proof/formula q
                            ::proof/justification ::proof/hypothesis}}
       ::proof/problems {0 {::proof/premises [0]
                            ::proof/goal     r
                            ::proof/id 0
                            ::proof/type ::proof/proof
                            ::proof/status ::proof/closed}
                         1 {::proof/premises [1]
                            ::proof/goal     p
                            ::proof/id 1

                            ::proof/type ::proof/proof
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
                            ::proof/type ::proof/proof
                            ::proof/status ::proof/open}
                         1 {::proof/premises [1]
                            ::proof/goal     p
                            ::proof/id 1
                            ::proof/type ::proof/proof
                            ::proof/status ::proof/open}}
       ::proof/edges {0 {::proof/to [1]}
                      1 {::proof/from [0]}}}
      {::proof/current-problem nil
       ::proof/premises {0 {::proof/formula p
                            ::proof/justification ::proof/assertion}
                         1 {::proof/formula q
                            ::proof/justification ::proof/assertion}}
       ::proof/problems {0 {::proof/premises [0 1]
                            ::proof/goal     r
                            ::proof/id 0
                            ::proof/type ::proof/proof
                            ::proof/status ::proof/closed}
                         1 {::proof/premises [1]
                            ::proof/goal     p
                            ::proof/id 1
                            ::proof/type ::proof/proof
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
                            ::proof/type ::proof/proof
                            ::proof/status ::proof/closed}}
       ::proof/edges {0 {::proof/to [1 2]}
                      1 {::proof/from [0]}
                      2 {::proof/from [0]}}}

      ;; Case 4
      {::proof/current-problem 2
       ::proof/premises {0 {::proof/formula ["!P"]
                            ::proof/justification ::proof/hypothesis}
                         1 {::proof/formula ["!Q"]
                            ::proof/justification ::proof/hypothesis}}
       ::proof/problems {0 {::proof/premises ()
                            ::proof/goal [:implies
                                          ["!P"]
                                          [:implies ["!Q"] ["!P"]]]
                            ::proof/id 0
                            ::proof/type ::proof/proof
                            ::proof/status ::proof/open}
                         1 {::proof/premises [0]
                            ::proof/goal [:implies ["!Q"] ["!P"]]
                            ::proof/id 1
                            ::proof/type ::proof/proof
                            ::proof/status ::proof/open}
                         2 {::proof/premises [1]
                            ::proof/goal ["!P"]
                            ::proof/id 2
                            ::proof/type ::proof/proof
                            ::proof/status ::proof/open}}
       ::proof/edges
       {0 {::proof/to [1]}
        1 {::proof/from [0] ::proof/to [2]},
        2 {::proof/from [1]}}}

      {::proof/current-problem nil
       ::proof/premises {0 {::proof/formula ["!P"]
                            ::proof/justification ::proof/hypothesis}
                         1 {::proof/formula ["!Q"]
                            ::proof/justification ::proof/hypothesis}}
       ::proof/problems {0 {::proof/premises ()
                            ::proof/goal [:implies
                                          ["!P"]
                                          [:implies ["!Q"] ["!P"]]]
                            ::proof/id 0
                            ::proof/type ::proof/proof
                            ::proof/status ::proof/closed}
                         1 {::proof/premises [0]
                            ::proof/goal [:implies ["!Q"] ["!P"]]
                            ::proof/id 1
                            ::proof/type ::proof/proof
                            ::proof/status ::proof/closed}
                         2 {::proof/premises [1]
                            ::proof/goal ["!P"]
                            ::proof/id 2
                            ::proof/type ::proof/proof
                            ::proof/status ::proof/closed}}
       ::proof/edges
       {0 {::proof/to [1]}
        1 {::proof/from [0] ::proof/to [2]},
        2 {::proof/from [1]}}}

      ;; Case 5
      {::proof/current-problem 0
       ::proof/premises {0 {::proof/formula ::formula/bottom
                            ::proof/justification ::proof/assertion}}
       ::proof/problems {0 {::proof/premises [0]
                            ::proof/goal q
                            ::proof/id 0
                            ::proof/type ::proof/proof
                            ::proof/status ::proof/open}}
       ::proof/edges {}}
      {::proof/current-problem nil
       ::proof/premises {0 {::proof/formula ::formula/bottom
                            ::proof/justification ::proof/assertion}}
       ::proof/problems {0 {::proof/premises [0]
                            ::proof/goal q
                            ::proof/id 0
                            ::proof/type ::proof/proof
                            ::proof/status ::proof/closed}}
       ::proof/edges {}}

      ;; Case 6
      {::proof/current-problem 1
       ::proof/premises {0 {::proof/formula ::formula/bottom
                            ::proof/justification ::proof/assertion}
                         1 {::proof/formula ["!A"]
                            ::proof/justification ::proof/hypothesis}}
       ::proof/problems {0 {::proof/premises [0]
                            ::proof/goal q
                            ::proof/id 0
                            ::proof/type ::proof/proof
                            ::proof/status ::proof/open}
                         1 {::proof/premises [1]
                            ::proof/goal ["!A"]
                            ::proof/id 1
                            ::proof/status ::proof/open
                            ::proof/type   ::proof/assert}}
       ::proof/edges {0 {::proof/to [1]}
                      1 {::proof/from[0]}}}
      {::proof/current-problem 0
       ::proof/premises {0 {::proof/formula ::formula/bottom
                            ::proof/justification ::proof/assertion}
                         1 {::proof/formula ["!A"]
                            ::proof/justification ::proof/hypothesis}
                         2 {::proof/formula ["!A"]
                            ::proof/justification ::proof/proved}}
       ::proof/problems {0 {::proof/premises [0 2]
                            ::proof/goal q
                            ::proof/id 0
                            ::proof/type ::proof/proof
                            ::proof/status ::proof/open}
                         1 {::proof/premises [1]
                            ::proof/goal ["!A"]
                            ::proof/id 1
                            ::proof/status ::proof/closed
                            ::proof/type   ::proof/assert}}
       ::proof/edges {0 {::proof/to [1]}
                      1 {::proof/from[0]}}}
      ;; Case 7
      {::proof/current-problem 1
       ::proof/premises {0 {::proof/formula ::formula/bottom
                            ::proof/justification ::proof/assertion}
                         1 {::proof/formula ["!A"]
                            ::proof/justification ::proof/hypothesis}}
       ::proof/problems {0 {::proof/premises [0]
                            ::proof/goal q
                            ::proof/id 0
                            ::proof/type ::proof/proof
                            ::proof/status ::proof/open}
                         1 {::proof/premises [1]
                            ::proof/goal ["!A"]
                            ::proof/id 1
                            ::proof/status ::proof/open
                            ::proof/type   ::proof/assert}
                         2 {::proof/premises [1]
                            ::proof/id 2
                            ::proof/status ::proof/open
                            ::proof/type   ::proof/proof}}
       ::proof/edges {0 {::proof/to [1 2]}
                      1 {::proof/from [0]}
                      2 {::proof/from [0]}}}
      {::proof/current-problem 2
       ::proof/premises {0 {::proof/formula ::formula/bottom
                            ::proof/justification ::proof/assertion}
                         1 {::proof/formula ["!A"]
                            ::proof/justification ::proof/hypothesis}
                         2 {::proof/formula ["!A"]
                            ::proof/justification ::proof/proved}}
       ::proof/problems {0 {::proof/premises [0 2]
                            ::proof/goal q
                            ::proof/id 0
                            ::proof/type ::proof/proof
                            ::proof/status ::proof/open}
                         1 {::proof/premises [1]
                            ::proof/goal ["!A"]
                            ::proof/id 1
                            ::proof/status ::proof/closed
                            ::proof/type   ::proof/assert}
                         2 {::proof/premises [1]
                            ::proof/id 2
                            ::proof/status ::proof/open
                            ::proof/type   ::proof/proof}}
       ::proof/edges {0 {::proof/to [1 2]}
                      1 {::proof/from [0]}
                      2 {::proof/from [0]}}}

      ;; Case 8
      {::proof/current-problem 0
       ::proof/problems {0 (proof/new-problem [] ["!equals" "a" "a"] 0)}
       ::proof/premises {}
       ::proof/edges  {}}
      {::proof/current-problem nil
       ::proof/problems {0 {::proof/premises []
                            ::proof/goal ["!equals" "a" "a"]
                            ::proof/id 0
                            ::proof/status ::proof/closed
                            ::proof/type ::proof/proof}}
       ::proof/premises {}
       ::proof/edges  {}}
      ))

(deftest test-conjunctive-proof
  (let [pandq (formula/conj p q)]
    (are [proof result]
        (= (goal/conjunctive-proof proof) result)
        {::proof/current-problem 0
         ::proof/premises {}
         ::proof/problems {0 {::proof/premises []
                              ::proof/goal pandq
                              ::proof/type ::proof/proof
                              ::proof/status ::proof/open
                              ::proof/id 0}}
         ::proof/edges {}}
        {::proof/current-problem 2
         ::proof/premises {}
         ::proof/problems {2 {::proof/premises []
                              ::proof/goal q
                              ::proof/status ::proof/open
                              ::proof/type ::proof/proof
                              ::proof/id 2}
                           1 {::proof/premises []
                              ::proof/goal p
                              ::proof/type ::proof/proof
                              ::proof/status ::proof/open
                              ::proof/id 1}
                           0 {::proof/premises []
                              ::proof/goal pandq
                              ::proof/type ::proof/proof
                              ::proof/status ::proof/open
                              ::proof/id 0}}
         ::proof/edges {0 {::proof/to [1 2]}
                        1 {::proof/from [0]}
                        2 {::proof/from [0]}}}
        assertion-hypothesis-proof assertion-hypothesis-proof)))

(deftest test-disjunctive-proof
  (let [porq (formula/disj p q)]
    (are [proof result]
        (= (goal/disjunctive-proof proof)
           result)
      ;; This is failing because reset-current-problem
      ;; can be called on a proof whose current-problem
      ;; has already been set to nil
        {::proof/current-problem 0
         ::proof/premises {0 {::proof/formula p
                              ::proof/justification ::proof/assertion}}
         ::proof/problems {0 {::proof/premises [0]
                              ::proof/goal porq
                              ::proof/id 0
                              ::proof/type ::proof/proof
                              ::proof/status ::proof/open}}
         ::proof/edges {}}
        {::proof/current-problem nil
         ::proof/premises {0 {::proof/formula p
                              ::proof/justification ::proof/assertion}}
         ::proof/problems {0 {::proof/premises [0]
                              ::proof/goal porq
                              ::proof/id 0
                              ::proof/type ::proof/proof
                              ::proof/status ::proof/closed}}
         ::proof/edges {}})))

(deftest test-negative-proof
  (are [proof result]
      (= (goal/negative-proof proof)
         result)
      assertion-hypothesis-proof assertion-hypothesis-proof
      {::proof/current-problem 0
       ::proof/premises {}
       ::proof/problems {0 {::proof/premises []
                            ::proof/goal [:not ["!P"]]
                            ::proof/id 0
                            ::proof/type ::proof/proof
                            ::proof/status ::proof/open}}
       ::proof/edges {}}
      {::proof/current-problem 1
       ::proof/premises {0 {::proof/formula ["!P"]
                            ::proof/justification ::proof/hypothesis}}
       ::proof/problems {0 {::proof/premises []
                            ::proof/goal [:not ["!P"]]
                            ::proof/id 0
                            ::proof/type ::proof/proof
                            ::proof/status ::proof/open}
                         1 {::proof/premises [0]
                            ::proof/goal ::formula/bottom
                            ::proof/id 1
                            ::proof/type ::proof/proof
                            ::proof/status ::proof/open}}
       ::proof/edges {0 {::proof/to [1]}
                      1 {::proof/from [0]}}}))

(deftest test-universal-proof
  (are [proof result]
      (= (goal/universal-proof proof)
         result)
    ;; case 1
      {::proof/current-problem 0
       ::proof/premises {}
       ::proof/problems {0 {::proof/premises []
                            ::proof/goal '[:forall [?x] ["!P" ?x]]
                            ::proof/id 0
                            ::proof/type ::proof/proof
                            ::proof/status ::proof/open}}
       ::proof/edges {}}
      {::proof/current-problem 1
       ::proof/premises {}
       ::proof/problems {0 {::proof/premises []
                            ::proof/goal '[:forall [?x] ["!P" ?x]]
                            ::proof/id 0
                            ::proof/type ::proof/proof
                            ::proof/status ::proof/open}
                         1 {::proof/premises []
                            ::proof/goal '["!P" "b"]
                            ::proof/id 1
                            ::proof/type ::proof/proof
                            ::proof/status ::proof/open}}
       ::proof/edges {0 {::proof/to [1]}
                      1 {::proof/from [0]}}}
      ;; Case 2
      {::proof/current-problem 0
       ::proof/premises {}
       ::proof/problems {0 {::proof/premises []
                            ::proof/goal '[:forall [?p] [:implies ?p ?p]]
                            ::proof/id 0
                            ::proof/type ::proof/proof
                            ::proof/status ::proof/open}}
       ::proof/edges {}}
      {::proof/current-problem 1
       ::proof/premises {}
       ::proof/problems {0 {::proof/premises []
                            ::proof/goal '[:forall [?p] [:implies ?p ?p]]
                            ::proof/id 0
                            ::proof/type ::proof/proof
                            ::proof/status ::proof/open}
                         1 {::proof/premises []
                            ::proof/goal '[:implies "b" "b"]
                            ::proof/id 1
                            ::proof/type ::proof/proof
                            ::proof/status ::proof/open}}
       ::proof/edges {0 {::proof/to [1]}
                      1 {::proof/from [0]}}}

      ;; Case 3
      {::proof/current-problem 0
       ::proof/premises {}
       ::proof/problems {0  {::proof/premises []
                             ::proof/goal '[:forall
                                            [?x]
                                            [:or ["!P" ?x] ["!Q" ?x]]]
                             ::proof/id 0
                             ::proof/type ::proof/proof
                             ::proof/status ::proof/open}}
       ::proof/edges {}}
{::proof/current-problem 1
 ::proof/premises {}
 ::proof/problems {0 {::proof/premises []
                      ::proof/goal '[:forall
                                     [?x]
                                     [:or ["!P" ?x] ["!Q" ?x]]]
                      ::proof/id 0
                      ::proof/type ::proof/proof
                      ::proof/status ::proof/open}
                   1 {::proof/premises []
                      ::proof/goal '[:or ["!P" "b"] ["!Q" "b"]]
                      ::proof/id 1
                      ::proof/type ::proof/proof
                      ::proof/status ::proof/open}
                   }
 ::proof/edges {0 {::proof/to [1]}
                1 {::proof/from [0]}}}
      ))

(deftest test-existential-proof
  (are [proof constants result]
      (= (goal/existential-proof proof constants)
         result)
    {::proof/current-problem 0
     ::proof/premises {}
     ::proof/problems {0 {::proof/premises []
                         ::proof/goal '[:exists [?x ?y] ["!P" ?x ?y]]
                          ::proof/id 0
                          ::proof/type ::proof/proof
                         ::proof/status ::proof/open}}
     ::proof/edges {}} ["a" "b"]
    {::proof/current-problem 1
     ::proof/premises {}
     ::proof/problems {0 {::proof/premises []
                          ::proof/goal '[:exists [?x ?y] ["!P" ?x ?y]]
                          ::proof/id 0
                          ::proof/type ::proof/proof
                          ::proof/status ::proof/open}
                      1 {::proof/premises []
                         ::proof/goal '["!P" "a" "b"]
                         ::proof/id 1
                         ::proof/type ::proof/proof
                         ::proof/status ::proof/open}}
     ::proof/edges {0 {::proof/to [1]}
                    1 {::proof/from [0]}}}))

(deftest test-existentital-proof-exception
  (is (thrown? java.lang.Exception
               (goal/existential-proof
                {::proof/current-problem 0
                 ::proof/premises {}
                 ::proof/problems {0 {::proof/premises []
                                      ::proof/goal
                                      '[:exists [?x ?y] ["!P" ?x ?y]]
                                      ::proof/id 0
                                      ::proof/type ::proof/proof
                                      ::proof/status ::proof/open}}
                 ::proof/edges {}} '["a" ?z]))))

(deftest test-assert
  (are [proof formula result]
      (= (goal/assert proof formula) result)
      {::proof/current-problem 0
       ::proof/premises {}
       ::proof/problems {0 {::proof/premises []
                            ::proof/goal '["!F" "a"]
                            ::proof/id 0
                            ::proof/type ::proof/proof
                            ::proof/status ::proof/open}}
       ::proof/edges {}}
      '[:implies ["p"] ["p"]]
      {::proof/current-problem 1
       ::proof/premises {}
       ::proof/problems {0 {::proof/premises []
                            ::proof/goal '["!F" "a"]
                            ::proof/id 0
                            ::proof/status ::proof/open
                            ::proof/type ::proof/proof}
                         1 {::proof/premises []
                            ::proof/goal '[:implies ["p"] ["p"]]
                            ::proof/id 1
                            ::proof/status ::proof/open
                            ::proof/type ::proof/assert}}
       ::proof/edges {0 {::proof/to [1]}
                      1 {::proof/from [0]}}}))
