(ns logos.proof-test
  (:require [clojure.test :refer [deftest is]]
            [logos.proof :as proof]
            [logos.formula :as formula]))


(deftest test-new-proof
  (let [f (formula/conj "p" "q")]
    (is (= (proof/new-proof [f] "p" [])
           {:premises [{:index 0
                        :formula f}]
            :status :open
            :goal "p"
            :subproofs []}))))

(deftest test-conditional-proof-one
  (let [p (formula/atom "@P" "null")
        q (formula/atom "@Q" "null")
        f (formula/implies p q)
        start-proof (proof/new-proof [] f [])]
    (println (formula/implication? f))
    (is (= (proof/conditional-proof start-proof)
           {:premises []
            :goal f
            :status :open
            :subproofs [{:premises
                         [{:formula p
                           :index 0}]
                         :status :open
                         :goal q
                         :subproofs []}]}))))

(deftest test-conditional-proof-two
  (let [p (formula/atom "@P" "null")
        q (formula/atom "@Q" "null")
        r (formula/atom "@R" "null")
        f (formula/implies p q)
        start-proof (proof/new-proof [r] f [])]
    (is (= {:premises [{:formula r
                        :index 1}
                       ]
            :goal f
            :status :open
            :subproofs [
                        {:premises
                         [{:formula p
                           :index 0}]
                         :status :open
                         :goal q
                         :subproofs []}
                        ]
            }
           (proof/conditional-proof start-proof)))))

(deftest test-conditional-proof-three
  (let [p (formula/atom "@P" "null")
        q (formula/atom "@Q" "null")
        r (formula/atom "@R" "null")
        f (formula/implies p q)
        pandr (formula/conj p r)
        proof (proof/new-proof
               [r] q [(proof/new-proof [q] f [])
                      (proof/new-proof [pandr] q [])])]
    (is (= (proof/conditional-proof proof)
           {:premises [{:formula r
                        :index 3}]
            :status :open
            :goal q
            :subproofs [{:premises [{:formula q
                                     :index 1}]
                         :status :open
                         :goal f
                         :subproofs [{:premises [{:formula p
                                                  :index 0}]
                                      :status :open
                                      :goal q
                                      :subproofs []}]}
                        {:premises [{:formula pandr
                                     :index 2}]
                         :status :open
                         :goal q
                         :subproofs []}]}))))

(deftest test-conditional-proof-four
  (let [a   (formula/atom "@p" '?x)
        ia  (formula/atom "@p" "b")
        p   (formula/forall
           '[?x] (formula/implies a a))
        pf  (proof/new-proof [] p [])
        universal (proof/universal-proof pf)
        conditional (proof/conditional-proof universal)]
    (is (= conditional
           (proof/new-proof
            [] p [(proof/new-proof
                   [] (formula/implies ia ia)
                   [(proof/new-proof [ia] ia [])])])))))



(deftest test-universal-proof-one
  (let [p (formula/forall '[?x]
                          (formula/atom "@P" '?x))
        proof (proof/new-proof [] p [])]
    (is (= (proof/universal-proof proof)
           {:premises []
            :status :open
            :goal p
            :subproofs [{:premises []
                         :status :open
                         :goal (formula/atom "@P" "b")
                         :subproofs []}]}))))

(deftest test-universal-proof-two
  (let [base (formula/forall '[?x]
                             (formula/atom "@P" '?x))
        form (formula/implies base base)]
    (is (= (proof/universal-proof
            (proof/conditional-proof
             (proof/new-proof [] form [])))
           {:premises []
            :status :open
            :goal form
            :subproofs
            [{:premises [{:formula base :index 0}]
              :status :open
              :goal base
              :subproofs
              [{:premises []
                :status :open
                :goal (formula/atom "@P" "b")
                :subproofs []}]}]}))))

(deftest test-conjunction-elimination
  (let [p  (formula/atom "@P" "a")
        q  (formula/atom "@Q" "a")
        f  (formula/conj p q)
        pf (proof/new-proof [f] p [])]
    (is (= (proof/conjunction-elimination pf 0)
           {:premises
            [{:formula f :index 0}
             {:formula p :index 1}
             {:formula q :index 2}]
            :goal p
            :status :open
            :subproofs []}))))
