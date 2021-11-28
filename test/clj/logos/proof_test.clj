(ns logos.proof-test
  (:require [clojure.test :refer [deftest is]]
            [logos.proof :as proof]
            [logos.formula :as formula]))
             

(deftest test-new-proof
  (let [f (formula/make-conjunction "p" "q")]
    (is (= (proof/new-proof [f] "p" [])
           {:premises [{:index 0
                        :formula f}]
            :goal "p"
            :subproofs []}))))

(deftest test-conditional-proof-one
  (let [f (formula/make-implication "p" "q")
        start-proof (proof/new-proof [] f [])]
    (is (= (proof/conditional-proof start-proof)
           {:premises []
            :goal f
            :subproofs [{:premises
                         [{:formula "p"
                           :index 0}]
                         :goal "q"
                         :subproofs []}]}))))

(deftest test-conditional-proof-two
  (let [f (formula/make-implication "p" "q")
        r "r"
        start-proof (proof/new-proof [r] f [])]
    (is (= {:premises [{:formula "r"
                        :index 1}
                       ]
            :goal f 
            :subproofs [
                        {:premises
                         [{:formula "p"
                           :index 0}]
                        :goal "q"
                         :subproofs []}
                        ]
            }
           
           (proof/conditional-proof start-proof)))))

(deftest test-conditional-proof-three
  (let [p "p"
        q "q"
        r "r"
        f (formula/make-implication p q)
        pandr (formula/make-conjunction p r)
        proof (proof/new-proof
               [r] q [(proof/new-proof [q] f [])
                      (proof/new-proof [pandr] q [])])]
    (is (= (proof/conditional-proof proof)
           {:premises [{:formula r
                        :index 3}]
            :goal q
            :subproofs [{:premises [{:formula q
                                     :index 1}]
                         :goal f
                         :subproofs [{:premises [{:formula p
                                                  :index 0}]
                                      :goal q
                                      :subproofs []}]}
                        {:premises [{:formula pandr
                                     :index 2}]
                         :goal q
                         :subproofs []}]}))))
           
                               
    
    
            
