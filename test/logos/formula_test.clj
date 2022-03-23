(ns logos.formula-test
  (:require [clojure.test :refer [deftest is are]]
            [logos.formula :as formula]))

(deftest constant?-test
  (are [object outcome]
      (= (formula/constant? object) outcome)
    "q"              true
    0                false
    '?x              false
    (fn [x] (= x x)) false
    ""               true
    "!pred"          false
    "adfa"           true))

(deftest variable?-test
  (are [object outcome]
      (= (formula/variable? object) outcome)
    "q"     false
    0       false
    '?x     true
    '?xyzab true
    'x      false))

(deftest atom?-test
  (are [object outcome]
      (= (formula/atom? object) outcome)
    "q"                                  true
    (formula/atom "!Pred" "arg1" "arg2") true
    (formula/atom
     (formula/lambda '[?x ?y]
                     (formula/atom
                      "!pred" '?x '?y))
     "one" "two")                        true
    1                                    false
    (formula/atom
     (formula/lambda '[?x ?y]
                     (formula/conj
                      (formula/atom
                       "!pred" '?x "a")
                      (formula/atom
                       "!pred1" '?y "b")))
     "one" "two")                         true
    ))

(deftest test-next-constant
  (are [test-string expectation]
      (= (formula/next-constant test-string) expectation)
    "z"    "aa"
    "a"    "b"
    "aza"  "azb"
    "azz"  "baa"
    "bczz" "bdaa"))

(deftest test-instantiate-new-variables
  (are [test-formula result-formula]
      (= (formula/instantiate-new-variables test-formula [])
         result-formula)
    (formula/forall '[?x] (formula/atom "!pred1" '?x "b"))
    (formula/atom "!pred1" "c" "b")
    (formula/forall '[?x] (formula/atom "!pred1" '?x '?y))
    (formula/atom "!pred1" "b" '?y)
    (formula/exists '[?x] (formula/atom "!pred1" '?x "b"))
    (formula/atom "!pred1" "c" "b")
    (formula/exists '[?x] (formula/atom "!pred1" '?x '?y))
    (formula/atom "!pred1" "b" '?y)))

(deftest test-negation
  (are [formula expectation]
      (= (formula/negation? formula) expectation)
    ::formula/bottom false
    [:not ["!P"]]    true))

(deftest test-negatum
  (are [formula result]
      (= (formula/negatum formula) result)
    [:not ["!P"]] ["!P"]
    [:not [:implies ["!P" "q"] ["!P" "r"]]]
    [:implies ["!P" "q"] ["!P" "r"]]))

(deftest formula-gather
  (are [formula gather-fn result]
       (= (formula/formula-gather formula gather-fn)
	  result)
    '[:forall [?x] [:implies ["!P" ?x] ["!Q" ?x]]] formula/implication?
    '[[:implies ["!P" ?x] ["!Q" ?x]]]

    '[:forall [?x] [:implies ["!P" ?x] ["!Q" ?x]]] formula/variable?
    '[?x ?x ?x]

    '[:forall [?x] [:implies ["!P" ?x] ["!Q" ?x]]] formula/universal?
    '[[:forall [?x] [:implies ["!P" ?x] ["!Q" ?x]]]]

    '[:forall [?x] [:implies ["!P" ?x] ["!Q" ?x]]] formula/constant?
    []

    '[:forall [?x] [:implies ["!P" ?x] ["!Q" "b"]]] formula/constant?
    ["b"]

    '[:implies ["!R"] [:forall [?x] [:implies ["!P" ?x] ["!Q" "b"]]]]
    formula/implication?
    '[[:implies ["!R"] [:forall [?x] [:implies ["!P" ?x] ["!Q" "b"]]]]
      [:implies ["!P" ?x] ["!Q" "b"]]]))

(deftest read-formula-test
  (are [string result]
      (= (formula/read-formula string) result)
    "(implies ?x ?x)" '[:implies ?x ?x]
    "(forall ?x
      (implies (!P ?x) (!P ?x)))" '[:forall [?x]
                                    [:implies
                                     ["!P" ?x]
                                     ["!P" ?x]]]))
