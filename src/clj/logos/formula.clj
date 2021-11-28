(ns logos.formula
  (:require [clojure.spec.alpha :as spec]
            [clojure.spec.gen.alpha :as gen]
            ))

(spec/def ::constant (spec/and string? #(not (= % ""))))
(spec/def ::variable symbol?)

(spec/def ::term (spec/or :constant ::constant :var ::variable))

(spec/def ::arguments (spec/coll-of
                       ::term :kind vector?))

(spec/def ::predicate (spec/or :atomic string? :compound ::lambda))

(spec/def ::atom
  (spec/keys :req [::atomic-predicate ::arguments]))

(spec/def ::conjuncts (spec/coll-of ::formula))
(spec/def ::conjunction
  (spec/keys :req [::conjuncts]))


(spec/def ::disjuncts (spec/coll-of ::formula))
(spec/def ::disjunction
  (spec/keys :req [::disjuncts]))

(spec/def ::antecedent ::formula)
(spec/def ::consequent ::formula)
(spec/def ::implication
  (spec/keys :req [::antecendent ::consequent]))


(spec/def ::negatum ::formula)
(spec/def ::negation (spec/keys :req [::negatum]))


(spec/def ::bound-formula ::formula)

(spec/def ::universal-vars (spec/coll-of ::variable))
(spec/def ::universal
  (spec/keys :req [::universal-vars ::bound-formula]))

(spec/def ::existential-vars (spec/coll-of ::variable))
(spec/def ::existential
  (spec/keys :req [:existential-vars ::bound-formula]))

(spec/def ::lambda-vars (spec/coll-of ::variable))
(spec/def ::lambda
  (spec/keys :req [::lambda-vars ::formula]))

(spec/def ::formula (spec/or :atom ::atom
                             :conjunction ::conjunction
                             :disjunction ::disjunction
                             :implication ::implication
                             :negation ::negation
                             :universal ::universal
                             :existential ::existential))


(declare formula?)

(defmacro formula-check?
  "A macro to make writing formula predicates
  simpler"
  [object formula-type]
  `(and
    (seq ~object)
    (= (first ~object) ~formula-type)
    (->> ~object
         rest
         (every? formula?))))

;;;;;;;
;; Atom

(defn atom? [object]
  (string? object))

;;;;;;;;;;;
;; Negation

(defn make-negation
  "Create a negation of a
   formula"
  [formula]
  [:not formula])

(defn negation?
  "Check whether an object
   is a negation"
  [object]
  (formula-check? object :not))

(defn negatum
  "Get the negatum
  from a negated formula"
  [formula]
  (when (negation? formula)
    (first formula)))

;;;;;;;;;;;;;;
;; Conjunction

(defn make-conjunction
  "Create a conjunction
  from formulas"
  [& formulas]
  `[:and ~@formulas])

(defn conjunction?
  "Check whether or not an
  object is a conjunction."
  [object]
  (formula-check? object :and))

(defn conjuncts
  "Get the conjuncts
  from a conjunction"
  [formula]
  (when (conjunction? formula)
    (rest formula)))

;;;;;;;;;;;;;;
;; Disjunction

(defn make-disjunction
  "Create a disjunction from
  formulas"
  [& formulas]
  `[:or ~@formulas])

(defn disjunction?
  "Check whether a formula
  is a disjunction"
  [object]
  (formula-check? object :or))

(defn disjuncts
  "Get the disjuncts from a
  disjunction"
  [formula]
  (when (disjunction? formula)
    (rest formula)))

;;;;;;;;;;;;;;
;; Implication

(defn make-implication
  "Make an implication
  from two formulas"
  [antecedent consequent]
  [:implies antecedent consequent])

(defn implication?
  "Check whether an object
  is an implication"
  [object]
  (formula-check? object :implies))

(defn antecedent
  "Get the antecedent of a formula"
  [formula]
  (when (implication? formula)
    (nth formula 1)))

 (defn consequent
  "Get the antecedent of a formula"
  [formula]
  (when (implication? formula)
    (nth formula 2)))

;;;;;;;;;;;
;; Formulas

(defn formula?
  [object]
  (or
   (atom? object)
   (negation? object)
   (conjunction? object)
   (disjunction? object)
   (implication? object)))


;;;;;;;;;;;;;
;;; Serialize

(defn to-string [formula]
  (cond
    (atom? formula)
    formula
    (negation? formula)
    (format "(not %s)"
            (->> formula
                 negatum 
                 to-string))
    (conjunction? formula)
    (str "(and "
         (clojure.string/join
          (->> formula
               conjuncts
               (map to-string))
          " ")
         ")")
    
    (disjunction? formula)
    (str "(or "
         (clojure.string/join
          (->> formula
               disjuncts
               (map to-string))
          " ")
         ")")
    (implication? formula)
    (format "(implies %s %s)"
            (->> formula
                 antecedent
                 to-string)
            (->> formula
                 consequent
                 to-string))))
  
