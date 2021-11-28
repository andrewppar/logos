(ns logos.formula)

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
  
