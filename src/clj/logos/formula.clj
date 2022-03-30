(ns logos.formula
  (:require
            [clojure.set :as s]
            [clojure.string :as string]))

(defmacro defs [& bindings]
  (loop [name  (first bindings)
         value (second bindings)
         todo  (rest (rest bindings))
         result '()]
    (if (seq todo)
      (recur (first todo)
             (second todo)
             (rest (rest todo))
             (clojure.core/conj result `(def ~name ~value)))
      (-> result
          (clojure.core/conj `(def ~name ~value))
          reverse
          (clojure.core/conj 'do)))))


(declare formula?)

(defmacro formula-check?
  "A macro to make writing formula predicates
  simpler"
  [object formula-type]
  `(if-not (coll? ~object)
     false
     (and
      (seq ~object)
      (= (first ~object) ~formula-type)
      (->> ~object
           rest
           (every? formula?)))))

;;;;;;;;;;;;;;;;;;;;
;;; atomic predicate

(defn atomic-predicate?
  [object]
  (and
   (string? object)
   (string/starts-with? object "!")))

;;;;;;;;;;;;
;;; constant


(defn constant?
  "Predicate determining whether
  an `object` is a `constant`"
  [object]
  (and
   (string? object)
   (not (atomic-predicate? object))))

(def next-char
  (zipmap
   (map char (range 97 123))
   (map char (range 98 124))))

(defn next-constant
  "Get the next lexicographical
  string given a string"
  [constant]
  (let [start     (clojure.string/join
                   "" (butlast constant))
        last      (first (take-last 1 constant))
        next-char (next-char last)]
    (if (= next-char \{)
      (if (= (first (take-last 1 start)) \z)
        (str (next-constant (clojure.string/join
                             "" (butlast start))) "a" \a)
        (str start "a" \a))
      (str start next-char))))

;;;;;;;;;;;;
;;; variable

(defn variable?
  "Predicate determining whether
  an `object` is a `variable`"
  [object]
  (and
   (symbol? object)
   (= (subs (name object) 0 1) "?")))

;;;;;;;;
;;; term

(defn term?
  "Predicate determining whether
  an `object` is a `term`"
  [object]
  (if-not (nil? object)
    (or
     (constant? object)
     (variable? object))
    false))


;;;;;;;
;; Atom

(declare predicate?)

(defn atom? [object]
  (or (term? object)
      (= object ::bottom)
      (and
       (vector? object)
       (predicate? (first object))
       (every? (fn [object]
                 (or (term? object)
                     (formula? object)))
               (rest object)))))

(defn atom [predicate & args]
  (apply vector predicate args))

(defn predicate [atom]
  (if-not (atom? atom)
    (throw
     (ex-info "Cannot get predicate from non-atom"
              {:caused-by `(not (atom? ~atom))}))
    (first atom)))

(defn terms [atom]
  (if-not (atom? atom)
    (throw
     (ex-info "Cannot get predicate from non-atom"
              {:caused-by `(not (atom? ~atom))}))
    (rest atom)))

;;;;;;;;;;;
;; Negation

(defn neg
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
    (second formula)))

;;;;;;;;;;;;;;
;; Conjunction

(defn conj
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

(defn disj
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

(defn implies
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

;;;;;;;;;;;;;
;;; variables

(declare universal?)
(declare existential?)
(declare lambda?)

(defn quantified-subformula
  [object]
  (if-not (or
           (universal? object)
           (lambda? object)
           (existential? object))
    (throw
     (ex-info
      (str
       "Cannot get quantified subformula of "
       "non-quantier formula.")
       {:caused-by `(not (or
                          (universal? ~object)
                          (lambda? ~object)
                          (existential? ~object)))}))
    (nth object 2)))

(defn bound-variables
  [object]
  (if-not (or
           (universal? object)
           (lambda? object)
           (existential? object))
    (throw
     (ex-info
      (str
       "Cannot get bound variables of "
       "non-quantier formula.")
       {:caused-by `(not (or
                          (universal? ~object)
                          (lambda? ~object)
                          (existential? ~object)))}))
    (vec (set (second object)))))

(defn free-variables-internal
  [formula acc bound-vars]
  (cond (atom? formula)
        (->> formula
             terms
             (filter
              (fn [term]
                (and
                 (variable? term)
                 (not (some #{term} (set bound-vars))))))
             set
             (s/union acc))
        (negation? formula)
        (free-variables-internal
         (negatum formula) acc bound-vars)
        (conjunction? formula)
        (->> formula
             conjuncts
             (map
              #(free-variables-internal % acc bound-vars))
             (apply s/union))
        (disjunction? formula)
        (->> formula
             disjuncts
             (map
              #(free-variables-internal % acc bound-vars))
             (apply s/union))
        (implication? formula)
        (s/union
         (free-variables-internal
          (antecedent formula) acc bound-vars)
         (free-variables-internal
          (consequent formula) acc bound-vars))
        (or
         (universal? formula)
         (existential? formula))
        (free-variables-internal
         (quantified-subformula formula)
         acc (s/union
              (bound-variables formula)
              bound-vars))))

(defn free-variables
  [formula]
  (free-variables-internal formula (set []) (set [])))


(defn formula-gather
  [formula predicate-fn]
  (loop [current-item formula
         todos        []
         result       []]
    (let [new-result (if (predicate-fn current-item)
                       (clojure.core/conj result current-item)
                       result)
          new-todos  (cond
                       (or (term? current-item)
                           (= current-item ::bottom))
                       todos
                       (atom? current-item)
                           (concat (clojure.core/conj todos (predicate current-item))
                                   (terms current-item))
                           (or (lambda? current-item)
                               (existential? current-item)
                               (universal? current-item))
                           (concat
                            (clojure.core/conj todos
                                  (quantified-subformula current-item))
                            (bound-variables current-item))
                           (negation? current-item)
                           (clojure.core/conj todos (negatum current-item))
                           (conjunction? current-item)
                           (->> current-item
                                conjuncts
                                (apply concat todos))
                           (disjunction? current-item)
                           (->> current-item
                                disjuncts
                                (apply concat todos))
                           (implication? current-item)
                           (-> todos
                               (clojure.core/conj (antecedent current-item))
                               (clojure.core/conj (consequent current-item)))
                           :else
                           todos)]
      (if (seq new-todos)
        (recur (first new-todos)
               (rest new-todos)
               new-result)
        new-result))))









;;;;;;;;;;;;;
;;; universal

(defn universal? [object]
  (and
   (vector? object)
   (= (first object) :forall)
   (vector? (second object))
   (every? variable? (second object))
   (formula? (nth object 2))))

(defn forall [variables formula]
  [:forall variables formula])

;;;;;;;;;;;;;;;
;;; existential

(defn existential? [object]
  (and
   (vector? object)
   (= (first object) :exists)
   (vector? (second object))
   (every? variable? (second object))
   (formula? (nth object 2))))

(defn exists [variables formula]
  [:exists variables formula])

;;;;;;;;;;;;
;; predicate

(defn lambda?
  [object]
  (and
   (vector? object)
   (= (first object) :lambda)
   (vector? (second object))
   (every? variable? (second object))
   (formula? (nth object 2))))

(defn predicate?
  [object]
  (or (atomic-predicate? object)
      (variable? object)
      (lambda? object)))

(defn lambda [vars formula]
  (if-not (= (free-variables formula)
             (set vars))
    (throw
     (ex-info (str
               "Cannot create a lambda with "
               "unbound formulas.")
              {:caused-by `(not
                            (=
                             ~(free-variables formula)
                             ~(set vars)))}))
    [:lambda vars formula]))


;;;;;;;;;;;
;; formula

(defn formula?
  [object]
  (or
   (atom? object)
   (negation? object)
   (conjunction? object)
   (disjunction? object)
   (implication? object)
   (universal? object)
   (existential? object)))

(defn constants [formula]
  (cond (or
         (existential? formula)
         (universal? formula))
        (constants (quantified-subformula formula))
        (conjunction? formula)
        (mapcat constants (conjuncts formula))
        (disjunction? formula)
        (mapcat constants (disjuncts formula))
        (implication? formula)
        (concat
         (constants (antecedent formula))
         (constants (consequent formula)))
        (negation? formula)
        (constants (negatum formula))
        (atom? formula)
        (filter constant? (terms formula))))

(defn get-unique-constant
  [seed-constant used-constants]
  (loop [last-constant seed-constant]
    (let [new-constant (next-constant last-constant)
          new-pred     (format "!%s" new-constant)]
      (if (or (some #{new-constant} used-constants)
              (some #{new-pred} used-constants))
        (recur new-constant)
        new-constant))))

(defn generate-new-constant-map
  [old-constants vars]
    (loop [last-constant "a"
           current-var   (first vars)
           todo-vars     (rest vars)
           result       {}]
      (let [match (get-unique-constant
                   last-constant old-constants)]
        (if (seq todo-vars)
          (recur match
                 (first todo-vars)
                 (rest todo-vars)
                 (assoc result current-var match))
          (assoc result current-var match)))))

(defn substitute-free-variables
  [formula constant-map]
  (cond (atom? formula)
        ;; This feels like an abstraction
        ;; violation
        (if (term? formula)
          (if-let [new (constant-map formula)] new formula)
          (let [pred (predicate formula)
                new-terms (mapv (fn [term]
                                  (if-let [new (constant-map term)]
                                    new
                                    term))
                               (terms formula))]
            (if-let [new-pred (constant-map pred)]
              (apply atom (format "!%s" new-pred) new-terms)
              (apply atom pred new-terms))))
        (negation? formula)
        (-> formula
            negatum
            (substitute-free-variables constant-map)
            neg)
        (conjunction? formula)
        (->> formula
             conjuncts
             (map (fn [conjunct]
                    (substitute-free-variables
                     conjunct constant-map)))
             (apply logos.formula/conj))
        (disjunction? formula)
        (->> formula
             disjuncts
             (map (fn [disjunct]
                    (substitute-free-variables
                     disjunct constant-map)))
             (apply logos.formula/disj))
        (implication? formula)
        (implies
         (substitute-free-variables
          (antecedent formula) constant-map)
         (substitute-free-variables
          (consequent formula) constant-map))
        (or (universal? formula)
            (existential? formula))
        (let [new-bounds (bound-variables formula)
              subformula (quantified-subformula formula)
              new-map    (apply
                          dissoc constant-map new-bounds)
              new-subformula (substitute-free-variables
                              subformula new-map)]
          (if (universal? formula)
            (forall new-bounds new-subformula)
            (exists new-bounds new-subformula)))))

(defn instantiate-new-variables
  [formula used-constants]
  (when (not
         (or
          (universal? formula)
          (existential? formula)))
    (throw
     (ex-info (str "Cannot instantiate variables "
                   "on non-universal or non-existential "
                   "formula")
              {:caused-by formula})))
  (let [vars          (bound-variables formula)
        old-constants (concat used-constants
                              (formula-gather
                               formula
                               (fn [obj]
                                 (or (constant? obj)
                                     (atomic-predicate? obj)))))
        new-constant-map (generate-new-constant-map
                          old-constants vars)]
    (substitute-free-variables (quantified-subformula
                                formula)
                               new-constant-map)))

;;;;;;;;;;;;;
;;; Serialize
;;TODO These should be multimethods
;; or formula should be a protocol

(declare to-string)

(defn serialize-predicate
  [predicate ]
  (cond (or (atomic-predicate? predicate)
            (variable? predicate))
        (str predicate)
        (lambda? predicate)
        (let [vars (bound-variables predicate)
              formula (quantified-subformula predicate)]
          (str
           "(lambda ("
           (->> vars
                (map name)
                (clojure.string/join " "))
           ")"
           (to-string formula)
           ")"))
        :else
        (throw
         (ex-info "Trying to serialize non-predicate."
                  {:caused-by predicate}))))

(defn to-string [formula]
  (cond
    (atom? formula)
    (cond (term? formula)
          (str formula)
          (= formula ::bottom)
          (str (name formula))
          :else
          (let [pred (->> formula
                          first
                          serialize-predicate)]
            (if (seq (terms formula))
              (str "(" pred " "
                   (->> formula
                        rest
                        (clojure.string/join " "))
                   ")")
              (str pred))))
    (negation? formula)
    (format "(not %s)"
            (->> formula
                 negatum
                 to-string))
    (conjunction? formula)
    (str "(and "
         (clojure.string/join " "
                              (->> formula
                                   conjuncts
                                   (map to-string)))
         ")")
    (disjunction? formula)
    (str "(or "
         (clojure.string/join " "
                              (->> formula
                                   disjuncts
                                   (map to-string)))
         ")")
    (implication? formula)
    (format "(implies %s %s)"
            (->> formula
                 antecedent
                 to-string)
            (->> formula
                 consequent
                 to-string))
    (universal? formula)
    (format "(forall %s %s)"
            (->> formula
                 bound-variables
                 (map name)
                 (clojure.string/join " "))
            (->> formula
                 quantified-subformula
                 to-string))
    (existential? formula)
    (format "(exists %s %s)"
            (->> formula
                 bound-variables
                 (map name)
                 (clojure.string/join " "))
            (->> formula
                 quantified-subformula
                 to-string))))


;;;;;;;;;;;;;;
;;; To Formula

(declare read-formula-sexp)

(defn read-formula-error [s-expression]
  (ex-info "Cannot parse to formula"
           {:caused-by s-expression}))

(defn read-quantified-formula
  [operator args]
    (let [vars        (into [] (butlast args))
          subformula  (read-formula-sexp (last args))]
      (if (every? variable? vars)
        (cond (= operator 'forall)
              (forall vars subformula)
              (= operator 'exists)
              (exists vars subformula)
              (= operator 'lambda)
              (lambda vars subformula))
        (read-formula-error (list operator args)))))

(defn read-formula-sexp
  [s-expression]
  (cond (coll? s-expression)
        (let [raw-operator (first s-expression)
              operator     (->> raw-operator
                            name
                            string/lower-case
                            symbol)
              args     (rest  s-expression)]
          (cond
            (= operator 'not)
            (if (= (count args) 1)
              (neg (-> args first read-formula-sexp))
              (throw (read-formula-error s-expression)))
            (= operator 'and)
            (apply logos.formula/conj (map read-formula-sexp args))
            (= operator 'or)
            (apply logos.formula/disj  (map read-formula-sexp args))

            (= operator 'implies)
            (if (= (count args) 2)
              (let [antecedent (first args)
                    consequent (second args)]
                (implies (read-formula-sexp antecedent)
                         (read-formula-sexp consequent)))
              (throw (read-formula-error s-expression)))
            (or (= operator 'forall)
                (= operator 'exists)
                (= operator 'lambda))
            (read-quantified-formula operator args)
            (predicate? raw-operator)
            (apply
             logos.formula/atom raw-operator
             (map read-formula-sexp args))
            (predicate? (str raw-operator))
            (apply
             logos.formula/atom (str raw-operator)
             (map read-formula-sexp args))))
        (variable? s-expression)
        s-expression
        :else
        (str s-expression)))

(defn read-formula
  "Convert a string to a formula
  object"
  [string]
  (let [to-parse (read-string string)]
    (if (formula? to-parse)
      to-parse
      (read-formula-sexp to-parse))))
