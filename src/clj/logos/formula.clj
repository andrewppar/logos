(ns logos.formula
  (:require [clojure.set :as s]
            [clojure.string :as string]))

(declare formula?)

(defn formula-check?
  "A function to make writing formula predicates
  simpler"
  [object formula-type]
  (if-not (coll? object)
    false
    (and
     (seq object)
     (= (first object) formula-type)
     (->> object
          rest
          (every? formula?)))))

;;;;;;;;;;;;;;;;;;;;
;;; atomic predicate

(defn atomic-predicate?
  "Check whether a predicate is as basic as possible, i.e. not
   a complex predicate like a lambda function or variable."
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
  "A map assigning the int for a char to the one that follows it"
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

(defn atom?
  "Predicate determining whether a formula is atomic. Importantly,
   this function determines that as long as the formula is a predicate
   followed by a well-formed term (e.g. a function term or even another
   formula) that formula is an atom?."
  [object]
  (or (term? object)
      (= object ::bottom)
      (and
       (vector? object)
       (predicate? (first object))
       (every? (fn [object]
                 (or (term? object)
                     (formula? object)))
               (rest object)))))

(defn atom
  "Simple atom constructor"
  [predicate & args]
  (apply vector predicate args))

(defn predicate
  "Get the predicate from an atom."
  [atom]
  (if-not (atom? atom)
    (throw
     (ex-info "Cannot get predicate from non-atom"
              {:caused-by `(not (atom? ~atom))}))
    (first atom)))

(defn terms
  "Get the terms from an atom, i.e. those things to which the predicate
   applies."
  [atom]
  (if-not (atom? atom)
    (throw
     (ex-info "Cannot get predicate from non-atom"
              {:caused-by `(not (atom? ~atom))}))
    (rest atom)))

;;;;;;;;;;;;
;;; Equality

(defn equality?
  "Check whether an object is the equality predicate for the language"
  [object]
  (and (atom? object)
       (= (predicate object) "!equals")
       (= (count (terms object)) 2)))

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
  "The immediate subformula of a formula whose main operator is
   a quantifier (i.e. exists, forall, or lambda)"
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
  "Any variables in a quantified formula that are bound by the main
  quantifier (i.e. exists, forall, lambda), e.g. in (forall ?x (!isa ?x ?y))
  they are [?x]"
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

(defn ^:private free-variables-internal
  "Return all the free variables in a formula, i.e. variables not
  in the scope of some quantifier.

  Note: This function is deeply recursive because it has to look at
  subformulas keeping track of what is bound."
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
  "Get all the variables of a formula (including subformulas) that
  are not bound by some quantifier (i.e. exists, forall, lambda)."
  [formula]
  (free-variables-internal formula (set []) (set [])))

(defn formula-gather
  "Given a predicate `predicate-fn` gather any expression (or
  subexpression) of `formula` that satisfies it. For example
  `(formula-gather '[\"!isa\" ?x ?y] atomic-predicate?)` => `[\"!isa\"]`"
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
                       (concat (clojure.core/conj
                                todos (predicate current-item))
                               (terms current-item))
                       (or (lambda? current-item)
                           (existential? current-item)
                           (universal? current-item))
                       (concat
                        (clojure.core/conj
                         todos (quantified-subformula current-item))
                        (bound-variables current-item))
                       (negation? current-item)
                       (clojure.core/conj todos (negatum current-item))
                       (conjunction? current-item)
                       (->> current-item
                            conjuncts
                            (concat todos))
                       (disjunction? current-item)
                       (->> current-item
                            disjuncts
                            (concat todos))
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

(defn universal?
  "Predicate to determine whether an object is a universally quantified
   formula, i.e. it is a formula prepended with `:forall` and sequence of
   variables."
  [object]
  (and
   (vector? object)
   (= (first object) :forall)
   (vector? (second object))
   (every? variable? (second object))
   (formula? (nth object 2))))

(defn forall
  "Create a universally quantified formula."
  [variables formula]
  [:forall variables formula])

;;;;;;;;;;;;;;;
;;; existential

(defn existential?
  "Predicate to determine whether an object is an existentially quantified
   formula, i.e. it is a formula prepended with `:exists` and sequence of
   variables."
  [object]
  (and
   (vector? object)
   (= (first object) :exists)
   (vector? (second object))
   (every? variable? (second object))
   (formula? (nth object 2))))

(defn exists
  "Create an existentially quantified formula."
  [variables formula]
  [:exists variables formula])

;;;;;;;;;;;;
;; predicate

(defn lambda?
  "Predicate to determine whether an object is a lambda expression,
  i.e. if it is a term that can be used for lambda conversion."
  [object]
  (and
   (vector? object)
   (= (first object) :lambda)
   (vector? (second object))
   (every? variable? (second object))
   (formula? (nth object 2))))

(defn predicate?
  "Predicate to determine whether or not something is a predicate of
  the language. This includes atomic-predicates, variables, and lambda
  expressions."
  [object]
  (or (atomic-predicate? object)
      (variable? object)
      (lambda? object)))

(defn lambda
  "Create a lambda expression from variables and formula."
  [vars formula]
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
  "Predicate to determine whether an object is a valid formula of the
  language."
  [object]
  (or
   (atom? object)
   (negation? object)
   (conjunction? object)
   (disjunction? object)
   (implication? object)
   (universal? object)
   (existential? object)))

(defn constants
  "Function to get all the constans in a formula."
  [formula]
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

(defn ^:private get-unique-constant
  "Generate a new unique constant given a `seed-constant` and other
  already `used-constants`."
  [seed-constant used-constants]
  (loop [last-constant seed-constant]
    (let [new-constant (next-constant last-constant)
          new-pred     (format "!%s" new-constant)]
      (if (or (some #{new-constant} used-constants)
              (some #{new-pred} used-constants))
        (recur new-constant)
        new-constant))))

(defn ^:private generate-new-constant-map
  "Given already used constants and variables that need to be assigned
   to new constants, generate a map from those variables to the new
   constants."
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
  "Given a formula and map from variables to constants, generate the
  formula that results from replacing all free occurrences of variables
  in `constant-map` by their mapped values."
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
                                    (if (coll? term)
                                      (substitute-free-variables term constant-map)
                                      term)
                                    ))
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
  "Given a quantified formula and a list of used constants, create a
  completely new formula by instantiating all variables bound by the
  main quantifier by a new constant."
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

(declare to-string)

(defn serialize-predicate
  "Create a string representation of a predicate"
  [predicate]
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

(defn to-string
  "Create a string representation of a formula."
  [formula]
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
                        (map to-string)
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

(defn ^:private read-formula-error
  "Error to throw if parsing a formula from a string fails."
  [s-expression]
  (ex-info "Cannot parse to formula"
           {:caused-by s-expression}))

(defn ^:private read-quantified-formula
  "Function for parsing a quantified formula an operator and args"
  [operator args]
  (let [raw-vars    (butlast args)
        vars        (if (and (= (count raw-vars) 1)
                             (coll? (first raw-vars)))
                      (first raw-vars)
                      (into [] (butlast args)))
        subformula  (read-formula-sexp (last args))]
    (if (every? variable? vars)
      (cond (= operator 'forall)
            (forall vars subformula)
            (= operator 'exists)
            (exists vars subformula)
            (= operator 'lambda)
            (lambda vars subformula))
      (read-formula-error (list operator args)))))

(defn ^:private read-formula-sexp
  "Read an s-expression representing a formula as a formula."
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

(defn ^:private open-paren?
  "Determine if the `char` is an open brace of some sort"
  [char]
  (or (= char \()
      (= char \[)))

(defn close-paren?
  "Determine if the `char` is a closed brace of some sort"
  [char]
  (or (= char \))
      (= char \])))

(defn get-matching-paren-idx
  "Given a string containing braces and an index - get the index of
  the matching brace."
  [string idx]
  (let [substring (subs string idx)]
    (when (open-paren? (first substring))
      (loop [current-idx  (inc idx)
             depth        0
             todo         (rest (rest substring))
             current-char (first (rest substring))]
        (cond
          (not (seq todo))
          (when (and (= depth 0)
                     (close-paren? current-char))
            (inc current-idx))
          (open-paren? current-char)
          (recur (inc current-idx) (inc depth) (rest todo) (first todo))
          (and (= depth 0)
               (close-paren? current-char))
          (inc current-idx)
          (close-paren? current-char)
          (recur (inc current-idx) (dec depth) (rest todo) (first todo))
          :else
          (recur (inc current-idx) depth (rest todo) (first todo)))))))

(defn ^:private whitespace-char?
  "Determine if a character is a whitespace character"
  [ch]
  (->> [\newline \space \tab \formfeed \backspace \return]
       (some #{ch})
       boolean))

(defn ^:private chunk-string
  "Given a string representing a formula, break it into chunks
  representing s-expressions"
  [string]
  (loop [char         (first string)
         todo         (rest string)
         current-item nil
         result       []]
    (if (not (seq todo))
      (cond (whitespace-char? char)
            (clojure.core/conj result (apply str current-item))
            (nil? char)
            result
            :else
            (->> (cons char current-item)
                 reverse
                 (apply str)
                 (clojure.core/conj result)))
      (cond (whitespace-char? char)
            (if (nil? current-item)
              (recur (first todo) (rest todo) current-item result)
              (let [new-item (string/reverse (apply str current-item))
                    new-result (clojure.core/conj result new-item)]
                (recur (first todo) (rest todo) nil new-result)))
            (open-paren? char)
            (let [current-string  (apply str (cons \( todo))
                  close-paren-idx (get-matching-paren-idx
                                   current-string 0)
                  next-item       (subs
                                   current-string 0 close-paren-idx)
                  new-result      (if (nil? current-item)
                                    (clojure.core/conj result next-item)
                                    (-> result
                                        (clojure.core/conj current-item)
                                        (clojure.core/conj next-item)))
                  new-string      (subs current-string close-paren-idx)]
              (recur (first new-string) (rest new-string) nil new-result))
            :else
            (let [new-item (cons char current-item)]
              (recur (first todo) (rest todo) new-item result))))))

(defn read-formula-string
  "Recursively convert a `string` into an s-expression."
  [raw-string]
  (let [string (string/trim raw-string)]
    (if (open-paren? (first string))
      (let [matched-paren-idx (get-matching-paren-idx string 0)
            substring         (subs string 1 (dec matched-paren-idx))]
        (->> substring
             chunk-string
             (mapv read-formula-string)
             (into ())
             reverse))
      (let [first-string (first (string/split string #"\s+"))]
        (cond (string/starts-with? first-string ":")
              (keyword (subs first-string 1))
              (and (string/starts-with? first-string "\"")
                   (string/ends-with? first-string "\""))
              (subs first-string 1 (dec (count first-string)))
              :else
              (symbol first-string))))))

(defn read-formula
  "Convert a string to a formula
  object"
  [string]
  (let [to-parse (read-formula-string string)]
    (if (formula? to-parse)
      to-parse
      (read-formula-sexp to-parse))))
