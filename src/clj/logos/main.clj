(ns logos.main
  (:require [logos.formula :as f]
            [logos.proof :as p :refer [conditional-proof
                                       universal-proof
                                       direct-proof
                                       conjunctive-proof]]))
(def current-proof (atom nil))

(defn start-proof
  "Create a new proof for `goal` with `premises`"
  [goal & premises]
  (let [all-forms (conj premises goal)]
    (when (not (every? f/formula? all-forms))
      (let [failure (->> all-forms
                         (filter (comp not f/formula?))
                         first)]
        (throw
         (ex-info (format "%s is not a formula" failure)
                  {:caused-by `(not
                                (f/formula? ~failure))}))))
    (let [proof (p/new-proof premises goal [])]
      (reset! current-proof proof)
      proof)))

(defn one-step [step-fn]
  (let [result (step-fn @current-proof)]
     (reset! current-proof result)
     (-> result
         (p/show-proof 0)
         println)))
