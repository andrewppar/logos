(ns logos.main
  (:require [logos.formula :as f]
            [logos.proof.proof :as proof]
            [logos.proof.goal :as goal]
            [logos.proof.premise :as premise]
            [logos.proof.show :refer [show-proof]]))

(def current-proof (atom nil))

(defn start-proof
  [goal & {:keys [premises]}]
  (let [new-proof (if (nil? premises)
                    (proof/new-proof premises goal)
                    (proof/new-proof goal))]
    (reset! current-proof new-proof)
    (println (show-proof new-proof))
    new-proof))

(defn one-step
  [proof function & {:keys [premise-idxs]}]
  (let [new-proof (if (nil? premise-idxs)
                    (function proof)
                    (function proof premise-idxs))]
    (println (show-proof new-proof))
    new-proof))
