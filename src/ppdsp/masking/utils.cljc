(ns ppdsp.masking.utils
  (:require [ppdsp.masking.attack-data
             :refer [split-knowns-at-unknown
                     attack-input-feature-count]]
            [ppdsp.utils :refer [mean]]
            [ppdsp.utils.matrices
             :refer [join-row-wise]]
            [clojure.core.matrix :as m]))

(defn combine-logps
  "Given a set of logp values, combine them into a joint probability."
  [logps]
  (->> logps
       ;; Never let a log-probability-density exceed a fixed
       ;; threshold. Also catch NaN, because the
       ;; (multivariate) gaussian distributions can sometimes
       ;; return that in place of a very high value.
       (map #(if (Double/isNaN %) 1000000 %))
       (map #(min % 1000000))
       ;; Because logp is used, probability densities should be
       ;; added instead of multiplied. Taking a mean is the
       ;; equivalent to taking a sum and dividing by a constant,
       ;; so has no impact on the relative sizes of objective
       ;; results. However, it does mean that scores are always
       ;; on the same relative scale regardless of how many
       ;; probabilities they involve.
       (mean)))

(defn build-attack-col-matrices
  "Build matrices with both known and unknown records as columns for
  input and masked data. Given the input unknown record is not
  available, it is set to zeroes to be replaced later. The index of
  the unknown column in both matrices is also given."
  [io-attack-data]
  (let [{knowns-before :before knowns-after :after} (split-knowns-at-unknown io-attack-data)
        known-before-input-rows (m/matrix (map :input knowns-before))
        known-after-input-rows (m/matrix (map :input knowns-after))
        known-before-masked-rows (m/matrix (map :masked knowns-before))
        known-after-masked-rows (m/matrix (map :masked knowns-after))
        input-feature-count (attack-input-feature-count io-attack-data)]
    {:input-cols (->> (join-row-wise known-before-input-rows
                                     (m/row-matrix (repeat input-feature-count 0))
                                     known-after-input-rows)
                      (m/transpose))
     :masked-cols (->> (join-row-wise known-before-masked-rows
                                      (m/row-matrix (:masked (:unknown io-attack-data)))
                                      known-after-masked-rows)
                       (m/transpose))
     :unknown-col-index (m/row-count known-before-input-rows)}))
