(ns ppdsp.masking.sensitive-drift
  "Perturbation-based masking method based on Solanki et al. 2018."
  (:require [clojure.core.matrix :as m]
            [clojure.core.matrix.stats :as ms]
            [ppdsp.dataset.base :refer [get-schema dataset->classes
                                        dataset->matrix matrix->dataset]]
            [ppdsp.utils :refer [mean standard-deviation safe-division
                                 remove-nth]]
            [ppdsp.utils.matrices :refer [join-row-wise
                                          join-col-wise
                                          submatrix-start-end
                                          matrix-batches
                                          drop-column]]))

(defn sliding-z-score-normalise-seq
  [xs sliding-window-size]
  (loop [xs xs
         window []
         result []]
    (if-let [x (first xs)]
      (let [new-window (take sliding-window-size (conj window x))
            ;; If std-dev is 0, then all values must equal x, so we
            ;; can fallback to 0.
            stddev (standard-deviation new-window)
            normal-x (if (= (float stddev) 0.0)
                       0
                       (/ (- x (mean new-window)) stddev))]
        (recur (rest xs)
               new-window
               (conj result normal-x)))
      result)))

(defn sliding-z-score-normalise-matrix
  [matrix sliding-window-size]
  (->> (m/columns matrix)
       (map #(sliding-z-score-normalise-seq % sliding-window-size))
       ;; Convert list of columns back into a matrix.
       (m/transpose)))

(defn aggregate-sensitive
  [normal-nonsensitive-matrix sensitive-vector sd-value]
  (let [row-means (map ms/mean (m/rows normal-nonsensitive-matrix))]
    (for [target-row-mean row-means]
      (let [sp-value (* target-row-mean sd-value)
            lower-bound (- target-row-mean sp-value)
            upper-bound (+ target-row-mean sp-value)
            aggregate-indexes-set
            (set (keep-indexed
                  (fn [idx row-mean]
                    ;; Return the index of the row if it is within the
                    ;; bounds, otherwise nil
                    (if (and (>= row-mean lower-bound)
                             (<= row-mean upper-bound))
                      idx))
                  row-means))]
        (->> sensitive-vector
             (keep-indexed (fn [idx value] (if (aggregate-indexes-set idx) value)))
             mean)))))

(defn sd-mask
  [dataset & {:keys [sliding-window-size aggregation-window-size sd-value]}]
  (let [input-matrix (dataset->matrix dataset)
        [matrix-rows matrix-cols] (m/shape input-matrix)
        normal-matrix (sliding-z-score-normalise-matrix input-matrix sliding-window-size)
        output-cols
        (for [col-idx (range matrix-cols)]
          (let [normal-nonsensitive (drop-column normal-matrix col-idx)
                input-sensitive (m/column-matrix (m/get-column input-matrix col-idx))]
            (apply concat
             (for [[nonsensitive-batch sensitive-batch]
                   (map vector
                        (matrix-batches normal-nonsensitive aggregation-window-size)
                        (matrix-batches input-sensitive aggregation-window-size))]
               (aggregate-sensitive nonsensitive-batch
                                    (m/get-column sensitive-batch 0)
                                    sd-value)))))
        output-matrix (m/transpose output-cols)]
    (matrix->dataset (get-schema dataset) output-matrix (dataset->classes dataset))))

(defn noop-attack
  "A naive attack that assumes the best estimation of the original
  record is the masked record."
  [io-attack-data rng & {:keys []}]
  {:optimum (-> io-attack-data :unknown :masked)
   :score 1
   :evaluations nil
   :best-noise-attack nil})

(def sd-attack noop-attack)
