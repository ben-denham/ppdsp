(ns ppdsp.masking.projection
  (:require [ppdsp.masking.base :refer [masked-schema-features
                                       mask-record-features
                                       decorate-mask-factory]]
            [ppdsp.utils.random :refer [seeded-rng next-gauss!
                                       next-derived-seed!]]
            [clojure.core.matrix :as m]
            [clojure.core.matrix.linear :as ml]
            [clojure.math.numeric-tower :refer [sqrt]])
  (:import [ppdsp.masking.base DatasetFeatureMask]))

(defn make-drifting-projection-mask-factory
  "Creates a dataset-feature-mask that right-multiplies dataset features
  by the given init-matrix, and incrementally updates init-matrix with
  drift-fn! after masking each record."
  [init-matrix drift-fn!]
  (fn [schema-features]
    (let [[matrix-rows matrix-cols] (m/shape init-matrix)
          matrix-atom (atom init-matrix)]
      (when (not-every? #(= :numeric (:options %)) schema-features)
        (throw (IllegalArgumentException. "Some dataset features are not numeric")))
      (when (not= matrix-rows (count schema-features))
        (throw (IllegalArgumentException. "Number of matrix rows and dataset features differ")))
      (reify DatasetFeatureMask
        (masked-schema-features [this]
          (map #(hash-map :name (str "dim" %)
                          :options :numeric)
               (range matrix-cols)))
        (mask-record-features [this features]
          (let [current-matrix @matrix-atom]
            (swap! matrix-atom drift-fn!)
            (when (not= (m/shape current-matrix) (m/shape @matrix-atom))
              (throw (IllegalStateException. "Drifted matrix does not have the same dimensionality as the previous matrix")))
            (m/mmul features current-matrix)))))))

(defn make-static-projection-mask-factory
  "Creates a dataset-feature-mask that right-multiplies dataset features
  by the given matrix."
  [matrix]
  (make-drifting-projection-mask-factory matrix identity))

(defn random-gauss-matrix
  "Generates a vector of vectors where each value is drawn from a
  Gaussian distribution with mean: mu and standard deviation: sigma."
  [[rows cols] mu sigma
   & {:keys [seed]}]
  (let [seed (or seed 1)
        rng (seeded-rng seed)]
    (vec
     (for [_ (range rows)]
       (vec
        (for [_ (range cols)]
          (next-gauss! rng mu sigma)))))))

(defn make-random-projection-mask-factory
  "Creates a dataset-feature-mask that performs a projection with a
  random matrix with the given number of output columns, and values
  drawn from a Gaussian with mean 0 and standard deviation: sigma."
  [output-cols sigma & {:keys [seed drift-fn!]
                        :or {drift-fn! identity}}]
  (fn [schema-features]
    (let [input-cols (count schema-features)
          matrix (random-gauss-matrix [input-cols output-cols] 0 sigma :seed seed)
          drifting-mask-factory (make-drifting-projection-mask-factory matrix drift-fn!)
          normalisation-factor (/ 1 (* (Math/sqrt output-cols) sigma))
          normalised-mask-factory (decorate-mask-factory drifting-mask-factory
                                                         identity
                                                         #(m/mul normalisation-factor %))]
      (normalised-mask-factory schema-features))))

(defn random-orthogonal-matrix
  "Produces a random orthogonal matrix by generating a
  random-gauss-matrix with mean zero and unit variance, and then
  producing an orthogonal 'Q' matrix through QR decomposition.

  This approach was described in: Meng, D., Sivakumar, K., & Kargupta,
  H. (2004, November). Privacy-sensitive Bayesian network parameter
  learning. In Data Mining, 2004. ICDM'04. Fourth IEEE International
  Conference on (pp. 487-490). IEEE.

  NOTE: Because the random matrix generally has full column rank (all
  columns are linearly independent), the matrix is also generally
  orthonormal. See:
  https://en.wikipedia.org/wiki/QR_decomposition#Square_matrix"
  [size & {:keys [seed]}]
  (:Q (ml/qr (random-gauss-matrix [size size] 0 1 :seed seed))))

(defn make-random-orthogonal-mask-factory
  "Creates a dataset-feature-mask that performs a transformation with a
  random orthogonal matrix."
  [& {:keys [seed drift-fn!]
      :or {drift-fn! identity}}]
  (fn [schema-features]
    (let [matrix (random-orthogonal-matrix (count schema-features) :seed seed)
          drifting-mask-factory (make-drifting-projection-mask-factory matrix drift-fn!)]
      (drifting-mask-factory schema-features))))

#_(defn make-translating-rotating-mask-with-additive-drift-factory
  [additive-sigma & {:keys [seed drift-fn!]}]
  (fn [schema-features]
    (let [seed (or seed 1)
          rng (seeded-rng seed)
          ;;translation-vector (random-gauss-matrix [1 (count schema-features)]
          ;;                                        0 1 :seed (next-derived-seed! rng))
          additive-vector-atom (atom (m/zero-vector (count schema-features)))
          rotation-mask-factory (make-random-orthogonal-mask-factory seed drift-fn!)
          mask-factory (decorate-mask-factory rotation-mask-factory
                                              identity
                                              (fn [features]
                                                (swap! additive-vector-atom
                                                         #(m/add % (random-gauss-matrix
                                                                    [1 (count schema-features)]
                                                                    0 additive-sigma
                                                                    :seed (next-derived-seed! rng))))
                                                (vec (first (m/rows (m/add features
                                                                           ;;translation-vector
                                                                           @additive-vector-atom))))))]
      (mask-factory schema-features))))

(defn make-random-projection-with-noise-and-translation-mask-factory
  "A mask factory that combines a random projection (to
  projection-features number of features and with projection-sigma
  used when generating gaussian projection matrix) with independent
  Gaussian noise (with independent-sigmas as the sigma for each
  attribute) added to each record, and cumulative Gaussian noise (with
  cumulative-sigmas as the sigma for each attribute) added to each
  record. The independent or cumulative noise can be excluded by
  setting the corresponding sigmas to 0.

  If a noise-log-atom is provided, it will be populated with a list of
  the cumulative-noise values that are added for each record."
  [projection-features projection-sigma independent-sigma cumulative-sigma translation
   & {:keys [seed noise-log-atom]}]
  (when noise-log-atom
    (reset! noise-log-atom []))
  (fn [schema-features]
    (let [seed (or seed 1)
          rng (seeded-rng seed)
          translation (or translation 0)
          translation-vector (repeat (count schema-features) translation)
          cumulative-noise-vector-atom (atom (m/zero-vector (count schema-features)))
          projection-mask-factory (make-random-projection-mask-factory
                                   projection-features projection-sigma
                                   :seed (next-derived-seed! rng))
          mask-factory (decorate-mask-factory
                        projection-mask-factory
                        identity
                        (fn [features]
                          (let [independent-noise
                                (repeatedly projection-features
                                            #(next-gauss! rng 0 independent-sigma))
                                cumulative-noise
                                (repeatedly projection-features
                                            #(next-gauss! rng 0 cumulative-sigma))]
                            ;; Add the new cumulative noise to the
                            ;; cumulative total.
                            (swap! cumulative-noise-vector-atom
                                   #(map + % cumulative-noise))
                            (when noise-log-atom
                              (swap! noise-log-atom conj cumulative-noise))
                            ;; Add the independent and cumulative noise
                            ;; to the features.
                            (vec (map +
                                      features
                                      translation-vector
                                      independent-noise
                                      @cumulative-noise-vector-atom)))))]
      (mask-factory schema-features))))
