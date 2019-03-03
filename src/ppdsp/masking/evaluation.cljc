(ns ppdsp.masking.evaluation
  (:require [ppdsp.masking.base :refer [mask-dataset
                                       make-dataset-mask
                                       make-distributed-dataset-mask]]
            [ppdsp.masking.projection
             :refer [make-random-projection-with-noise-and-translation-mask-factory]]
            [ppdsp.masking.attack-data :refer [build-io-attack-data]]
            [ppdsp.masking.single-stage-cumulative-attack :refer [known-io-projection-and-cumulative-noise-map-attack-single-stage]]
            [ppdsp.masking.two-stage-cumulative-attack :refer [known-io-projection-and-cumulative-noise-map-attack-two-stage]]
            [ppdsp.masking.two-stage-independent-attack :refer [known-io-projection-and-independent-noise-map-attack-two-stage]]
            [ppdsp.classifier.moa-classifier
             :refer [adaptive-random-forest perceptron]]
            [ppdsp.dataset.base :refer [get-schema dataset->matrix
                                       first-record normalise-dataset]]
            [ppdsp.training :refer [train-classifier get-accuracy]]
            [ppdsp.utils :refer [map-vals mean median std-dev pmap-pool
                                debug try-times sync-println]]
            [ppdsp.utils.random :refer [seeded-rng next-derived-seed!]]
            [ppdsp.utils.matrices :refer [get-matrix-rows random-li-row-indexes!]]
            [ppdsp.utils.timing :refer [get-current-thread-time!]]
            [clojure.string :as string]
            [clojure.core.matrix :as m]
            [clojure.core.matrix.stats :as ms]
            [clojure.math.numeric-tower :refer [sqrt]]))

;; Use vectorz Java implementation for faster matrix operations.
(m/set-current-implementation :vectorz)

;; == Accuracy Evaluation ==

(defn test-classification-accuracy
  [classifier-fn dataset]
  (let [results (train-classifier (classifier-fn (get-schema dataset)) dataset)]
    {:accuracy (get-accuracy results)
     :raw-results results}))

;; Distance measures

(defn pairwise-row-euclidean-distances
  "Given m*n matrix x, return a m*m matrix where each value v_{ij}
  returns the Euclidean distance between rows x_i and x_j. Given the
  matrix is symmetric, only the lower-triangular matrix of the result
  is returned.

  This implementation is based off that of Numpy:
  https://scikit-learn.org/stable/modules/generated/sklearn.metrics.pairwise.euclidean_distances.html"
  [x]
  (let [xy (let [x (m/matrix x)]
             (m/mmul x (m/transpose x)))
        ;; Clone is required, as we will be mutating xy
        xy-diag (m/clone (m/diagonal xy))
        row-count (m/row-count xy)
        ;; The basis for the result is xy
        res xy]
    ;; Mutate each cell in the result matrix, this uses less memory
    ;; than creating matrices to add values from xy-diag.
    (doseq [i (range row-count)
            j (range row-count)]
      (if (< j i)
        ;; Transform each cell value as needed (see Numpy
        ;; implementation).
        (m/mset! res i j
                 (sqrt
                  (+ (* -2 (m/mget res i j))
                     (m/mget xy-diag i)
                     (m/mget xy-diag j))))
        ;; If j >= i, set the value to zero, as we are constructing a
        ;; lower-triangular matrix.
        (m/mset! res i j 0)))
    ;; Return the result matrix
    res))

(defn row-distance-error
  "Computes the sum-of-squared-error (SSE) between the
  pairwise-row-euclidean-distance matrices of the dataset and
  masked-dataset."
  [dataset masked-dataset]
  (let [orig-dists (-> dataset
                       dataset->matrix
                       pairwise-row-euclidean-distances)
        masked-dists (-> masked-dataset
                         dataset->matrix
                         pairwise-row-euclidean-distances)
        ;; The basis for the result is orig-dists
        dist-error orig-dists]
    ;; Use mutation to save memory.
    (m/sub! dist-error masked-dists)
    (m/mul! dist-error dist-error)
    (m/esum dist-error)))

(defn relative-error
  "Return the relative error between the two record vectors, as defined
  in Liu (2007)."
  [true-vector recovered-vector]
  (/ (m/length (m/sub recovered-vector true-vector))
     (m/length true-vector)))

;; == Privacy Evaluation ==

(defn get-record-bounds
  "For a given number of records and a range (between 0 and 1), return
  the lower (inclusive) and upper (exclusive) bound record indexes for
  that range centered at the middle of the set of records."
  [record-count record-range range-position]
  (if (= 1 record-range)
    ;; Short-circuit for full range.
    [0 record-count]
    (case range-position
      :middle (let [onesided-bound (Math/round (* record-count (* record-range 0.5)))
                    mid-record (quot record-count 2)]
                ;; Return the start and end bounds either side of the middle.
                [(max 0 (- mid-record onesided-bound))
                 (min record-count (+ mid-record onesided-bound))])
      :start [0 (Math/round (* record-count record-range))])))

(defn get-default-known-record-counts
  "Return 1, half number of masked features (rounded down for odd
  number), and one less than masked features (max possible)."
  [masked-feature-count]
  (->> [1 (quot masked-feature-count 2) (dec masked-feature-count)]
       ;; Remove duplicates.
       (distinct)
       ;; Ensure all of the computed counts are greater than zero.
       (filter #(> % 0))))

(defn generate-io-attack-data!
  "Use the rng to generate data for a known input/output attack (in
  particular, selecting the known and unknown records)."
  [rng input-matrix masked-matrix projection-sigma
   independent-sigma cumulative-sigma known-record-count known-record-range
   known-record-range-position]
  (let [record-count (m/row-count masked-matrix)
        [known-record-start known-record-end]
        (get-record-bounds record-count known-record-range
                           known-record-range-position)
        ;; Create a derived rng so that the number of known records
        ;; does not effect subsequent rng in the experiment.
        records-rng (seeded-rng (next-derived-seed! rng))
        unknown-record-index
        (first (random-li-row-indexes! records-rng input-matrix 1))
        known-record-indexes
        (random-li-row-indexes! records-rng input-matrix
                                known-record-count
                                :start-index known-record-start
                                :end-index known-record-end
                                :init-li-row-indexes [unknown-record-index])]
    (build-io-attack-data projection-sigma independent-sigma cumulative-sigma
                          input-matrix masked-matrix
                          known-record-indexes unknown-record-index)))

(defn attack-strategy-map
  []
  {;; Single stage Cumulative Noise Attacks
   :a-rp {:attack-function known-io-projection-and-cumulative-noise-map-attack-single-stage
          :cumulative-noise? false
          :bias-input-prob? false
          :only-one-known? false}
   :a-rpcn-single-stage-unbalanced {:attack-function known-io-projection-and-cumulative-noise-map-attack-single-stage
                                    :cumulative-noise? nil ;; Chosen based on cumulative-noise-sigma.
                                    :bias-input-prob? false
                                    :only-one-known? false}
   :a-rpcn-single-stage {:attack-function known-io-projection-and-cumulative-noise-map-attack-single-stage
                         :cumulative-noise? nil ;; Chosen based on cumulative-noise-sigma.
                         :bias-input-prob? true
                         :only-one-known? false}
   :a-rpcn-1-single-stage-unbalanced {:attack-function known-io-projection-and-cumulative-noise-map-attack-single-stage
                                      :cumulative-noise? nil ;; Chosen based on cumulative-noise-sigma.
                                      :bias-input-prob? false
                                      :only-one-known? true}
   :a-rpcn-1-single-stage {:attack-function known-io-projection-and-cumulative-noise-map-attack-single-stage
                           :cumulative-noise? nil ;; Chosen based on cumulative-noise-sigma.
                           :bias-input-prob? true
                           :only-one-known? true}
   ;; Two stage Cumulative Noise Attacks
   :a-rpcn-unbalanced {:attack-function known-io-projection-and-cumulative-noise-map-attack-two-stage
                       :cumulative-noise? nil ;; Chosen based on cumulative-noise-sigma.
                       :bias-input-prob? false
                       :only-one-known? false}
   :a-rpcn {:attack-function known-io-projection-and-cumulative-noise-map-attack-two-stage
            :cumulative-noise? nil ;; Chosen based on cumulative-noise-sigma.
            :bias-input-prob? true
            :only-one-known? false}
   :a-rpcn-1-unbalanced {:attack-function known-io-projection-and-cumulative-noise-map-attack-two-stage
                         :cumulative-noise? nil ;; Chosen based on cumulative-noise-sigma.
                         :bias-input-prob? false
                         :only-one-known? true}
   :a-rpcn-1 {:attack-function known-io-projection-and-cumulative-noise-map-attack-two-stage
              :cumulative-noise? nil ;; Chosen based on cumulative-noise-sigma.
              :bias-input-prob? true
              :only-one-known? true}
   ;; Two Stage Independent Noise Attacks
   :a-rpin-unbalanced {:attack-function known-io-projection-and-independent-noise-map-attack-two-stage
                       :independent-noise? nil ;; Chosen based on cumulative-noise-sigma.
                       :bias-input-prob? false
                       :only-one-known? false}
   :a-rpin {:attack-function known-io-projection-and-independent-noise-map-attack-two-stage
            :independent-noise? nil ;; Chosen based on cumulative-noise-sigma.
            :bias-input-prob? true
            :only-one-known? false}
   :a-rpin-1-unbalanced {:attack-function known-io-projection-and-independent-noise-map-attack-two-stage
                         :independent-noise? nil ;; Chosen based on cumulative-noise-sigma.
                         :bias-input-prob? false
                         :only-one-known? true}
   :a-rpin-1 {:attack-function known-io-projection-and-independent-noise-map-attack-two-stage
              :independent-noise? nil ;; Chosen based on cumulative-noise-sigma.
              :bias-input-prob? true
              :only-one-known? true}})

(defn perform-attack
  "Perform an attack with the given io-attack-data with the given
  attack-strategies. Take the best of attempt-count attempts for each
  strategy. Return a map of strategies to the best recovery attempt
  with that strategy."
  [rng io-attack-data original-record attempt-count
   & {:keys [optimization-max-evaluations optimization-relative-threshold
             attack-strategies]}]
  (let [;; Use a derived seed to have a different seed for each
        ;; attack, but add the attempt index so that the same seeds
        ;; are used for each strategy in this attack.
        base-attack-seed (max 0 (- (next-derived-seed! rng) attempt-count))
        attack-strategies (or attack-strategies (sort (keys (attack-strategy-map))))
        ;; If no cumulative noise, do not include attack strategies
        ;; that account for cumulative noise.
        attack-strategies (if (<= (:cumulative-sigma io-attack-data) 0)
                            (remove #(contains? #{:a-rpcn-single-stage-unbalanced
                                                  :a-rpcn-single-stage
                                                  :a-rpcn-1-single-stage-unbalanced
                                                  :a-rpcn-1-single-stage
                                                  :a-rpcn-unbalanced
                                                  :a-rpcn
                                                  :a-rpcn-1-unbalanced
                                                  :a-rpcn-1} %)
                                    attack-strategies)
                            attack-strategies)
        attack-strategies (if (<= (:independent-sigma io-attack-data) 0)
                            (remove #(contains? #{:a-rpin-unbalanced
                                                  :a-rpin
                                                  :a-rpin-1-unbalanced
                                                  :a-rpin-1} %)
                                    attack-strategies)
                            attack-strategies)]
    (zipmap
     attack-strategies
     (for [strategy attack-strategies]
       (let [{:keys [attack-function cumulative-noise? independent-noise? bias-input-prob? only-one-known?]}
             (get (attack-strategy-map) strategy)
             start-time (get-current-thread-time!)
             attempts (doall
                       (for [i (range attempt-count)]
                         (attack-function
                          io-attack-data (seeded-rng (+ base-attack-seed i))
                          :optimization-max-evaluations optimization-max-evaluations
                          :optimization-relative-threshold optimization-relative-threshold
                          :cumulative-noise? cumulative-noise?
                          :independent-noise? independent-noise?
                          :bias-input-prob? bias-input-prob?
                          :only-one-known? only-one-known?)))
             stop-time (get-current-thread-time!)
             attempts (map #(assoc %
                                   :relative-error
                                   (relative-error original-record
                                                   (take (count original-record) (:optimum %)))
                                   :mean-attempt-cpu-nanoseconds
                                   (/ (- (:cpu-nano stop-time)
                                         (:cpu-nano start-time))
                                      attempt-count))
                           attempts)
             best-attempt (apply
                           max-key :score
                           attempts)]
         best-attempt)))))

(defn evaluate-privacy
  "Perform an attack-based evaluation of the privacy of the given
  masking using the given privacy-evaluation-configuration. Return a
  map that contains the attack-results of each evaluation performed,
  as well as relevant configuration information."
  [rng input-matrix masked-matrix projection-sigma
   independent-sigma cumulative-sigma translation noise-log
   {:keys [optimization-max-evaluations optimization-relative-threshold
           attack-count attempt-count
           known-record-counts known-record-ranges known-record-range-position
           attack-strategies evaluation-threads]
    :as privacy-evaluation-configuration}]
  ;; A separate evaluation for each prior knowledge configuration
  {:configuration privacy-evaluation-configuration
   :evaluations
   (pmap-pool
    (or evaluation-threads 1)
    (fn [[known-record-count known-record-range rng]]
      (let [known-record-range-position (or known-record-range-position
                                            :middle)]
       (debug
        (sync-println (str "Starting: privacy-evaluation"
                           "-pf" (m/column-count masked-matrix)
                           "-ps" projection-sigma
                           "-is" independent-sigma
                           "-cs" cumulative-sigma
                           "-tr" translation
                           "-krc" known-record-count
                           "-krr" known-record-range
                           "-krrp" known-record-range-position)))
       ;; Record contains raw results for this
       ;; prior-knowledge-configuration.
       {:known-record-count known-record-count
        :known-record-range known-record-range
        :known-record-range-position known-record-range-position
        :attack-results
        (doall
         (for [i (range (or attack-count 100))]
           (let [;; We may need to try generate-io-attack-data multiple
                 ;; times, in case the first few selected records end
                 ;; up being linearly dependent with all other records.
                 io-attack-data (try-times
                                 100
                                 (generate-io-attack-data! (seeded-rng (next-derived-seed! rng))
                                                           input-matrix
                                                           masked-matrix
                                                           projection-sigma
                                                           independent-sigma
                                                           cumulative-sigma
                                                           known-record-count
                                                           known-record-range
                                                           known-record-range-position))
                 original-record (m/get-row input-matrix
                                            (get-in io-attack-data [:unknown :index]))
                 known-indexes (map :index (:knowns io-attack-data))
                 true-known-noise-differences (map #(reduce m/add (->> noise-log
                                                                       (drop (inc %1))
                                                                       (take (- %2 %1))))
                                                   known-indexes (rest known-indexes))
                 attack-result (perform-attack (seeded-rng (next-derived-seed! rng))
                                               io-attack-data original-record
                                               (or attempt-count 100)
                                               :optimization-max-evaluations optimization-max-evaluations
                                               :optimization-relative-threshold optimization-relative-threshold
                                               :attack-strategies attack-strategies)]
             {:strategies attack-result
              :known-indexes (->> io-attack-data :knowns (map :index))
              :unknown-index (->> io-attack-data :unknown :index)
              :original-record (map double original-record)
              :true-known-noise-differences true-known-noise-differences
              :true-unknown-cumulative-noise
              (reduce m/add (take (:index (:unknown io-attack-data)) noise-log))})))}))
    ;; Attack configuration combinations.
    (for [known-record-count (or known-record-counts
                                 (get-default-known-record-counts (m/column-count masked-matrix)))
          known-record-range (or known-record-ranges
                                 [0.1 0.5 1])]
      ;; Each configuration needs its own rng to prevent race
      ;; conditions when multi-threading.
      [known-record-count known-record-range (seeded-rng (next-derived-seed! rng))]))})

;; == Combined Evaluation ==

(defn masking-experiment
  [& {:keys [raw-dataset projection-features projection-sigma
             independent-noise-sigma cumulative-noise-sigma
             translation classifier-fns privacy-evaluation-configuration
             seed evaluations]}]
  (let [rng (seeded-rng seed)
        ;; We max/min normalise the dataset so that the effect of the
        ;; same sigma (particularly for additive noise) is consistent
        ;; across different datasets. This was previously done in
        ;; Chen, K., Sun, G., & Liu, L. (2007, April). Towards
        ;; attack-resilient geometric data perturbation. In
        ;; proceedings of the 2007 SIAM international conference on
        ;; Data mining (pp. 78-89). Society for Industrial and Applied
        ;; Mathematics.
        dataset (normalise-dataset raw-dataset)
        noise-log-atom (atom [])
        dataset-mask-factory (make-random-projection-with-noise-and-translation-mask-factory
                              projection-features projection-sigma
                              independent-noise-sigma cumulative-noise-sigma
                              translation
                              :seed (next-derived-seed! rng)
                              :noise-log-atom noise-log-atom)
        masked-dataset (mask-dataset (make-dataset-mask dataset-mask-factory dataset) dataset)
        input-matrix (dataset->matrix dataset)
        masked-matrix (dataset->matrix masked-dataset)
        evaluations (or evaluations [:accuracy :privacy])
        output (zipmap
                evaluations
                (for [evaluation evaluations]
                  (case evaluation
                    :accuracy (map-vals #(test-classification-accuracy % masked-dataset)
                                        classifier-fns)
                    :privacy (evaluate-privacy rng input-matrix masked-matrix
                                               projection-sigma independent-noise-sigma
                                               cumulative-noise-sigma translation
                                               @noise-log-atom
                                               privacy-evaluation-configuration))))]
    (assoc output
           :projection-features projection-features
           :projection-sigma projection-sigma
           :independent-noise-sigma independent-noise-sigma
           :cumulative-noise-sigma cumulative-noise-sigma
           :translation translation
           :seed seed)))

(defn flatten-masking-experiment-recoveries
  "Take a sequence of outputs from masking-experiment, and return a
  flattened list of all record recoveries from attacks."
  [results]
  (for [result results
        evaluation (-> result :privacy :evaluations)
        attack (-> evaluation :attack-results)
        [strategy recovery] (-> attack :strategies)]
    (assoc recovery
           :strategy strategy
           :known-record-count (:known-record-count evaluation)
           :known-record-range (:known-record-range evaluation)
           :projection-features (:projection-features result)
           :projection-sigma (:projection-sigma result)
           :independent-noise-sigma (:independent-noise-sigma result)
           :cumulative-noise-sigma (:cumulative-noise-sigma result)
           :translation (:translation result)
           :known-indexes (:known-indexes attack)
           :unknown-index (:unknown-index attack))))

(defn unknown-record-displacement
  "Return the displacement of the unknown record from the known records
  in a flattened or attack result: the smallest difference between the
  unknown index and any known index."
  [result]
  (->> result
       :known-indexes
       (map #(Math/abs (- (:unknown-index result) %)))
       (reduce min)))

(defn unknown-record-relative-position
  [result]
  (cond
    (< (:unknown-index result) (apply min (:known-indexes result))) :before
    (> (:unknown-index result) (apply max (:known-indexes result))) :after
    :default :middle))

(defn prob-eps-privacy-breach
  "Return the probability of an epsilon-privacy breach - the proportion
  of recoveries with a relative error less than epsilon."
  [flat-results epsilon]
  (/ (count (filter #(< (:relative-error %) epsilon) flat-results))
     (count flat-results)))

(defn map-attack-results
    [func results]
    (for [result results]
        (update-in result [:privacy :evaluations]
                   (fn [evaluations]
                       (for [evaluation evaluations]
                           (update evaluation :attack-results
                                   (fn [attack-results]
                                       (map func attack-results))))))))

(defn add-combined-result
    [results metric strategies]
    (let [strategy-name (keyword (str (name metric) "_" (string/join ":" (map name strategies))))
          comparison-operator (case metric
                                  :relative-error min-key
                                  :score max-key
                                  :default (throw (Exception. "Unsupported combined result metric")))]
        (map-attack-results
            (fn [attack-result]
                (assoc-in attack-result [:strategies strategy-name]
                          (->> attack-result
                               :strategies
                               (filter #(contains? (set strategies) (first %)))
                               vals
                               (apply comparison-operator metric))))
            results)))

(defn get-cumulative-noise-sigma
  "Gets a cumulative-noise-sigma that results in an equivalent amount of
  total noise as the given independent-noise-sigma for a dataset with
  the given record-count."
  [independent-noise-sigma record-count]
  (/ (* record-count independent-noise-sigma)
     (->> (range 1 (inc record-count))
          (map sqrt)
          (reduce +))))
