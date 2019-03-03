(ns ppdsp.masking.evaluation-test
  (:use midje.sweet)
  (:require [ppdsp.masking.evaluation :as e]
            [ppdsp.masking.projection :refer [random-gauss-matrix]]
            [ppdsp.utils.random :refer [seeded-rng next-int!]]
            [clojure.core.matrix :as m]
            [clojure.math.numeric-tower :refer [expt]])
  (:import [ppdsp.dataset.base StaticDataset]))

(defn- round-precision
  [x precision]
  (let [factor (Math/pow 10 precision)]
    (-> x
        (* factor)
        (Math/round)
        (/ factor)
        (float))))

(defn- matrix->dataset
  [matrix & {:keys [seed]}]
  (let [seed (or seed 1)
        feature-count (m/column-count matrix)
        schema (-> (for [i (range feature-count)]
                     {:name (str "f" i) :options :numeric})
                   (concat [{:name "class" :options ["0" "1"]}])
                   (vec))
        rng (seeded-rng seed)
        records (for [[i row] (map-indexed vector (m/rows matrix))]
                  {:id i :values (conj (vec row) (str (next-int! rng 2)))})]
    (StaticDataset. schema records)))

(facts "about pairwise-row-euclidean-distances"
       (fact "it should return the correct distance for a trivial example"
             (e/pairwise-row-euclidean-distances
              [[3 0]
               [0 4]])
             =>
             (m/matrix
              [[0 0]
               [5 0]]))
       (fact "it should return a lower-triangular matrix"
             (m/lower-triangular? (e/pairwise-row-euclidean-distances
                                   (random-gauss-matrix [10 15] 0 4)))
             => true)
       (fact "it should always return zero distance between the same row (along the diagonal)"
             (m/diagonal (e/pairwise-row-euclidean-distances
                          (random-gauss-matrix [10 15] 0 4)))
             =>
             (m/matrix (repeat 10 0)))
       (fact "its result dimensions should both equal the input rows"
             (m/shape (e/pairwise-row-euclidean-distances
                       (random-gauss-matrix [10 15] 0 4)))
             => [10 10])
       (fact "it should have the correct euclidean distance between each row"
             (let [matrix (random-gauss-matrix [10 15] 0 4)
                   matrix-rows (m/rows matrix)]
               (m/emap #(round-precision % 10)
                       (m/matrix :persistent-vector
                                 (e/pairwise-row-euclidean-distances matrix)))
               => (m/emap #(round-precision % 10)
                          (m/matrix :persistent-vector
                                    (for [i (range 10)]
                                      (for [j (range 10)]
                                        (if (<= j i)
                                          (m/distance (nth matrix-rows i)
                                                      (nth matrix-rows j))
                                          0.0))))))))

(facts "about row-distance-error"
       (fact "it should return zero when comparing a dataset to itself"
             (let [dataset (matrix->dataset (random-gauss-matrix [10 15] 0 4))]
               (e/row-distance-error dataset dataset) => 0.0))
       (fact "it should return a correct SSE for known error"
             (let [m1 [[1 1 0 0 0 0]
                       [0 0 1 1 0 0]
                       [0 0 0 0 1 1]] ;; => Distance matrix = 3x3 lower triangle of "2"
                   m2 [[2 2 0 0 0 0]
                       [0 0 2 2 0 0]
                       [0 0 0 0 2 2]]] ;; => Distance matrix = 3x3 lower triangle of "4"
               (e/row-distance-error (matrix->dataset m1)
                                     (matrix->dataset m2))
               =>
               (-> (- 4 2) ;; Difference in one cell
                   (expt 2) ;; Squared
                   (* 3) ;; Number of cells in 3x3 lower triangle
                   (float)))))

(facts "about relative-error"
       (fact "it should return 0 for a perfect recovery"
             (e/relative-error [1 2 3 4 5] [1 2 3 4 5]) => 0.0)
       (fact "it should return the error magnitude divided the true record's magnitude"
             (e/relative-error [3 4] [3 5]) => 0.2)
       (fact "it should return the error magnitude divided the true record's magnitude for a more complex example"
             (e/relative-error [1 2 3 4 5] [5 4 3 2 1]) => 0.8528028654224418))

(facts "about get-record-bounds"
       (facts "about :middle"
              (fact "it should return the min and max bounds for a full range"
                    (e/get-record-bounds 1000 1 :middle) => [0 1000])
              (fact "it should return the correct bounds for an even
                     number of records that are divisible by half the range"
                    (e/get-record-bounds 1000 0.5 :middle) => [250 750])
              (fact "it should return the correct bounds for an even number
                     of records that are not divisible by half the range"
                    (e/get-record-bounds 30 0.5 :middle) => [7 23])
              (fact "it should return the correct bounds for an odd number of
                     records that are divisible by half the range"
                    (e/get-record-bounds 15 0.667 :middle) => [2 12])
              (fact "it should return the correct bounds for an odd number of
                     records that are not divisible by half the range"
                    (e/get-record-bounds 15 0.5 :middle) => [3 11]))
       (facts "about :start"
              (fact "it should return the min and max bounds for a full range"
                    (e/get-record-bounds 1000 1 :start) => [0 1000])
              (fact "it should return the correct bounds for an even
                     number of records that are divisible by the range"
                    (e/get-record-bounds 1000 0.5 :start) => [0 500])
              (fact "it should return the correct bounds for an even number
                     of records that are not divisible by the range"
                    (e/get-record-bounds 30 0.25 :start) => [0 8])
              (fact "it should return the correct bounds for an odd number of
                     records that are divisible by the range"
                    (e/get-record-bounds 15 0.333 :start) => [0 5])
              (fact "it should return the correct bounds for an odd number of
                     records that are not divisible by the range"
                    (e/get-record-bounds 15 0.25 :start) => [0 4])))

(facts "about get-default-known-record-counts"
       (fact "it should return the correct values for an even number of masked features"
             (e/get-default-known-record-counts 6) => [1 3 5])
       (fact "it should return the correct values for an odd number of masked features"
             (e/get-default-known-record-counts 5) => [1 2 4])
       (fact "it should return the correct values for 4 masked features"
             (e/get-default-known-record-counts 4) => [1 2 3])
       (fact "it should return the correct values for 3 masked features"
             (e/get-default-known-record-counts 3) => [1 2])
       (fact "it should return the correct values for 2 masked features"
             (e/get-default-known-record-counts 2) => [1])
       (fact "it should return the correct values for 1 masked feature"
             (e/get-default-known-record-counts 1) => [1]))
