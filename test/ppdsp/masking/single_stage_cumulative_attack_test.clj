(ns ppdsp.masking.single-stage-cumulative-attack-test
  (:use midje.sweet)
  (:require
   [ppdsp.masking.single-stage-cumulative-attack :as ma]
   [ppdsp.masking.attack-data :refer [build-io-attack-data]]
   [clojure.core.matrix :as m]))

(facts "about get-noise-difference-sigmas"
       (fact "it should return the correct sigmas in the correct order"
             (let [record-count 1000
                   cumulative-sigma 0.001
                   io-attack-data
                   (build-io-attack-data 0.1 0.0 cumulative-sigma
                                         (m/reshape (range (* record-count record-count))
                                                    [record-count record-count])
                                         (m/reshape (range (* record-count record-count))
                                                    [record-count record-count])
                                         [0 99 999]
                                         499)]
               (ma/get-noise-difference-sigmas io-attack-data)
               => (->> [99 400 500]
                       (map #(Math/sqrt (* (Math/pow cumulative-sigma 2) %)))))))

(facts "about get-phi-logp"
       (fact "it should return a value under normal circumstances"
             (let [x (m/matrix [[1 1 1]
                                [1 2 2]
                                [1 2 3]])
                   y (m/matrix [[2 2 2]
                                [2 3 3]
                                [2 3 4]])]
               (ma/get-phi-logp x y)
               => -15.326691499835558))
       (fact "it should return -Infinity if x does not have full column rank"
             (let [x (m/matrix [[1 1 1]
                                [1 1 1]
                                [1 1 1]])
                   y (m/matrix [[2 2 2]
                                [2 2 2]
                                [2 2 2]])]
               (ma/get-phi-logp x y)
               => Double/NEGATIVE_INFINITY))
       (fact "it should throw an exception if the covariance array has full column rank but is not positive definite"
             (let [;; Example from letter recognition dataset.
                   x (m/matrix [[0.3333333333333333,1.1067388129526214,0.2666666666666667],
                                [0.8,               1.0523516912562685,0.4],
                                [0.2,               1.3768528538876366,0.2666666666666667],
                                [0.4666666666666667,0.8366248228115485,0.2666666666666667],
                                [0.1333333333333333,0.6208978940597113,0.1333333333333333]])
                   y (m/matrix [[1 1 1]
                                [1 1 1]])]
               (try
                 (ma/get-phi-logp x y)
                 (catch IllegalArgumentException ex [(type ex) (.getMessage ex)]))
               => [IllegalArgumentException "The matrix is not positive definite."])))

(facts "about remove-cumulative-noise"
       (fact "it should return a value under normal circumstances"
             (ma/remove-cumulative-noise
              ;; 3 record differences as rows
              [-1 -2 -3
               -4 -5 -6
               -7 -8 -9]
              ;; 3 records as columns
              (m/matrix [[10 40 70]
                         [20 50 80]
                         [30 60 90]])
              ;; Noise difference sigmas
              [1 10 100])
             => [(m/matrix [[11 45 82]
                            [22 57 95]
                            [33 69 108]])
                 [-1.4189385332046727
                  -2.9189385332046727
                  -5.418938533204672
                  -3.3015236261987186
                  -3.3465236261987186
                  -3.4015236261987187
                  -5.526558719192764
                  -5.527308719192764
                  -5.528158719192764]]))
