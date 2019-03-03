(ns ppdsp.masking.utils-test
  (:use midje.sweet)
  (:require
   [ppdsp.masking.utils :as mu]
   [ppdsp.masking.attack-data :refer [build-io-attack-data]]
   [clojure.core.matrix :as m]))

(let [io-attack-data-for-unknown
      (fn [unknown-index]
        (build-io-attack-data 0.1 0.0 0.001
                              (m/matrix [[1 2 3]
                                         [4 5 6]
                                         [7 8 9]
                                         [10 11 12]
                                         [13 14 15]])
                              (m/matrix [[10 20 30]
                                         [40 50 60]
                                         [70 80 90]
                                         [100 110 120]
                                         [130 140 150]])
                              (disj #{0 2 4} unknown-index)
                              unknown-index))]
  (facts "about build-attack-col-matrices"
         (fact "it should create matrices correctly"
               (mu/build-attack-col-matrices (io-attack-data-for-unknown 2))
               => {:unknown-col-index 1
                   :input-cols (m/matrix [[1 0 13]
                                          [2 0 14]
                                          [3 0 15]])
                   :masked-cols (m/matrix [[10 70 130]
                                           [20 80 140]
                                           [30 90 150]])})
         (fact "it should create matrices correctly when the unknown record is first"
               (mu/build-attack-col-matrices (io-attack-data-for-unknown 0))
               => {:unknown-col-index 0
                   :input-cols (m/matrix [[0 7 13]
                                          [0 8 14]
                                          [0 9 15]])
                   :masked-cols (m/matrix [[10 70 130]
                                           [20 80 140]
                                           [30 90 150]])})
         (fact "it should create matrices correctly when the unknown record is last"
               (mu/build-attack-col-matrices (io-attack-data-for-unknown 4))
               => {:unknown-col-index 2
                   :input-cols (m/matrix [[1 7 0]
                                          [2 8 0]
                                          [3 9 0]])
                   :masked-cols (m/matrix [[10 70 130]
                                           [20 80 140]
                                           [30 90 150]])})))
