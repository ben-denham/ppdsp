(ns ppdsp.masking.attack-data-test
  (:use midje.sweet)
  (:require
   [ppdsp.masking.attack-data :as mad]
   [clojure.core.matrix :as m]))

(let [input-feature-count 11
      masked-feature-count 7
      record-count 100
      io-attack-data
      (mad/build-io-attack-data 0.1 0.0 0.01
                                (m/reshape (range (* record-count input-feature-count))
                                           [record-count input-feature-count])
                                (m/reshape (range (* record-count masked-feature-count))
                                           [record-count masked-feature-count])
                                [0] 1)]
  (facts "about attack-input-feature-count"
         (fact "it should return the correct count"
               (mad/attack-input-feature-count io-attack-data) => input-feature-count))
  (facts "about attack-masked-feature-count"
         (fact "it should return the correct count"
               (mad/attack-masked-feature-count io-attack-data) => masked-feature-count)))

(let [io-attack-data-for-unknown
      (fn [unknown-index]
        (mad/build-io-attack-data 0.1 0.0 0.001
                                  (m/matrix [[1 2 3]
                                             [4 5 6]
                                             [7 8 9]])
                                  (m/matrix [[10 20 30]
                                             [40 50 60]
                                             [70 80 90]])
                                  (disj (set (range 3)) unknown-index)
                                  unknown-index))]
  (facts "about split-knowns-at-unknown"
         (fact "it should split about the unknown record correctly"
               (let [io-attack-data (io-attack-data-for-unknown 1)]
                 (mad/split-knowns-at-unknown io-attack-data)
                 => {:before (take 1 (:knowns io-attack-data))
                     :after (take-last 1 (:knowns io-attack-data))}))
         (fact "it should split correctly when the unknown record is first"
               (let [io-attack-data (io-attack-data-for-unknown 0)]
                 (mad/split-knowns-at-unknown io-attack-data)
                 => {:after (:knowns io-attack-data)}))
         (fact "it should split correctly when the unknown record is last"
               (let [io-attack-data (io-attack-data-for-unknown 2)]
                 (mad/split-knowns-at-unknown io-attack-data)
                 => {:before (:knowns io-attack-data)}))))
