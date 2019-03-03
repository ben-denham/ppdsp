(ns ppdsp.training
  (:require
   [ppdsp.dataset.base
    :refer [pop-record-and-rest]]
   [ppdsp.classifier.base
    :refer [process-record]]))

(defn train-classifier
  ([classifier dataset]
   (train-classifier classifier dataset nil))
  ([classifier dataset progress-atom]
   (loop [dataset dataset
          results []
          i 0]
     (let [[record rest-dataset] (pop-record-and-rest dataset)]
       (if record
         (let [raw-result (doall (process-record classifier record))
               result (assoc raw-result :truth (last (:values record)))]
           (when (and (= (mod i 1000) 0) progress-atom)
             (reset! progress-atom i))
           (recur rest-dataset (conj results result) (inc i)))
         results)))))

(defn get-accuracy
  [results]
  (/ (count (filter #(= (:truth %) (:class %)) results))
     (count results)))
