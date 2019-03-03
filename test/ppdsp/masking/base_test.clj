(ns ppdsp.masking.base-test
  (:use midje.sweet)
  (:require
   [ppdsp.masking.projection :refer [make-static-projection-mask-factory]]
   [ppdsp.masking.base :refer [make-dataset-mask make-distributed-dataset-mask
                              mask-dataset]]
   [ppdsp.dataset.base :refer [dataset->matrix get-schema pop-record-and-rest]]
   [ppdsp.utils.random :refer [seeded-rng next-int! next-float!]]
   [clojure.core.matrix :as m]
   [clojure.pprint :refer [pprint]])
  (:import [ppdsp.dataset.base StaticDataset]))

(defn- make-demo-numeric-dataset
  [feature-count record-count & {:keys [seed]}]
  (let [seed (or seed 1)
        schema (-> (for [i (range feature-count)]
                     {:name (str "f" i) :options :numeric})
                   (concat [{:name "class" :options ["0" "1"]}])
                   (vec))
        rng (seeded-rng seed)
        records (for [i (range record-count)]
                  {:id i :values (conj (vec (take feature-count (repeatedly #(next-float! rng))))
                                       (str (next-int! rng 2)))})]
    (StaticDataset. schema records)))

(defn- dataset->classes
  [dataset]
  (loop [dataset dataset
         classes []]
    (let [[record rest-dataset] (pop-record-and-rest dataset)]
      (if record
        (recur rest-dataset (conj classes (last (:values record))))
        classes))))

(defn- round-precision
  [x precision]
  (let [factor (Math/pow 10 precision)]
    (-> x
        (* factor)
        (Math/round)
        (/ factor)
        (float))))

(facts "about make-dataset-mask-factory"
       (let [dataset (make-demo-numeric-dataset 5 10)
             projection [[1 0 0]
                         [0 1 0]
                         [0 0 1]
                         [0 0 0]
                         [0 0 0]]
             mask-factory (make-static-projection-mask-factory projection)
             mask (make-dataset-mask mask-factory dataset)
             masked-dataset (mask-dataset mask dataset)]
         (fact "it should fail when some dataset features are not numeric"
               (let [dataset (assoc-in dataset [:schema 0 :options] ["0" "1"])]
                 (try
                   (make-dataset-mask mask-factory dataset)
                   (catch IllegalArgumentException e (.getMessage e)))
                 => "Some dataset features are not numeric"))
         (fact "it should fail when the projection rows and dataset columns are not equal"
               (let [projection (conj projection [0 0 0])]
                 (try
                   (make-dataset-mask (make-static-projection-mask-factory projection)
                                      dataset)
                   (catch IllegalArgumentException e (.getMessage e)))
                 => "Number of matrix rows and dataset features differ"))
         (fact "it should mask feature values correctly"
               (m/emap #(round-precision % 6)
                       (dataset->matrix masked-dataset))
               =>
               (m/emap #(round-precision % 6)
                       (m/submatrix (dataset->matrix dataset) [[0 10] [0 3]])))
         (fact "it should not change record classes"
               (dataset->classes masked-dataset) => (dataset->classes dataset))
         (fact "it should mask the schema correctly"
               (get-schema masked-dataset)
               =>
               [{:name "0-dim0" :options :numeric}
                {:name "0-dim1" :options :numeric}
                {:name "0-dim2" :options :numeric}
                {:name "class" :options ["0" "1"]}])))

(facts "about make-distributed-dataset-mask"
       (let [dataset (make-demo-numeric-dataset 4 10)
             projection [[1 0 0]
                         [0 1 0]
                         [0 0 1]
                         [0 0 0]]
             mask (make-distributed-dataset-mask
                   {["f0" "f2"] (make-static-projection-mask-factory
                                 [[1]
                                  [0]])
                    ["f1" "f3"] (make-static-projection-mask-factory
                                 [[1]
                                  [0]])}
                   dataset)
             masked-dataset (mask-dataset mask dataset)]
         (fact "it should mask feature values correctly"
               (m/emap #(round-precision % 6)
                       (dataset->matrix masked-dataset))
               =>
               (m/emap #(round-precision % 6)
                       (m/submatrix (dataset->matrix dataset) [[0 10] [0 2]])))
         (fact "it should not change record classes"
               (dataset->classes masked-dataset) => (dataset->classes dataset))
         (fact "it should mask the schema correctly"
               (get-schema masked-dataset)
               =>
               [{:name "0-dim0" :options :numeric}
                {:name "1-dim0" :options :numeric}
                {:name "class" :options ["0" "1"]}])))
