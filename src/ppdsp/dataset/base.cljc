(ns ppdsp.dataset.base
  "IMPORTANT NOTE: There is a hard assumption throughout the codebase
  that the last value in each record is the class value."
  (:require [clojure.core.matrix :as m]
            [clojure.core.matrix.linear :as ml]))

(defprotocol Dataset
  (get-schema [dataset])
  (pop-record-and-rest [dataset]))

(defrecord StaticDataset [schema records]
  Dataset
  (get-schema [this]
    schema)
  (pop-record-and-rest [this]
    [(first records) (StaticDataset. schema (rest records))]))

(defn concat-datasets
  "Returns a new dataset made by concatenating multiple datasets"
  ([datasets]
   (concat-datasets datasets 0))
  ([[head-dataset & rest-datasets] next-id]
   ;; Build a re-ified Dataset for the collection of datasets.
   (let [schemas (map get-schema (conj rest-datasets head-dataset))]
     (when (not (apply = schemas))
       (throw (Exception. "Cannot concat datasets with different schemas.")))
     (reify Dataset
       (get-schema [this]
         (first schemas))
       (pop-record-and-rest [this]
         ;; Pop a record from the head dataset.
         (let [[record rest-head-dataset] (pop-record-and-rest head-dataset)]
           (if record
             ;; If a non-nill record was returned, return it (though
             ;; with an id in the sequence of this concatenation) with
             ;; a new concated-dataset that only has the "rest" of the
             ;; head dataset.
             [(assoc record :id next-id)
              (concat-datasets (concat [rest-head-dataset] rest-datasets)
                               (inc next-id))]
             (if (empty? rest-datasets)
               ;; If there are no records left in the head dataset and
               ;; no other datasets, then return nil and this
               ;; unchanged dataset.
               [nil this]
               ;; If there are no records left in the head dataset,
               ;; but their are other datasets, then recur with a
               ;; concated-dataset of the remaining datasets.
               (pop-record-and-rest (concat-datasets rest-datasets next-id))))))))))

(defn get-dataset-id-map
  "Returns a map of all records in the dataset keyed by :id"
  [dataset]
  (loop [dataset dataset
         record-map {}]
    (let [[record rest-dataset] (pop-record-and-rest dataset)]
      (if record
        (recur rest-dataset (assoc record-map (:id record) record))
        record-map))))

(defn get-dataset-cardinalities
  "Return a sequence of the cardinalities of the attributes in a
  dataset (containing only nominal attributes)."
  [dataset]
  (let [feature-options (->> dataset
                             (get-schema)
                             (map :options))]
    (when (some #(= :numeric %) feature-options)
      (throw (Exception. "Cannot get cardinality of numeric feature.")))
    (map count feature-options)))

(defn dataset->matrix
  "Converts a dataset into a vector of vectors representing the
  features (excludes the class)."
  [dataset]
  (loop [dataset dataset
         matrix []]
    (let [[record rest-dataset] (pop-record-and-rest dataset)]
      (if record
        (recur rest-dataset (conj matrix (vec (drop-last (:values record)))))
        matrix))))

(defn dataset->classes
  [dataset]
  (loop [dataset dataset
         classes []]
    (let [[record rest-dataset] (pop-record-and-rest dataset)]
      (if record
        (recur rest-dataset (conj classes (last (:values record))))
        classes))))

(defn matrix->dataset
  "Converts a schema, a vector of vectors (or any core.matrix)
  representing the features, and a vector representing the classes
  into a dataset."
  [schema matrix classes]
  (StaticDataset. schema
                  (map (fn [i row class]
                         {:id i :values (conj (vec row) class)})
                       (range (m/row-count matrix))
                       (m/rows matrix)
                       classes)))

(defn normalise-matrix-data
  "Normalise the values in each feature column of the dataset to
  be (/ (- x xmin) (- xmax xmin))"
  [matrix]
  (let [cols (m/columns matrix)
        mins (map #(apply min %) cols)
        maxs (map #(apply max %) cols)]
    (for [row (m/rows matrix)]
      (map (fn [x min max]
             (/ (- x min) (- max min)))
           row mins maxs))))

(defn normalise-dataset
  "Normalise the values in a dataset."
  [dataset]
  (matrix->dataset (get-schema dataset)
                   (normalise-matrix-data (dataset->matrix dataset))
                   (dataset->classes dataset)))

(defn first-record
  "Utility function to return a vector of values for the first record
  in the given dataset."
  [dataset]
  (->> dataset
       pop-record-and-rest
       first
       :values))

(defn dataset-matrix-rank
  "Return the matrix rank of the given dataset."
  [dataset]
  (-> dataset dataset->matrix m/matrix ml/rank))

(defn dataset-attribute-count
  "Returns the number of features in the dataset (excluding the class)."
  [dataset]
  (count (get-schema dataset)))

(defn dataset-feature-count
  "Returns the number of features in the dataset (excluding the class)."
  [dataset]
  (dec (dataset-attribute-count dataset)))

(defn dataset-record-count
  "Returns the number of records in the dataset."
  [dataset]
  (m/row-count (dataset->matrix dataset)))
