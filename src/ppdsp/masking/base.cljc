(ns ppdsp.masking.base
  (:require [ppdsp.dataset.base
             :refer [get-schema pop-record-and-rest]])
  (:import [ppdsp.dataset.base Dataset]))

(defprotocol DatasetMask
  "A DatasetMask is created for a specific dataset schema and can be
  used to mask the records of that dataset according to some masking
  scheme."
  (get-masked-schema [this])
  (mask-record [this record]))

(defprotocol DatasetFeatureMask
  "A DatasetFeatureMask is created for a specific dataset schema and
  defines the transformations for the features of the dataset's schema
  and records."
  (masked-schema-features [this])
  (mask-record-features [this features]))

(defn decorate-mask-factory
  "Decorate the given mask-factory by calling the given
  schema-features-decorator and record-features-decorator on the
  outputs of mask-factory's masked-schema-features and
  masked-record-features respectively."
  [mask-factory schema-features-decorator record-features-decorator]
  (fn [schema-features]
    (let [base-mask (mask-factory schema-features)]
      (reify DatasetFeatureMask
        (masked-schema-features [this]
          (schema-features-decorator (masked-schema-features base-mask)))
        (mask-record-features [this features]
          (record-features-decorator (mask-record-features base-mask features)))))))

(defn make-distributed-dataset-mask
  "Takes a map of feature keys to DatasetFeatureMasks for those groups
  of features, and returns a DatasetMask for the given feature-set."
  [feature-group-mask-factories dataset]
  (let [schema (get-schema dataset)
        ;; Assumes last attribute is class
        schema-features (drop-last schema)
        feature-keys (map :name schema-features)
        schema-features-map (zipmap feature-keys schema-features)
        feature-group-masks
        (for [[group-keys mask-factory] feature-group-mask-factories]
          (->> group-keys
               (select-keys schema-features-map)
               vals
               mask-factory
               (vector group-keys)))
        masked-schema-features (->> feature-group-masks
                                    (map second)
                                    (map masked-schema-features)
                                    (map-indexed
                                     (fn [idx masked-group-schema]
                                       (map (fn [feature-schema]
                                              (update feature-schema :name #(str idx "-" %)))
                                            masked-group-schema)))
                                    (reduce concat))
        ;; Combine the masked features schema with the original class
        ;; schema
        masked-schema (conj (vec masked-schema-features) (last schema))]
    (reify DatasetMask
      (get-masked-schema [this]
        masked-schema)
      (mask-record [this record]
        ;; Update the record feature values using the
        ;; DatasetFeatureMasks for each feature group.
        (update record :values
                (fn [attributes]
                  (let [feature-values-map (zipmap feature-keys
                                                   (drop-last attributes))
                        masked-feature-group-values
                        (for [[group-keys feature-mask] feature-group-masks]
                          (->> group-keys
                               (select-keys feature-values-map)
                               vals
                               (mask-record-features feature-mask)))]
                    (conj (vec (reduce concat masked-feature-group-values))
                          (last attributes)))))))))

(defn make-dataset-mask
  "Takes a dataset-feature-mask-factory that generates a
  DatasetFeatureMask for a given dataset feature schema (excluding the
  class attribute) and returns a DatasetMask that can be applied to
  the given dataset."
  [dataset-feature-mask-factory dataset]
  (let [all-feature-names (vec (map :name (drop-last (get-schema dataset))))]
    ;; This is a special case of a distributed mask: All features
    ;; have one mask applied.
    (make-distributed-dataset-mask
     {all-feature-names dataset-feature-mask-factory}
     dataset)))

(defn mask-dataset
  "Returns a new dataset that was created by applying the given
  dataset-mask to every record in the given dataset."
  [dataset-mask dataset]
  (reify Dataset
    (get-schema [this]
      (get-masked-schema dataset-mask))
    (pop-record-and-rest [this]
      (let [[raw-record raw-rest] (pop-record-and-rest dataset)]
        (if raw-record
          [(mask-record dataset-mask raw-record)
           (mask-dataset dataset-mask raw-rest)]
          [nil this])))))
