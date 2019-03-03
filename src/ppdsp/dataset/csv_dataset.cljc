(ns ppdsp.dataset.csv-dataset
  (:require [ppdsp.dataset.base
             :refer [get-schema pop-record-and-rest]]
            [ppdsp.utils
             :refer [index-of seq-contains? with-file-reader]]
            [clojure.string :as str])
  (:import [ppdsp.dataset.base StaticDataset]))

(defn- lines->schema [header-line data-lines numeric-features]
  (->> header-line
       (map-indexed
        (fn [col-idx col-name]
          {:name col-name
           :options
           (if (seq-contains? numeric-features col-name)
             :numeric
             ;; For nominal features, get a sorted list of unique
             ;; possible values.
             (->> data-lines
                  (map #(get % col-idx))
                  (distinct)
                  (sort)
                  (into [])))}))
       (into [])))

(defn- line->record [schema line]
  (->> line
       (map-indexed
        (fn [col-idx col-val]
          (if (empty? col-val)
            nil
            (let [options (get-in schema [col-idx :options])]
              (if (= options :numeric)
                ;; Easy numeric conversion
                (read-string col-val)
                ;; For easy MOA instance construction, nominal record
                ;; values are numeric indexes into the options for the
                ;; column.
                (index-of options col-val))))))
       (into [])))

(defn read-csv-dataset
  ([path max-records]
   (read-csv-dataset path max-records []))
  ([path max-records numeric-features]
   (with-file-reader path reader
     (let [lines (->> (line-seq reader)
                      (map #(str/split % #",")))
           header-line (first lines)
           data-lines (take max-records (rest lines))
           schema (lines->schema header-line data-lines numeric-features)
           records (->> data-lines
                        (map #(line->record schema %))
                        (map-indexed (fn [idx record]
                                       {:id (inc idx) :values record})))]
       (StaticDataset. schema records)))))
