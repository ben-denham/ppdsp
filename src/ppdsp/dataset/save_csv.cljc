(ns ppdsp.dataset.save-csv
  (:require [ppdsp.dataset.base
             :refer [get-schema pop-record-and-rest]]
            [clojure.java.io :as io]
            [clojure.string :as s]))

(defn- get-header-line
  [schema]
  (map :name schema))

(defn- record->line
  [record schema]
  (map
   (fn [value {:keys [options]}]
     (if (= :numeric options)
       value
       (get options value)))
   (:values record) schema))

(defn- get-data-lines
  [dataset]
  (let [schema (get-schema dataset)]
    (loop [lines []
           dataset dataset]
      (let [[record rest-dataset] (pop-record-and-rest dataset)]
        (if record
          (recur (conj lines (record->line record schema))
                 rest-dataset)
          lines)))))

(defn- write-csv-lines
  [filename lines]
  (with-open [writer (io/writer filename)]
    (doseq [line lines]
      (.write writer (str (s/join "," line) "\n")))))

(defn save-dataset-to-csv
  [dataset filename]
  (let [header-line (get-header-line (get-schema dataset))
        data-lines (get-data-lines dataset)]
    (write-csv-lines filename (concat [header-line] data-lines))))
