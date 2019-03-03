(ns ppdsp.masking.attack-data
  (:require [clojure.core.matrix :as m]))

(defrecord IOAttackRecord [index input masked])

(defrecord IOAttackData [projection-sigma independent-sigma cumulative-sigma knowns unknown])

(defn build-known-attack-record
  "Represents a known record in an input/output attack."
  [input-matrix masked-matrix index]
  (IOAttackRecord. index
                   (m/get-row input-matrix index)
                   (m/get-row masked-matrix index)))

(defn build-unknown-attack-record
  "Represents an unknown record in an input/output attack."
  [masked-matrix index]
  (IOAttackRecord. index
                   nil
                   (m/get-row masked-matrix index)))

(defn build-io-attack-data
  "Represents the data available during a known-io attack on a single
  unknown record given a set of known records."
  [projection-sigma independent-sigma cumulative-sigma input-matrix masked-matrix
   known-record-indexes unknown-record-index]
  (IOAttackData. projection-sigma
                 independent-sigma
                 cumulative-sigma
                 (->> known-record-indexes
                      ;; Sort to ensure records are in correct order.
                      (sort)
                      (map #(build-known-attack-record
                             input-matrix masked-matrix %)))
                 (build-unknown-attack-record
                  masked-matrix unknown-record-index)))

(defn attack-input-feature-count
  [io-attack-data]
  (m/column-count (m/row-matrix (:input (first (:knowns io-attack-data))))))

(defn attack-masked-feature-count
  [io-attack-data]
  (m/column-count (m/row-matrix (:masked (first (:knowns io-attack-data))))))

(defn closest-known-index-to-unknown
  "Returns the closest known index to the unknown record."
  [io-attack-data]
  (apply min-key
         #(Math/abs (- (-> io-attack-data :unknown :index) %))
         (->> io-attack-data :knowns (map :index))))

(defn split-knowns-at-unknown
  "Return a map with :before and :after keys that reference the subsets
  of known records before and after the unknown record in the given
  io-attack-data."
  [io-attack-data]
  (->> (:knowns io-attack-data)
       (group-by #(if (< (:index %) (:index (:unknown io-attack-data)))
                    :before
                    :after))))
