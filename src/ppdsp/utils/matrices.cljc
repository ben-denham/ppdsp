(ns ppdsp.utils.matrices
  (:require [ppdsp.utils :refer [seq-contains? debug]]
            [ppdsp.utils.random :refer [seeded-shuffle next-int!]]
            [clojure.core.matrix :as m]
            [clojure.core.matrix.protocols :as mp]
            [clojure.core.matrix.linear :as ml]
            [clojure.core.matrix.stats :as ms]))

(defn get-matrix-rows
  "Return a new matrix with only the rows specified by row-indexes."
  [matrix row-indexes]
  (m/matrix (map #(m/get-row matrix %) row-indexes)))

(defn get-matrix-row-map
  "Return a map with row-indexes as keys and their associated rows in
  matrix as values."
  [matrix row-indexes]
  (->> (get-matrix-rows matrix row-indexes)
       (map vector row-indexes)
       (into (sorted-map))))

(defn concat-matrix-row
  "Return a new matrix with row appended to the end of matrix (even if
  matrix is empty)."
  [matrix row]
  (if (= (m/row-count matrix) 0)
    (m/matrix [row])
    (m/matrix (m/join matrix (m/row-matrix row)))))

(defn row-linearly-independent-to?
  "Returns true if row is linearly independent to all rows in
  matrix (even if matrix is empty). Will return false if matrix itself
  already has linear dependence."
  [matrix row]
  (if (= (m/row-count matrix) 0)
    true
    (let [combined-matrix (concat-matrix-row matrix row)]
      ;; Test for linear independence between row (full row-rank; rank
      ;; >= row count).
      (>= (ml/rank combined-matrix) (m/row-count combined-matrix)))))

(defn change-vector-magnitude
  "Take any (core.matrix) vector v and reduce its magnitude to
  new-magnitude without changing its direction."
  [v new-magnitude]
  (let [old-magnitude (m/length v)]
    (m/mul v (/ new-magnitude old-magnitude))))

(defn random-li-row-indexes!
  "Uses the provided rng to randomly choose a number (equal to
  row-count) of linearly independent rows from full-matrix, and
  returns the indexes of those rows. By providing init-li-row-indexes,
  the selected rows must also be linearly independent to the rows at
  those indexes. start-index and end-index can be provided to narrow
  the range of indexes searched to a certain range (inclusive of both
  start, exclusive of end). Throws an exception if it is impossible to
  find the desired number of linearly independent rows."
  [rng full-matrix row-count
   & {:keys [init-li-row-indexes start-index end-index]}]
  (if (= row-count 0)
    []
    (let [init-li-row-indexes (or init-li-row-indexes [])
          start-index (or start-index 0)
          end-index (or end-index (m/row-count full-matrix))]
      (loop [;; Possible row indexes to test are from the start-index to
             ;; the end-index excluding those in the
             ;; init-indexes. Shuffled so they are tested in a random
             ;; order.
             possible-indexes (as-> (range start-index end-index) $
                                (remove #(seq-contains? init-li-row-indexes %) $)
                                (seeded-shuffle $ (next-int! rng Integer/MAX_VALUE)))
             ;; For efficiency, maintain a matrix with all rows we need
             ;; to be linearly independent with.
             li-rows (get-matrix-rows full-matrix init-li-row-indexes)
             result-indexes []]
        (if-let [random-row-index (first possible-indexes)]
          (let [random-row (m/get-row full-matrix random-row-index)]
            (if (row-linearly-independent-to? li-rows random-row)
              (let [new-result-indexes (conj result-indexes random-row-index)]
                (if (>= (count new-result-indexes) row-count)
                  ;; We have reached the required number of rows, so
                  ;; we can return the found indexes (sorted).
                  (sort new-result-indexes)
                  ;; Add the row to the result, and keep searching.
                  (recur (rest possible-indexes)
                         (concat-matrix-row li-rows random-row)
                         new-result-indexes)))
              ;; The tested row was not linearly independent with
              ;; li-rows, so move on to the next possible-index
              (recur (rest possible-indexes) li-rows result-indexes)))
          ;; We have run out of possible-indexes without reaching the
          ;; required row-count.
          (throw (Exception. "Impossible to find linearly independent row(s).")))))))

(defn accumulate-rows
  "Returns a new matrix where each row is the sum of itself and all rows
  before it in the original matrix"
  [matrix]
  (let [result (m/mutable matrix)]
    (doseq [i (range 1 (m/row-count result))]
      (m/set-row! result i (m/add (m/get-row result i)
                                  (m/get-row result (dec i)))))
    result))

(defn join-matrices
  "Join any number of matrices along the specified axis."
  [axis & matrices]
  (loop [matrices matrices
         result (m/matrix [])]
    (if-let [matrix (first matrices)]
      (if (empty? matrix)
        (recur (rest matrices) result)
        (if (empty? result)
          (recur (rest matrices) matrix)
          (recur (rest matrices) (m/join-along axis result matrix))))
      result)))

(defn join-row-wise
  "Join any number of matrices as groups of rows."
  [& matrices]
  (apply join-matrices 0 matrices))

(defn join-col-wise
  "Join any number of matrices as groups of columns."
  [& matrices]
  (apply join-matrices 1 matrices))

(defn normalise-to-mean-zero-cols
  "Normalise matrix so all columns have mean zero."
  [matrix]
  (let [col-means (map ms/mean (m/columns matrix))]
    (m/sub matrix
           (m/broadcast col-means (m/shape matrix)))))

(defn normalise-to-mean-zero-rows
  "Normalise matrix so all rows have mean zero."
  [matrix]
  (m/transpose (normalise-to-mean-zero-cols (m/transpose matrix))))

(defn normalise-to-first-row
  "Subtract the first row from all rows in the matrix (including itself)."
  [matrix]
  (m/sub matrix (m/broadcast (m/get-row matrix 0) (m/shape matrix))))

(defn normalise-to-first-column
  "Subtract the first column from all columns in the matrix (including itself)."
  [matrix]
  (m/transpose (normalise-to-first-row (m/transpose matrix))))

(defn remove-first-column
  "Remove the first column from the matrix."
  [matrix]
  (m/submatrix matrix 1 [1 (dec (m/column-count matrix))]))

(defn remove-first-row
  "Remove the first row from the matrix."
  [matrix]
  (m/submatrix matrix 0 [1 (dec (m/row-count matrix))]))

(defn repeat-block-diagonal
  "Repeat the given matrix k times along the diagonal of a new matrix,
  and fill the rest of the matrix with zeroes. Equivalent to the
  Kronecker product of the matrix with a k-identity matrix."
  [matrix k]
  (let [[block-rows block-cols] (m/shape matrix)
        ;; Create an empty matrix of the correct shape.
        bd-matrix (m/mutable (m/zero-matrix (* block-rows k) (* block-cols k)))]
      (doseq [i (range block-rows)
              j (range block-cols)]
        ;; For each value in the original matrix...
        (let [value (m/mget matrix i j)]
          (doseq [l (range k)]
            ;; Set it as the value in the appropriate location of each
            ;; block along the diagonal.
            (m/mset! bd-matrix
                     (+ i (* l block-rows))
                     (+ j (* l block-cols))
                     value))))
      bd-matrix))

(defn matrix->nested-double-array
  "Convert a matrix into a nested Java array of doubles."
  [matrix]
  (->> matrix
       m/rows
       (map m/to-double-array)
       into-array))
