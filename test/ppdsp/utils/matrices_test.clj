(ns ppdsp.utils.matrices-test
  (:use midje.sweet)
  (:require
   [ppdsp.utils.matrices :as um]
   [ppdsp.utils.random :refer [seeded-rng]]
   [clojure.core.matrix :as m]))

(facts "about get-matrix-rows"
       (fact "it should return a matrix with the specified rows"
             (um/get-matrix-rows
              (m/matrix [[1 1 1]
                         [2 2 2]
                         [3 3 3]
                         [4 4 4]
                         [5 5 5]])
              [0 2 4])
             =>
             (m/matrix [[1 1 1]
                        [3 3 3]
                        [5 5 5]]))
       (fact "it should return an empty matrix when no rows are specified"
             (um/get-matrix-rows
              (m/matrix [[1 1 1]
                         [2 2 2]
                         [3 3 3]])
              [])
             =>
             (m/matrix [])))

(facts "about get-matrix-row-map"
       (fact "it should return a map with the specified rows"
             (um/get-matrix-row-map
              (m/matrix [[1 1 1]
                         [2 2 2]
                         [3 3 3]
                         [4 4 4]
                         [5 5 5]])
              [0 2 4])
             =>
             {0 (m/matrix [1 1 1])
              2 (m/matrix [3 3 3])
              4 (m/matrix [5 5 5])})
       (fact "it should return an empty map when no rows are specified"
             (um/get-matrix-row-map
              (m/matrix [[1 1 1]
                         [2 2 2]
                         [3 3 3]])
              [])
             =>
             {}))

(facts "about concat-matrix-row"
       (fact "it should return a new matrix with the row appended"
             (um/concat-matrix-row (m/matrix [[1 1 1]
                                              [2 2 2]
                                              [3 3 3]])
                                   (m/matrix [4 4 4]))
             => (m/matrix [[1 1 1]
                           [2 2 2]
                           [3 3 3]
                           [4 4 4]]))
       (fact "it should return a new matrix with the row appended if vectors are given as inputs"
             (um/concat-matrix-row [[1 1 1]
                                    [2 2 2]
                                    [3 3 3]]
                                   [4 4 4])
             => (m/matrix [[1 1 1]
                           [2 2 2]
                           [3 3 3]
                           [4 4 4]]))
       (fact "it should return a new matrix with the row appended if only the row is a vector"
             (um/concat-matrix-row (m/matrix [[1 1 1]
                                              [2 2 2]
                                              [3 3 3]])
                                   [4 4 4])
             => (m/matrix [[1 1 1]
                           [2 2 2]
                           [3 3 3]
                           [4 4 4]]))
       (fact "it should return a matrix with only the row if the matrix is empty"
             (um/concat-matrix-row [] [4 4 4])
             => (m/matrix [[4 4 4]])))

(facts "about row-linearly-independent-to?"
       (fact "it should return true if the row is linearly independent"
             (um/row-linearly-independent-to? (m/matrix [[1 1 1]
                                                         [1 2 2]])
                                              [1 2 3])
             => true)
       (fact "it should return false if the row is not linearly independent"
             (um/row-linearly-independent-to? (m/matrix [[1 1 1]])
                                              [1 1 1])
             => false)
       (fact "it should return false if the matrix itself is not linearly independent"
             (um/row-linearly-independent-to? (m/matrix [[1 1 1]
                                                         [2 2 2]])
                                              [1 2 3])
             => false)
       (fact "it should return true if the matrix is empty"
             (um/row-linearly-independent-to? []
                                              [1 2 3])
             => true))

(facts "about change-vector-magnitude"
       (fact "the new vector should have the new magnitude"
             (m/length (um/change-vector-magnitude [1 2 3] 50)) => 50.0)
       (fact "the new vector should have the same direction as the
       original vector for a positive magnitude"
             (let [v [1 2 3]]
               (um/row-linearly-independent-to? (m/row-matrix v)
                                                (um/change-vector-magnitude v 50))
               => false))
       (fact "it should flip the direction for a negative magnitude"
             (let [v [1 2 3]]
               (um/change-vector-magnitude v (- (m/length v))) => (m/sub v))))

(facts "about random-li-row-indexes!"
       (fact "it should return only one index if that is all that is requested"
             (count (um/random-li-row-indexes! (seeded-rng 1)
                                               (m/matrix [[1 1]
                                                          [1 2]
                                                          [1 3]])
                                               1))
             => 1)
       (fact "it should return the requested number of linearly independent rows"
             (um/random-li-row-indexes! (seeded-rng 1)
                                        (m/matrix [[1 1 1]
                                                   [1 2 2]
                                                   [1 2 3]])
                                        3)
             => [0 1 2])
       (fact "it should return rows that are linearly independent to the given initial rows"
             (um/random-li-row-indexes! (seeded-rng 1)
                                        (m/matrix [[1 1 1]
                                                   [1 2 2]
                                                   [1 2 3]])
                                        2
                                        :init-li-row-indexes [0])
             => [1 2])
       (fact "it should only consider rows within the given start/end range"
             (um/random-li-row-indexes! (seeded-rng 1)
                                        (m/matrix [[1 1 1]
                                                   [1 2 2]
                                                   [1 2 3]])
                                        1
                                        :start-index 1
                                        :end-index 2)
             => [1])
       (fact "it should throw an exception if more rows are requested than there are in the matrix"
             (try (um/random-li-row-indexes! (seeded-rng 1)
                                             (m/matrix [[1 1 1]
                                                        [1 2 2]
                                                        [1 2 3]])
                                             5
                                             :start-index 1
                                             :end-index 2)
                  (catch Exception ex (.getMessage ex)))
             => "Impossible to find linearly independent row(s).")
       (fact "it should throw an exception if more li rows are requested than there are in the matrix"
             (try (um/random-li-row-indexes! (seeded-rng 1)
                                             (m/matrix [[1 1]
                                                        [1 2]
                                                        [1 3]])
                                             3
                                             :start-index 1
                                             :end-index 2)
                  (catch Exception ex (.getMessage ex)))
             => "Impossible to find linearly independent row(s).")
       (fact "it should return an empty result when requesting 0 rows"
             (um/random-li-row-indexes! (seeded-rng 1)
                                        (m/matrix [[1 1]
                                                   [1 2]
                                                   [1 3]])
                                        0)
             => []))

(facts "about accumulate-rows"
       (fact "it should add previous rows to subsequent rows"
             (um/accumulate-rows (m/matrix [[1 1 1]
                                            [2 2 2]
                                            [3 3 3]]))
             => (m/matrix [[1 1 1]
                           [3 3 3]
                           [6 6 6]])))

(facts "about join-matrices"
       (fact "it should join matrices along the 0 axis"
             (um/join-matrices 0
                               (m/matrix [[1 1 1]
                                          [2 2 2]
                                          [3 3 3]])
                               (m/matrix [[4 4 4]
                                          [5 5 5]
                                          [6 6 6]]))
             => (m/matrix [[1 1 1]
                           [2 2 2]
                           [3 3 3]
                           [4 4 4]
                           [5 5 5]
                           [6 6 6]]))
       (fact "it should join matrices along the 1 axis"
             (um/join-matrices 1
                               (m/matrix [[1 2 3]
                                          [1 2 3]
                                          [1 2 3]])
                               (m/matrix [[4 5 6]
                                          [4 5 6]
                                          [4 5 6]]))
             => (m/matrix [[1 2 3 4 5 6]
                           [1 2 3 4 5 6]
                           [1 2 3 4 5 6]]))
       (fact "it should join 3 matrices"
             (um/join-matrices 0
                               (m/matrix [[1 1 1]
                                          [2 2 2]
                                          [3 3 3]])
                               (m/matrix [[4 4 4]
                                          [5 5 5]
                                          [6 6 6]])
                               (m/matrix [[7 7 7]
                                          [8 8 8]
                                          [9 9 9]]))
             => (m/matrix [[1 1 1]
                           [2 2 2]
                           [3 3 3]
                           [4 4 4]
                           [5 5 5]
                           [6 6 6]
                           [7 7 7]
                           [8 8 8]
                           [9 9 9]]))
       (fact "it should handle an empty matrix in the middle"
             (um/join-matrices 0
                               (m/matrix [[1 1 1]
                                          [2 2 2]
                                          [3 3 3]])
                               (m/matrix [])
                               (m/matrix [[4 4 4]
                                          [5 5 5]
                                          [6 6 6]]))
             => (m/matrix [[1 1 1]
                           [2 2 2]
                           [3 3 3]
                           [4 4 4]
                           [5 5 5]
                           [6 6 6]]))
       (fact "it should handle an empty matrix at the start"
             (um/join-matrices 0
                               (m/matrix [])
                               (m/matrix [[1 1 1]
                                          [2 2 2]
                                          [3 3 3]])
                               (m/matrix [[4 4 4]
                                          [5 5 5]
                                          [6 6 6]]))
             => (m/matrix [[1 1 1]
                           [2 2 2]
                           [3 3 3]
                           [4 4 4]
                           [5 5 5]
                           [6 6 6]]))
       (fact "it should handle an empty matrix at the end"
             (um/join-matrices 0
                               (m/matrix [[1 1 1]
                                          [2 2 2]
                                          [3 3 3]])
                               (m/matrix [[4 4 4]
                                          [5 5 5]
                                          [6 6 6]])
                               (m/matrix []))
             => (m/matrix [[1 1 1]
                           [2 2 2]
                           [3 3 3]
                           [4 4 4]
                           [5 5 5]
                           [6 6 6]]))
       (fact "it should handle empty matrices at the start and end"
             (um/join-matrices 0
                               (m/matrix [])
                               (m/matrix [[1 1 1]
                                          [2 2 2]
                                          [3 3 3]])
                               (m/matrix [[4 4 4]
                                          [5 5 5]
                                          [6 6 6]])
                               (m/matrix []))
             => (m/matrix [[1 1 1]
                           [2 2 2]
                           [3 3 3]
                           [4 4 4]
                           [5 5 5]
                           [6 6 6]]))
       (fact "it should handle all empty matrices"
             (um/join-matrices 0
                               (m/matrix [])
                               (m/matrix [])
                               (m/matrix []))
             => (m/matrix []))
       (fact "it should join row-matrices"
             (um/join-matrices 0
                               (m/matrix [[1 1 1]
                                          [2 2 2]
                                          [3 3 3]])
                               (m/row-matrix [4 4 4])
                               (m/matrix [[5 5 5]
                                          [6 6 6]
                                          [7 7 7]]))
             => (m/matrix [[1 1 1]
                           [2 2 2]
                           [3 3 3]
                           [4 4 4]
                           [5 5 5]
                           [6 6 6]
                           [7 7 7]]))
       (fact "it should join col-matrices"
             (um/join-matrices 1
                               (m/matrix [[1 2 3]
                                          [1 2 3]
                                          [1 2 3]])
                               (m/column-matrix [4 4 4])
                               (m/matrix [[5 6 7]
                                          [5 6 7]
                                          [5 6 7]]))
             => (m/matrix [[1 2 3 4 5 6 7]
                           [1 2 3 4 5 6 7]
                           [1 2 3 4 5 6 7]])))

(facts "about join-row-wise"
       (fact "it should join matrices row-wise"
             (um/join-row-wise (m/matrix [[1 1 1]
                                          [2 2 2]
                                          [3 3 3]])
                               (m/matrix [[4 4 4]
                                          [5 5 5]
                                          [6 6 6]]))
             => (m/matrix [[1 1 1]
                           [2 2 2]
                           [3 3 3]
                           [4 4 4]
                           [5 5 5]
                           [6 6 6]])))

(facts "about join-col-wise"
       (fact "it should join matrices col-wise"
             (um/join-col-wise (m/matrix [[1 2 3]
                                          [1 2 3]
                                          [1 2 3]])
                               (m/matrix [[4 5 6]
                                          [4 5 6]
                                          [4 5 6]]))
             => (m/matrix [[1 2 3 4 5 6]
                           [1 2 3 4 5 6]
                           [1 2 3 4 5 6]])))

(facts "about normalise-to-first-row"
       (fact "it should subtract the first row from all rows"
             (um/normalise-to-first-row (m/matrix [[1 4 5]
                                                   [2 8 25]
                                                   [3 12 125]]))
             => (m/matrix [[0 0 0]
                           [1 4 20]
                           [2 8 120]]))
       (fact "it should have no effect if the first row is zero"
             (um/normalise-to-first-row (m/matrix [[0 0 0]
                                                   [2 8 25]
                                                   [3 12 125]]))
             => (m/matrix [[0 0 0]
                           [2 8 25]
                           [3 12 125]])))

(facts "about normalise-to-first-column"
       (fact "it should subtract the first column from all columns"
             (um/normalise-to-first-column (m/matrix [[1 2 3]
                                                      [4 8 12]
                                                      [5 25 125]]))
             => (m/matrix [[0 1 2]
                           [0 4 8]
                           [0 20 120]]))
       (fact "it should have no effect if the first column is zero"
             (um/normalise-to-first-column (m/matrix [[0 2 3]
                                                      [0 8 12]
                                                      [0 25 125]]))
             => (m/matrix [[0 2 3]
                           [0 8 12]
                           [0 25 125]])))

(facts "about remove-first-row"
       (fact "it should remove the first row"
             (um/remove-first-row (m/matrix [[1 1 1]
                                             [2 2 2]
                                             [3 3 3]]))
             => (m/matrix [[2 2 2]
                           [3 3 3]])))

(facts "about remove-first-column"
       (fact "it should remove the first column"
             (um/remove-first-column (m/matrix [[1 2 3]
                                                [1 2 3]
                                                [1 2 3]]))
             => (m/matrix [[2 3]
                           [2 3]
                           [2 3]])))

(facts "about repeat-block-diagonal"
       (fact "it should repeat the given block the given number of times along a diagonal"
             (um/repeat-block-diagonal (m/matrix [[1 2]
                                                  [3 4]]) 3)
             => (m/matrix [[1 2 0 0 0 0]
                           [3 4 0 0 0 0]
                           [0 0 1 2 0 0]
                           [0 0 3 4 0 0]
                           [0 0 0 0 1 2]
                           [0 0 0 0 3 4]]))
       (fact "it should repeat the given block (with more rows than columns) the given number of times along a diagonal"
             (um/repeat-block-diagonal (m/matrix [[1 2]
                                                  [3 4]
                                                  [5 6]]) 3)
             => (m/matrix [[1 2 0 0 0 0]
                           [3 4 0 0 0 0]
                           [5 6 0 0 0 0]
                           [0 0 1 2 0 0]
                           [0 0 3 4 0 0]
                           [0 0 5 6 0 0]
                           [0 0 0 0 1 2]
                           [0 0 0 0 3 4]
                           [0 0 0 0 5 6]]))
       (fact "it should repeat the given block (with more columns than rows) the given number of times along a diagonal"
             (um/repeat-block-diagonal (m/matrix [[1 2 3]
                                                  [4 5 6]]) 3)
             => (m/matrix [[1 2 3 0 0 0 0 0 0]
                           [4 5 6 0 0 0 0 0 0]
                           [0 0 0 1 2 3 0 0 0]
                           [0 0 0 4 5 6 0 0 0]
                           [0 0 0 0 0 0 1 2 3]
                           [0 0 0 0 0 0 4 5 6]])))

(facts "about matrix->nested-double-array"
       (fact "it should return a nested double array"
             (type (um/matrix->nested-double-array (m/matrix [[1 2 3]
                                                              [4 5 6]
                                                              [7 8 9]])))
             => (Class/forName "[[D"))
       (fact "it should return a double-array"
             )
       (fact "it should retain the correct values"
             (->> (m/matrix [[1 2 3]
                             [4 5 6]
                             [7 8 9]])
                  (um/matrix->nested-double-array)
                  (vec)
                  (map vec))
             => [[1.0 2.0 3.0]
                 [4.0 5.0 6.0]
                 [7.0 8.0 9.0]])
       (fact "it should convert a row-matrix into a nested array of doubles"
             (type (um/matrix->nested-double-array (m/row-matrix [1 2 3])))
             => (Class/forName "[[D"))
       (fact "it should convert a column-matrix into a nested array of doubles"
             (type (um/matrix->nested-double-array (m/column-matrix [1 2 3])))
             => (Class/forName "[[D")))
