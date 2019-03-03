(ns ppdsp.masking.projection-test
  (:use midje.sweet)
  (:require
   [ppdsp.masking.projection :as p]
   [ppdsp.utils :refer [std-dev]]
   [clojure.core.matrix :as m])
  (:import [ppdsp.dataset.base StaticDataset]))

(facts "about random-gauss-matrix"
       (let [row-count 1000
             col-count 2000
             mu 11
             sigma 7
             matrix (p/random-gauss-matrix [row-count col-count] mu sigma)]
         (fact "it should return a matrix with the correct number of rows"
               (m/row-count matrix) => row-count)
         (fact "it should return a matrix with the correct number of columns"
               (m/column-count matrix) => col-count)
         (fact "it should return a matrix with the correct mean"
               (Math/round (/ (m/esum matrix) (* row-count col-count))) => mu)
         (fact "it should return a matrix with the correct standard deviation"
               (Math/round (std-dev (m/eseq matrix))) => sigma)))

(facts "about random-orthogonal-matrix"
       (let [matrix (p/random-orthogonal-matrix 11)]
         (fact "it should return an orthogonal matrix"
               (m/orthogonal? matrix) => true)
         (fact "it should return a matrix with the correct size"
               (m/shape matrix) => [11 11])))
