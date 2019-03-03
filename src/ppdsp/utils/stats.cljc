(ns ppdsp.utils.stats
  (:import [org.apache.commons.math3.stat.ranking
            NaturalRanking NaNStrategy TiesStrategy]
           [org.apache.commons.math3.distribution
            NormalDistribution]
           [org.apache.commons.math3.stat.inference
            MannWhitneyUTest WilcoxonSignedRankTest]))

(defn- mann-whitney-p-value
  [u-min u-mean u-std-dev]
  (let [z (/ (- u-min u-mean) u-std-dev)
        standard-normal (NormalDistribution. 0 1)]
    (* 2 (.cumulativeProbability standard-normal z))))

(defn- places=
  [places x y]
  (let [format-str (str "%." places "f")]
    (= (format format-str x)
       (format format-str y))))

(defn mann-whitney-u-test
  "Performs the Mann-Whitney U Test for independent non-empty samples xs
  and ys."
  [xs ys]
  (let [zs (concat xs ys)
        ranking (NaturalRanking. NaNStrategy/FIXED TiesStrategy/AVERAGE)
        ranks (.rank ranking (double-array zs))
        n1 (count xs)
        n2 (count ys)
        n-sum (+ n1 n2)
        n-prod (* n1 n2)
        x-ranks (take n1 ranks)
        u1 (- (reduce + x-ranks)
              (/ (* n1 (inc n1)) 2))
        u2 (- n-prod u1)
        u-max (max u1 u2)
        u-min (min u1 u2)
        u-mean (/ n-prod 2.0)
        u-std-dev (Math/sqrt (* (/ n-prod 12.0)
                                (inc n-sum)))
        tie-correction (->> (vals (frequencies ranks))
                            (map #(/ (- (Math/pow % 3) %)
                                     12.0))
                            (reduce +))
        u-std-dev-tie-corrected (Math/sqrt (* (/ n-prod (* n-sum (dec n-sum)))
                                              (- (/ (- (Math/pow n-sum 3) n-sum)
                                                    12.0)
                                                 tie-correction)))
        tie-correction-wiki (->> (vals (frequencies ranks))
                                 (map #(/ (- (Math/pow % 3) %)
                                          (* n-sum (dec n-sum))))
                                 (reduce +))
        u-std-dev-tie-corrected-wiki (Math/sqrt (* (/ n-prod 12.0)
                                              (- (inc n-sum)
                                                 tie-correction-wiki)))
        d-xs (double-array xs)
        d-ys (double-array ys)
        results
        {;; u and lib-u are calculated the same way, and should be the
         ;; same.
         :u u-max
         :lib-u (.mannWhitneyU (MannWhitneyUTest.) d-xs d-ys)
         :p (mann-whitney-p-value u-min u-mean u-std-dev)
         ;; p and lib-p are calculated the same way, and should be the
         ;; same.
         :lib-p (.mannWhitneyUTest (MannWhitneyUTest.) d-xs d-ys)
         ;; p-tie-corrected uses a different std-dev to account for
         ;; ties, as described in:
         ;; http://www.lboro.ac.uk/media/wwwlboroacuk/content/mlsc/downloads/2.3_mann_whitney.pdf
         :p-tie-corrected (mann-whitney-p-value u-min u-mean
                                                u-std-dev-tie-corrected)
         ;; p-tie-corrected-wiki uses a different std-dev to account
         ;; for ties, as described in:
         ;; https://en.wikipedia.org/wiki/Mann%E2%80%93Whitney_U_test#Normal_approximation_and_tie_correction
         :p-tie-corrected-wiki (mann-whitney-p-value u-min u-mean
                                                     u-std-dev-tie-corrected-wiki)}]
    (if (not (places= 10 (:u results) (:lib-u results)))
      (throw (Exception. (str "Clojure U and Apache Commons U were not the same: "
                              (:u results) " / " (:lib-u results)))))
    (if (not (places= 10 (:p results) (:lib-p results)))
      (throw (Exception. (str "Clojure P and Apache Commons P were not the same: "
                              (:p results) " / " (:lib-p results)))))
    (if (not= (places= 10 (:p-tie-corrected results) (:p-tie-corrected-wiki results)))
      (throw (Exception. (str "Tie corrected P values were not the same: "
                              (:p-tie-corrected results) " / "
                              (:p-tie-corrected-wiki results)))))
    (select-keys results [:u :p :p-tie-corrected])))

(defn wilcoxon-signed-rank-test
  [xs ys]
  (let [d-xs (double-array xs)
        d-ys (double-array ys)]
    {:signed-rank (.wilcoxonSignedRank (WilcoxonSignedRankTest.) d-xs d-ys)
     :signed-rank-p (.wilcoxonSignedRankTest (WilcoxonSignedRankTest.) d-xs d-ys false)}))
