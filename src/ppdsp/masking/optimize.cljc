(ns ppdsp.masking.optimize
  (:import [org.apache.commons.math3.analysis
            MultivariateFunction]
           [org.apache.commons.math3.optim
            InitialGuess MaxEval SimpleValueChecker OptimizationData]
           [org.apache.commons.math3.optim.nonlinear.scalar
            GoalType ObjectiveFunction]
           [org.apache.commons.math3.optim.nonlinear.scalar.noderiv
            NelderMeadSimplex SimplexOptimizer]
           [org.apache.commons.math3.exception
            TooManyEvaluationsException]))

(defn nelder-mead-optimize
  "Some configuration set based on:
  https://docs.scipy.org/doc/scipy/reference/optimize.minimize-neldermead.html"
  [objective-fn initial-guess maximize?
   & {:keys [max-evaluations relative-threshold]}]
  (let [variable-count (count initial-guess)
        ;; Set default relative threshold to the same as scipy.
        relative-threshold (or relative-threshold 0.0001)
        ;; Set default maximum evaluations to the same as scipy.
        max-evaluations (or max-evaluations (* 200 variable-count))
        result-atom (atom {:optimum nil
                           :score (if maximize?
                                    Double/NEGATIVE_INFINITY
                                    Double/POSITIVE_INFINITY)
                           :evaluations 0})
        objective (reify MultivariateFunction
                    (value [this vars]
                      (let [new-score (double (objective-fn (vec vars)))]
                        ;; Keep track of the number of evaluations and
                        ;; the optimum vars and score, because we will
                        ;; need to fall back to these if an exception
                        ;; is thrown for reaching maximum evaluations.
                        (swap! result-atom
                               (fn [{:keys [optimum score evaluations]}]
                                 {:optimum (if (or (and maximize? (> new-score score))
                                                   (and (not maximize?) (< new-score score)))
                                             (vec vars)
                                             optimum)
                                  :score (if maximize?
                                           (max new-score score)
                                           (min new-score score))
                                  :evaluations (inc evaluations)}))
                        new-score)))
        ;; Set absolute threshold of zero, given probabilities range
        ;; to very small values.
        optimizer (SimplexOptimizer. (SimpleValueChecker. relative-threshold 0))
        params [(MaxEval. max-evaluations)
                (ObjectiveFunction. objective)
                (if maximize? GoalType/MAXIMIZE GoalType/MINIMIZE)
                (InitialGuess. (double-array initial-guess))
                (NelderMeadSimplex. variable-count)]
        ;; Call variadic method with an array of values.
        optimum (try (.optimize optimizer (into-array OptimizationData params))
                     (catch TooManyEvaluationsException ex nil))]
    (if optimum
     {:optimum (vec (.getFirst optimum))
      :score (.getSecond optimum)
      :evaluations (.getEvaluations optimizer)}
     (assoc @result-atom :max-evaluations-reached true))))
