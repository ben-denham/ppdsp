(ns ppdsp.utils.moving-average)

(defprotocol MovingAverage
  (update-ma [this value]
    "Return a new moving average based on the current state and the
    new value in the series.")
  (get-current-ma [this]
    "Returns the current value of the moving average"))

;; Exponentially Weighted Moving Average

(defrecord EWMA [alpha current-ma]
  MovingAverage
  (update-ma [this value]
    (let [new-ma
          (+ (* alpha value)
             (* (- 1 alpha) current-ma))]
      (assoc this :current-ma new-ma)))
  (get-current-ma [this]
    current-ma))

(defn make-ewma
  "See: https://en.wikipedia.org/wiki/Moving_average#Exponential_moving_average"
  [alpha initial-ma]
  (EWMA. alpha initial-ma))

(defn make-ewma-for-window
  "See: https://en.wikipedia.org/wiki/Moving_average#Relationship_between_SMA_and_EMA"
  [window-size initial-ma]
  (make-ewma (/ 2 (+ window-size 1)) initial-ma))
