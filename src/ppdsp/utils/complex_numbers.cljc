(ns ppdsp.utils.complex-numbers
  (:require [ppdsp.utils :refer [format-number]]))

(defn complex [x y]
  [x y])

(defn *c
  ([[x1 y1] [x2 y2]]
   [(- (* x1 x2) (* y1 y2))
    (+ (* x1 y2) (* y1 x2))])
  ([x y & more]
   (apply *c (conj more (*c x y)))))

(defn +c
  ([[x1 y1] [x2 y2]]
   [(+ x1 x2) (+ y1 y2)])
  ([x y & more]
   (apply +c (conj more (+c x y)))))

(defn -c
  ([[x1 y1] [x2 y2]]
   [(- x1 x2) (- y1 y2)])
  ([x y & more]
   (apply -c (conj more (-c x y)))))

(defn c-conjugate [[x y]]
  [x (- y)])

(defn c-zero? [c]
  (every? zero? c))

(defn c-near-zero? [c]
  (->> (map format-number c)
       (every? #(re-matches #"0\.0+" %))))

(defn format-complex-number [c-num]
  (let [[real imag] (map format-number c-num)]
    (str real
         (when (not (re-matches #"0\.0+" imag))
           (str " + " imag "i")))))
