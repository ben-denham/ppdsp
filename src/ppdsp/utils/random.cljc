(ns ppdsp.utils.random)

(defprotocol RandomNumberGenerator
  (next-int! [this ceiling]
    "Returns the next random integer between 0 (inclusive) and
    ceiling (exclusive).")
  (next-float! [this]
    "Returns the next random float between 0.0 and 1.0.")
  (next-double! [this]
    "Returns the next random double between 0.0 and 1.0.")
  (next-gauss! [this mu sigma]
    "Returns the next random float from a Gaussian distribution with
    given mean (mu) and standard deviation (sigma).")
  (next-derived-seed! [this]
    "Returns a new seed to provide to a derived random number
    generator."))

(defprotocol RandomShuffler
  (shuffle! [this coll]
    "Returns a shuffled vector representation of the collection."))

(defn seeded-rng [seed]
  #?(:clj
     (let [rng (java.util.Random. seed)]
       (reify RandomNumberGenerator
         (next-int! [this ceiling]
           (.nextInt rng ceiling))
         (next-float! [this]
           (.nextFloat rng))
         (next-double! [this]
           (.nextDouble rng))
         (next-gauss! [this mu sigma]
           (+ mu (* sigma (.nextGaussian rng))))
         (next-derived-seed! [this]
           (.nextLong rng))))
     :cljr nil))

(defn seeded-shuffler
  "See: https://stackoverflow.com/a/32452825"
  [seed]
  #?(:clj
     (let [rng (java.util.Random. seed)]
       (reify
         RandomShuffler
         (shuffle! [this coll]
           (let [al (java.util.ArrayList. coll)]
             (java.util.Collections/shuffle al rng)
             (clojure.lang.RT/vector (.toArray al))))))
     :cljr nil))

(defn seeded-shuffle
  [coll seed]
  (shuffle! (seeded-shuffler seed) coll))

(defn random-choice! [rng values]
  (->> (count values)
       (next-int! rng)
       (nth values)))

(defn uniform-random! [rng min max]
  (+ min (* (- max min) (next-double! rng))))
