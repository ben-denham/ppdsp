(ns ppdsp.utils
  (:require [clojure.pprint :refer [cl-format]]
            #?(:clj [clojure.java.io :as io]
               :cljr [clojure.clr.io :as io]))
  (:import [java.util.concurrent Executors]))

(defn get-keys [coll indexes]
  (if (map? coll)
    (map #(get coll %) indexes)
    (map #(nth coll %) indexes)))

(defn index-of [coll item]
  (->> (map-indexed vector coll)
       (filter #(= item (second %)))
       (first)
       (first)))

(defn indexes-where [pred sequence]
  (->> (map-indexed vector sequence)
       (filter #(pred (second %)))
       (map first)))

(defn keys-where [pred hashmap]
  (->> hashmap
       (map vec)
       (filter #(pred (second %)))
       (map first)))

(defn max-fn [func coll]
  (reduce #(if (>= (func %1) (func %2)) %1 %2) coll))

(defn seq-contains? [coll item]
  (some #(= % item) coll))

(defn seq-is-subset? [super-coll sub-coll]
  (every? #(seq-contains? super-coll %) sub-coll))

(defn percent [value]
  (str (cl-format nil "~,2F" (* 100 value)) "%"))

(defn mean [xs]
  (if (empty? xs)
    0
    (/ (apply + xs) (count xs))))

(defn std-dev
  [values]
  (let [mu (mean values)]
    (Math/sqrt (/ (reduce + (map #(Math/pow (- % mu) 2) values))
                  (- (count values) 1)))))

(defn median [xs]
  (let [sorted-xs (sort xs)
        xs-count (count xs)
        ;; quot performs division rounding down to integer.
        mid-index (quot xs-count 2)]
    (if (odd? xs-count)
      (nth sorted-xs mid-index)
      (mean [(nth sorted-xs (dec mid-index))
             (nth sorted-xs mid-index)]))))

(defn weighted-mean
  "Find the weighted mean for a sequence of items, where the value is
  given by value-key and the weight is given by ."
  [value-key weight-key items]
  (let [weights (map weight-key items)
        total-weight (reduce + weights)]
    (as-> items $
      (map #(* (value-key %) (weight-key %)) $)
      (reduce + $)
      (/ $ total-weight))))

(defn rolling-difference
  "Returns a sequence of numerical differences between subsequent
  `values`."
  [values]
  (map - (rest values) (drop-last values)))

(defn safe-division
  ([x y fallback]
   (if (= 0 y) fallback (/ x y)))
  ([x y]
   (safe-division x y 0)))

(defn bool-to-int
  [b]
  (if b 1 0))

(defn standard-deviation [coll]
  (if (<= (count coll) 1)
    0
    (let [avg (mean coll)
          squares (for [x coll]
                    (let [x-avg (- x avg)]
                      (* x-avg x-avg)))
          total (count coll)]
      (-> (/ (apply + squares)
             (- total 1))
          (Math/sqrt)))))

(defn filtered-proportion [pred items]
  (loop [items items total-true 0 total-false 0]
    (if (empty? items)
      (safe-division total-true (+ total-true total-false) 0)
      (if (pred (first items))
        (recur (rest items) (inc total-true) total-false)
        (recur (rest items) total-true (inc total-false))))))

(defn replace-in-seq
  "Replaces all occurrences of a given value in a sequence with a
  replacement value."
  [needle replacement haystack]
  (map #(if (= % needle) replacement %) haystack))

(defn dot-product
  "Computes the dot-product of xs and ys."
  [xs ys]
  (->> (map * xs ys)
       (reduce +)))

(defn map-vals [f m]
  (into {} (map (fn [[k v]] [k (f v)]) m)))

(defn keys->map
  "Returns a map where each key maps to the value given by applying
  val-fn."
  [val-fn keys]
  (zipmap keys (map val-fn keys)))

(def ^:dynamic *format-precision* 5)

(defn format-number [num]
  (let [f (str "%." *format-precision* "f")
        formatted-num (format f (float num))]
    (if (re-matches #"-0\.0+" formatted-num)
      (subs formatted-num 1) ;; Remove negative sign from negative zero
      formatted-num)))

(defn format-number-scientific [num]
  (let [f (str "%." *format-precision* (if (> num 10000) "e" "f"))
        formatted-num (format f (double num))]
    (if (re-matches #"-0\.0+" formatted-num)
      (subs formatted-num 1) ;; Remove negative sign from negative zero
      formatted-num)))

(defn log-time-string
  []
  #?(:clj (.format (java.text.SimpleDateFormat. "yyyyMMdd-HHmm")
                   (new java.util.Date))
     :cljr "todo"))

(defmacro with-file-reader [path sym & body]
  `(with-open [~sym #?(:clj (io/reader ~path)
                       :cljr (io/text-reader ~path))]
     (do ~@body)))

(defn pmap-pool [thread-count func coll]
  (let [pool (Executors/newFixedThreadPool thread-count)
        results (->> coll
                     (map (fn [item]
                            (fn [] (func item))))
                     (.invokeAll pool)
                     (map #(.get %))
                     (doall))]
    (.shutdown pool)
    results))

(defn try-times*
  "Executes thunk. If an exception is thrown, will retry. At most n retries
  are done. If still some exception is thrown it is bubbled upwards in
  the call chain. See: https://stackoverflow.com/a/1879961"
  [n thunk]
  (loop [n n]
    (if-let [result (try [(thunk)]
                         (catch Exception e
                           (when (zero? n)
                             (throw e))))]
      (result 0)
      (recur (dec n)))))

(defmacro try-times
  "Executes body. If an exception is thrown, will retry. At most n retries
  are done. If still some exception is thrown it is bubbled upwards in
  the call chain. See: https://stackoverflow.com/a/1879961"
  [n & body]
  `(try-times* ~n (fn [] ~@body)))

(let [printing-lock (Object.)]
  (defn sync-println [& args]
    (locking printing-lock (apply println args))))

(def ^:dynamic *debug* true)

(defmacro debug [& body]
  `(when *debug*
     (do ~@body)))
