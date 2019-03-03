(ns ppdsp.utils.timing
  (:import [java.lang.management ManagementFactory]))

;; Tip: use (merge-with op time1 time2) to apply op to the components
;; of time1 and time2, e.g. (merge-with - stop-time start-time) to get
;; time difference.

(def zero-time {:cpu-nano 0
                :wall-nano 0})

(defn get-current-thread-time! []
  "Returns a map with the current wall-clock and thread CPU times in
  nanoseconds."
  (let [thread-bean (ManagementFactory/getThreadMXBean)
        wall-nanoseconds (System/nanoTime)
        cpu-nanoseconds (.getCurrentThreadCpuTime thread-bean)]
    {:cpu-nano cpu-nanoseconds
     :wall-nano wall-nanoseconds}))

(defn max-time
  "Return the longest of the two given times (according to CPU time)."
  [time-a time-b]
  (if (> (:cpu-nano time-a) (:cpu-nano time-b))
    time-a
    time-b))

(defn nano-to-seconds
  [nano]
  (/ nano 1000000000))
