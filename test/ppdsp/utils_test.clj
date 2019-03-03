(ns ppdsp.utils-test
  (:use midje.sweet)
  (:require
   [ppdsp.utils :as utils]))

(facts "about std-dev"
       (fact "it should return the correct sample standard deviation result for a known sample (https://en.wikipedia.org/wiki/Standard_deviation)"
             (format "%.2f" (utils/std-dev [727.7 1086.5 1091.0 1361.3 1490.5 1956.1]))
             => "420.96"))
