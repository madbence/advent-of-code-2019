(ns aoc.d01
  (:require [aoc.utils :refer [->lines ->int]]))

(defn ->fuel [n]
  (as-> n $ (/ $ 3) (.floor js/Math $) (- $ 2)))

(defn ->fuel-all [n]
  (loop [fuel (->fuel n)
         sum 0]
    (if (neg? fuel)
      sum
      (recur
        (->fuel fuel)
        (+ sum fuel)))))

(defn a [input]
  (->> input
       (->lines)
       (map ->int)
       (map ->fuel)
       (reduce +)))

(defn b [input]
  (->> input
       (->lines)
       (map ->int)
       (map ->fuel-all)
       (reduce +)))
