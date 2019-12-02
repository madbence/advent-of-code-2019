(ns aoc.d01
  (:require [aoc.utils :refer [->lines ->int]]))

(defn ->fuel [n]
 (- (.floor js/Math (/ n 3)) 2))

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
       (reduce #(+ %1 %2) 0)))

(defn b [input]
  (->> input
       (->lines)
       (map ->int)
       (map ->fuel-all)
       (reduce #(+ %1 %2) 0)))
