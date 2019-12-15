(ns aoc.d13
  (:require [aoc.intcode :refer [->intcode run-intcode]]))

(defn a [input]
  (->> (->intcode input)
       run-intcode
       :output
       (partition 3)
       (filter #(= 2 (nth % 0)))
       count))
