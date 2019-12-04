(ns aoc.d04
  (:require [aoc.utils :refer [->int]]))

(defn is-password? [n]
  (let [pass (into [] (str n))
        pairs (map vector pass (rest pass))]
    (and (some #(= (first %) (second %)) pairs)
         (every? #(>= (second %) (first %)) pairs))))

(defn a [input]
  (let [[from to] (map ->int (.split input "-"))]
    (->> (range from to)
         (filter is-password?)
         count)))
