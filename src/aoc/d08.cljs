(ns aoc.d08
  (:require [aoc.utils :refer [->int]]))

(defn fewer-zeros [a b]
  (if (< (-> (filter zero? a) count) (-> (filter zero? b) count)) a b))

(defn checksum [layer]
  (* (-> (filter #(= 1 %) layer) count) (-> (filter #(= 2 %) layer) count)))

(defn a [input]
  (->> (drop-last input)
       (map ->int)
       (partition (* 6 25))
       (reduce fewer-zeros)
       checksum))
