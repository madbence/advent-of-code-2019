(ns aoc.d08
  (:require [aoc.utils :refer [->int]]))

(defn fewer-zeros [a b]
  (if (< (-> (filter zero? a) count) (-> (filter zero? b) count)) a b))

(defn checksum [layer]
  (* (-> (filter #(= 1 %) layer) count) (-> (filter #(= 2 %) layer) count)))

(defn merge-layers [a b]
  (map #(if (= %2 2) %1 %2) a b))

(defn dump-image [image]
  (->> (map #(if (zero? %) " " ".") image)
       (partition 25)
       (map #(apply str %))
       (reduce #(str %1 "\n" %2))))

(defn a [input]
  (->> (drop-last input)
       (map ->int)
       (partition (* 6 25))
       (reduce fewer-zeros)
       checksum))

(defn b [input]
  (->> (drop-last input)
       (map ->int)
       (partition (* 6 25))
       reverse
       (reduce merge-layers)
       dump-image))
