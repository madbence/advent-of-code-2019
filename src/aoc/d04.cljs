(ns aoc.d04
  (:require [aoc.utils :refer [->int]]))

(defn is-password? [n]
  (let [pass (into [] (str n))
        pairs (map vector pass (rest pass))]
    (and (some #(= (first %) (second %)) pairs)
         (every? #(>= (second %) (first %)) pairs))))

(defn is-password-second? [n]
  (let [pass (into [] (str n))
        pairs (map vector pass (rest pass))
        quads (map vector pass (rest pass) (nthrest pass 2) (nthrest pass 3))]
    (and (some #(= (first %) (second %)) pairs)
         (every? #(>= (second %) (first %)) pairs)
         (or (and (= (nth pass 0) (nth pass 1))
                  (not= (nth pass 1) (nth pass 2)))
             (and (= (nth pass 4) (nth pass 5))
                  (not= (nth pass 3) (nth pass 4)))
             (some #(and (= (nth % 1) (nth % 2))
                         (not= (nth % 0) (nth % 1))
                         (not= (nth % 2) (nth % 3))) quads)))))

(defn a [input]
  (let [[from to] (map ->int (.split input "-"))]
    (->> (range from to)
         (filter is-password?)
         count)))

(defn b [input]
  (let [[from to] (map ->int (.split input "-"))]
    (->> (range from to)
         (filter is-password-second?)
         count)))
