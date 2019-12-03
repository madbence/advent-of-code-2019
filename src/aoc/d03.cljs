(ns aoc.d03
  (:require [aoc.utils :refer [->lines]]
            [clojure.set :refer [intersection]]))

(defn ->segment [move pos]
  (case (:dir move)
    :u (->> (range 1 (inc (:count move)))
            (map #(assoc pos :y (+ (:y pos) %))))
    :d (->> (range 1 (inc (:count move)))
            (map #(assoc pos :y (- (:y pos) %))))
    :l (->> (range 1 (inc (:count move)))
            (map #(assoc pos :x (- (:x pos) %))))
    :r (->> (range 1 (inc (:count move)))
            (map #(assoc pos :x (+ (:x pos) %))))))

(defn ->path [moves]
  (loop [path #{}
         pos {:x 0 :y 0}
         moves moves]
    (if (empty? moves)
      path
      (let [curr (first moves)
            segment (->segment curr pos)
            tail (rest moves)]
        (recur (into path segment)
               (last segment)
               tail)))))

(defn distance [point]
  (+ (.abs js/Math (:x point)) (.abs js/Math (:y point))))

(defn nearest [points]
  (->> points
       (map distance)
       (reduce min)))

(defn parse-move [move]
  (let [match (.match move #"(.)(\d+)")]
    {:dir (-> match (get 1) .toLowerCase keyword)
     :count (int (get match 2))}))

(defn parse-line [line]
  (as-> line $
        (.split $ ",")
        (map parse-move $)))

(defn a [input]
  (->> input
       ->lines
       (map parse-line)
       (map ->path)
       (reduce intersection)
       nearest))
