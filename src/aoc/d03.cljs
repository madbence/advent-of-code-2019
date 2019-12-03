(ns aoc.d03
  (:require [aoc.utils :refer [->lines]]
            [clojure.set :refer [intersection]]))

(def ops {:u [:y +]
          :d [:y -]
          :r [:x +]
          :l [:x -]})

(defn ->segment [move pos]
  (->> (range 1 (inc (:count move)))
       (map (fn [n]
              (let [[axis op] ((:dir move) ops)]
                (assoc pos axis (op (axis pos) n)))))))

(defn ->path [moves]
  (loop [path [{:x 0 :y 0}]
         moves moves]
    (if (empty? moves)
      (rest path)
      (let [curr (first moves)
            pos (last path)
            segment (->segment curr pos)
            tail (rest moves)]
        (recur (into path segment)
               tail)))))

(defn distance [point]
  (+ (.abs js/Math (:x point)) (.abs js/Math (:y point))))

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
       (map #(into #{} %))
       (reduce intersection)
       (map distance)
       (reduce min)))
