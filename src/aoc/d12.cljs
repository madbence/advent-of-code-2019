(ns aoc.d12
  (:require [aoc.utils :refer [->int]]))

(defn parse-moon [line]
  (->> (.match line #"<x=(-?\d+), y=(-?\d+), z=(-?\d+)>")
       rest
       (map ->int)
       vec))

(defn apply-gravity [v p ps]
  (- (+ v (-> (filter #(> % p) ps) count))
     (-> (filter #(< % p) ps) count)))

(defn step-moons [moons]
  (let [[xs ys zs] (apply map vector (map first moons))]
    (map (fn [[[x y z] [vx vy vz]]]
           (let [vx' (apply-gravity vx x xs)
                 vy' (apply-gravity vy y ys)
                 vz' (apply-gravity vz z zs)]
             [[(+ x vx')
               (+ y vy')
               (+ z vz')]
              [vx' vy' vz']])) moons)))

(defn abs [n]
  (if (neg? n) (- n) n))

(defn total-energy [moon]
  (* (->> (first moon) (map abs) (reduce +))
     (->> (second moon) (map abs) (reduce +))))

(defn a [input]
  (->> (.split input "\n")
       drop-last
       (map parse-moon)
       (map #(vector % [0 0 0]))
       (iterate step-moons)
       (drop 1000)
       first
       (map total-energy)
       (reduce +)))
