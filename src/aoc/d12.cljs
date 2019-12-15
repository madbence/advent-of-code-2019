(ns aoc.d12
  (:require [aoc.utils :refer [->int lcm]]))

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

(defn slice-axis [moons n]
  (map #(vector (nth (first %) n) (nth (second %) n)) moons))

(defn find-repeated-state-on [n state]
  (->> (iterate step-moons state)
       rest
       (take-while (fn [state'] (not= (slice-axis state' n) (slice-axis state n))))
       count
       inc))

(defn find-repeated-state [state]
  (lcm
    (find-repeated-state-on 0 state)
    (lcm
      (find-repeated-state-on 1 state)
      (find-repeated-state-on 2 state))))

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

(defn b [input]
  (->> (.split input "\n")
       drop-last
       (map parse-moon)
       (map #(vector % [0 0 0]))
       find-repeated-state))
