(ns aoc.utils
  (:require ["fs" :as fs]))

(defn read-input [day]
  (fs/readFileSync (str "./" day ".txt") "utf-8"))

(defn ->lines [input]
  (filter js/Boolean (-> input (.split "\n"))))

(defn ->int [n]
  (js/parseInt n 10))

(defn gcd [a b]
  (loop [a a
         b b]
    (if (zero? b) a
      (recur b (mod a b)))))

(defn lcm [a b]
  (/ (* a b) (gcd a b)))
