(ns aoc.utils
  (:require ["fs" :as fs]))

(defn read-input [day]
  (fs/readFileSync (str "./" day ".txt") "utf-8"))

(defn ->lines [input]
  (filter js/Boolean (-> input (.split "\n"))))

(defn ->int [n]
  (js/parseInt n 10))
