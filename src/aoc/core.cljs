(ns aoc.core
  (:require ["fs" :as fs]))

(defn read-input [day]
  (fs/readFileSync (str "./" day ".txt") "utf-8"))

(defn ->lines [input]
  (filter js/Boolean (-> input (.split "\n"))))

(defn ->int [n]
  (js/parseInt n 10))

(defn ->fuel [n]
 (- (.floor js/Math (/ n 3)) 2))

(defn ->fuel-all [n]
  (loop [fuel (->fuel n)
         sum 0]
    (if (neg? fuel)
      sum
      (recur
        (->fuel fuel)
        (+ sum fuel)))))

(defn d01a [input]
  (->> input
       (->lines)
       (map ->int)
       (map ->fuel)
       (reduce #(+ %1 %2) 0)))

(defn d01b [input]
  (->> input
       (->lines)
       (map ->int)
       (map ->fuel-all)
       (reduce #(+ %1 %2) 0)))

(defn -main [& args]
  (let [days {:d01a d01a
              :d01b d01b}
        day (keyword (str (first args) (second args)))
        input (read-input (first args))]
    (println ((day days) input))))
