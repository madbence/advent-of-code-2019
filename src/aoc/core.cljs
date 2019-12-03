(ns aoc.core
  (:require ["fs" :as fs]
            [aoc.d01 :as d01]
            [aoc.d02 :as d02]
            [aoc.d03 :as d03]
            [aoc.utils :refer [read-input]]))

(defn -main [& args]
  (let [days {:d01a d01/a
              :d01b d01/b
              :d02a d02/a
              :d02b d02/b
              :d03a d03/a
              :d03b d03/b}
        day (keyword (str (first args) (second args)))
        input (read-input (first args))]
    (println ((day days) input))))
