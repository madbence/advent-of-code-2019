(ns aoc.core
  (:require ["fs" :as fs]
            [aoc.d01 :as d01]
            [aoc.d02 :as d02]
            [aoc.d03 :as d03]
            [aoc.d04 :as d04]
            [aoc.d05 :as d05]
            [aoc.d06 :as d06]
            [aoc.d07 :as d07]
            [aoc.d08 :as d08]
            [aoc.d09 :as d09]
            [aoc.d10 :as d10]
            [aoc.d11 :as d11]
            [aoc.d12 :as d12]
            [aoc.d13 :as d13]
            [aoc.d15 :as d15]
            [aoc.utils :refer [read-input]]))

(defn -main [& args]
  (let [days {:d01a d01/a
              :d01b d01/b
              :d02a d02/a
              :d02b d02/b
              :d03a d03/a
              :d03b d03/b
              :d04a d04/a
              :d04b d04/b
              :d05a d05/a
              :d05b d05/b
              :d06a d06/a
              :d06b d06/b
              :d07a d07/a
              :d07b d07/b
              :d08a d08/a
              :d08b d08/b
              :d09a d09/a
              :d09b d09/b
              :d10a d10/a
              :d11a d11/a
              :d11b d11/b
              :d12a d12/a
              :d12b d12/b
              :d13a d13/a
              :d13b d13/b
              :d15a d15/a
              :d15b d15/b}
        day (keyword (str (first args) (second args)))
        input (read-input (first args))]
    (println ((day days) input))))
