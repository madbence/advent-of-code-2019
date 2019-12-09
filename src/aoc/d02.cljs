(ns aoc.d02
  (:require [aoc.intcode :refer [->intcode run-intcode]]))

(defn intcode-result [noun verb intcode]
  (-> intcode
      (assoc-in [:memory 1] noun)
      (assoc-in [:memory 2] verb)
      run-intcode
      :memory
      (get 0)))

(defn find-intcode [target intcode]
  (->> (for [noun (range 0 100)
             verb (range 0 100)
             :when (= target (intcode-result noun verb intcode))]
         (+ (* noun 100) verb))
       first))

(defn a [input]
  (->> (->intcode input)
       (intcode-result 12 2)))


(defn b [input]
  (->> (->intcode input)
       (find-intcode 19690720)))
