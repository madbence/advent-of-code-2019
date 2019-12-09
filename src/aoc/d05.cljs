(ns aoc.d05
  (:require [aoc.intcode :refer [->intcode run-intcode]]))

(defn a [input]
  (-> (->intcode input)
      (update :input conj 1)
      run-intcode
      :output
      first))

(defn b [input]
  (-> (->intcode input)
      (update :input conj 5)
      run-intcode
      :output
      first))
