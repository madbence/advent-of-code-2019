(ns aoc.d05
  (:require [aoc.utils :refer [->int]]))

(defn nth-digit [n i]
  (-> (iterate #(.floor js/Math (/ % 10)) n)
      (nth i)
      (mod 10)))

(defn decode-param [pos code instruction i]
  (let [param (nth code (+ pos i 1))
        mode (nth-digit instruction (+ i 2))]
    (case mode
      0 {:mode :position :value param}
      1 {:mode :immediate :value param})))

(defn get-value [code ip n]
  (let [param (nth code (+ ip n))
        mode (nth-digit (nth code ip) (+ n 1))]
    (if (= mode 1)
      param
      (nth code param))))

(def ops {99 (fn [out code ip] [out nil ip])
           1 (fn [out code ip] [out (assoc code (nth code (+ ip 3)) (+ (get-value code ip 1) (get-value code ip 2))) (+ ip 4)])
           2 (fn [out code ip] [out (assoc code (nth code (+ ip 3)) (* (get-value code ip 1) (get-value code ip 2))) (+ ip 4)])
           3 (fn [out code ip] [out (assoc code (nth code (+ ip 1)) 1) (+ ip 2)])
           4 (fn [out code ip] [(cons (get-value code ip 1) out) code (+ ip 2)])})

(defn step-intcode [output code ip]
  ((get ops (mod (nth code ip) 100)) output code ip))

(defn run-intcode [code]
  (loop [out '()
         code code
         ip 0]
    (let [[out' code' ip'] (step-intcode out code ip)]
      (if (nil? code')
        [out' code ip']
        (recur out' code' ip')))))

(defn a [input]
  (->> (-> input (.split ","))
       (map ->int)
       (into [])
       run-intcode
       first
       first))
