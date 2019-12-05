(ns aoc.d05
  (:require [aoc.utils :refer [->int]]))

(defn nth-digit [n i]
  (-> (iterate #(.floor js/Math (/ % 10)) n) (nth i) (mod 10)))

(defn get-param [{code :code ip :ip} n]
  (nth code (+ ip n)))

(defn get-value [{code :code ip :ip} n]
  (let [param (nth code (+ ip n))
        mode (nth-digit (nth code ip) (+ n 1))]
    (if (= mode 1)
      param
      (nth code param))))

(defn add [state]
  (-> state
      (assoc-in [:code (get-param state 3)] (+ (get-value state 1) (get-value state 2)))
      (update :ip + 4)))

(defn mul [state]
  (-> state
      (assoc-in [:code (get-param state 3)] (* (get-value state 1) (get-value state 2)))
      (update :ip + 4)))

(defn in [state]
  (-> state
      (assoc-in [:code (get-param state 1)] (first (:input state)))
      (update :ip + 2)
      (update :input rest)))

(defn out [state]
  (-> state
      (update :ip + 2)
      (update :output #(cons %2 %1) (get-value state 1))))

(defn jump-if-true [state]
  (-> state
      (assoc :ip (if (> (get-value state 1) 0) (get-value state 2) (+ (:ip state) 3)))))

(defn jump-if-false [state]
  (-> state
      (assoc :ip (if (= (get-value state 1) 0) (get-value state 2) (+ (:ip state) 3)))))

(defn less-than [state]
  (-> state
      (update :ip + 4)
      (assoc-in [:code (get-param state 3)] (if (< (get-value state 1) (get-value state 2)) 1 0))))

(defn equals [state]
  (-> state
      (update :ip + 4)
      (assoc-in [:code (get-param state 3)] (if (= (get-value state 1) (get-value state 2)) 1 0))))

(def ops {1 add
          2 mul
          3 in
          4 out
          5 jump-if-true
          6 jump-if-false
          7 less-than
          8 equals})

(defn step-intcode [state]
  (let [instruction (mod (nth (:code state) (:ip state)) 100)
        op (get ops instruction)]
    (if op (op state) nil)))

(defn run-intcode [in code]
  (loop [state {:input in
                :output '()
                :code code
                :ip 0}]
    (let [state' (step-intcode state)]
      (if (nil? state')
        state
        (recur state')))))

(defn ->intcode [input]
  (->> (.split input ",") (map ->int) (into [])))

(defn a [input]
  (->> (->intcode input)
       (run-intcode '(1))
       :output
       first))

(defn b [input]
  (->> (->intcode input)
       (run-intcode '(5))
       :output
       first))
