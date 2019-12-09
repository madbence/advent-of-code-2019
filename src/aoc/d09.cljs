(ns aoc.d09
  (:require [aoc.utils :refer [->int]]))

(defn nth-digit [n i]
  (-> (iterate #(.floor js/Math (/ % 10)) n) (nth i) (mod 10)))

(defn get-param [{code :code ip :ip base :base} n]
  (let [mode (nth-digit (get code ip 0) (+ n 1))]
    (case mode
      0 (get code (+ ip n) 0)
      2 (+ base (get code (+ ip n))) 0)))

(defn get-value [{code :code ip :ip base :base} n]
  (let [param (get code (+ ip n) 0)
        mode (nth-digit (get code ip 0) (+ n 1))]
    (case mode
      2 (get code (+ base param) 0)
      1 param
      0 (get code param 0))))

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

(defn set-base [state]
  (-> state
      (update :ip + 2)
      (update :base + (get-value state 1))))

(def ops {1 add
          2 mul
          3 in
          4 out
          5 jump-if-true
          6 jump-if-false
          7 less-than
          8 equals
          9 set-base})

(defn step-intcode [state]
  (let [instruction (mod (get (:code state) (:ip state) 0) 100)
        op (get ops instruction)]
    (if op (op state) nil)))

(defn run-intcode [in state]
  (loop [state (assoc state :input in)]
    (let [state' (step-intcode state)]
      (if (nil? state')
        state
        (recur state')))))

(defn init-intcode [code]
  {:input '()
   :output '()
   :code code
   :ip 0
   :base 0})

(defn ->intcode [input]
  (->> (.split input ",") (map ->int) (map vector (iterate inc 0)) (into {})))

(defn a [input]
  (->> (->intcode input)
       init-intcode
       (run-intcode '(1))
       :output))
