(ns aoc.intcode
  (:require [aoc.utils :refer [->int]]))

(defn ->intcode [source]
  {:memory (->> (.split source ",") (map ->int) (map vector (iterate inc 0)) (into {}))
   :ip 0
   :input '()
   :output '()})

(defn nth-digit [n i]
  (-> (iterate #(.floor js/Math (/ % 10)) n) (nth i) (mod 10)))

(defn read-memory [state address]
  (get (:memory state) address 0))

(defn write-memory [state address value]
  (assoc-in state [:memory address] value))

(defn get-param-mode [state n]
  (-> (read-memory state (:ip state)) (nth-digit (+ n 2))))

(defn get-param [state n]
  (case (get-param-mode state n)
    0 (read-memory state (+ (:ip state) 1 n))))

(defn get-value [state n]
  (case (get-param-mode state n)
    0 (read-memory state (read-memory state (+ (:ip state) 1 n)))
    1 (read-memory state (+ (:ip state) 1 n))))

(defn step-intcode [state]
  (case (mod (get (:memory state) (:ip state)) 100)
    1 (-> state
          (update :ip + 4)
          (write-memory (get-param state 2) (+ (get-value state 0) (get-value state 1))))
    2 (-> state
          (update :ip + 4)
          (write-memory (get-param state 2) (* (get-value state 0) (get-value state 1))))
    3 (-> state
          (update :ip + 2)
          (write-memory (get-param state 0) (first (:input state)))
          (update :input rest))
    4 (-> state
          (update :ip + 2)
          (update :output conj (get-value state 0)))
    5 (-> state
          (assoc :ip (if (zero? (get-value state 0)) (+ (:ip state) 3) (get-value state 1))))
    6 (-> state
          (assoc :ip (if (zero? (get-value state 0)) (get-value state 1) (+ (:ip state) 3))))
    7 (-> state
          (update :ip + 4)
          (write-memory (get-param state 2) (if (< (get-value state 0) (get-value state 1)) 1 0)))
    8 (-> state
          (update :ip + 4)
          (write-memory (get-param state 2) (if (= (get-value state 0) (get-value state 1)) 1 0)))
    nil))

(defn run-intcode [state]
  (->> (iterate step-intcode state) (take-while some?) last))
