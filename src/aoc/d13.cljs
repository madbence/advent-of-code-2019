(ns aoc.d13
  (:require [aoc.intcode :refer [->intcode run-intcode-until-input run-intcode step-intcode]]))

(defn ->map [output]
  (->> (partition 3 output)
       (map #(vector [(nth % 2) (nth % 1)] (nth % 0)))
       (reduce conj {})))

(defn display-map [m]
  (doseq [y (range 0 23)]
    (doseq [x (range 0 37)]
      (print (get m [x y])))
    (print \newline)))

(defn ball-position [state]
  (->> (partition 3 (:output state))
       (filter #(= 4 (first %)))
       (first)
       ((fn [[t y x]] x))))

(defn paddle-position [state]
  (->> (partition 3 (:output state))
       (filter #(= 3 (first %)))
       (first)
       ((fn [[t y x]] x))))

(defn run-game [state]
  (loop [state (assoc-in state [:memory 0] 2)
         m {}]
    (let [state' (-> state
                     (assoc :output '())
                     run-intcode-until-input)
          ball (ball-position state')
          paddle (paddle-position state')
          input (cond (= ball paddle) 0 (< ball paddle) -1 :else 1)
          state'' (step-intcode (assoc state' :input (list input)))
          m' (merge m (->map (:output state'')))]
      ; (print "\033[1;1H")
      ; (display-map m')
      ; (println (get m' [-1 0]))
      (if (nil? state')
        (let [state''' (run-intcode state)
              m'' (merge m' (->map (:output state''')))]
          (get m'' [-1 0]))
        (recur state'' m')))))

(defn a [input]
  (->> (->intcode input)
       run-intcode
       :output
       (partition 3)
       (filter #(= 2 (nth % 0)))
       count))

(defn b [input]
  (->> (->intcode input)
       run-game))
