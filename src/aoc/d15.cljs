(ns aoc.d15
  (:require [aoc.intcode :refer [->intcode run-intcode-while]]))

(defn get-reply [state in]
  (run-intcode-while (-> state
                         (assoc :input [in])
                         (assoc :output '())) #(empty? (:output %))))

(defn step [pos dir]
  (case dir
    1 (update pos :y inc)
    2 (update pos :y dec)
    3 (update pos :x dec)
    4 (update pos :x inc)))

(defn step-robot [m robot pos dir]
  (let [robot' (get-reply robot dir)
        reply (first (:output robot'))
        pos' (if (= 0 reply) pos (step pos dir))]
    [(assoc m (step pos dir) reply)
     robot'
     pos'
     reply]))

(defn left [dir]
  (case dir
    1 3
    2 4
    3 2
    4 1))

(defn right [dir]
  (case dir
    1 4
    2 3
    3 1
    4 2))

(defn display-map [m pos]
  (print "\033[1;1H")
  (doseq [y (range -19 22)]
    (doseq [x (range -21 20)]
      (cond
        (= pos {:x x :y y}) (print \D)
        (= 0 (get m {:x x :y y} 1)) (print \#)
        (= 2 (get m {:x x :y y} 1)) (print \X)
        :else (print \.)))
    (print \newline)))

(defn expore [state]
  (loop [state state
         pos {:x 0 :y 0}
         m {pos 1}
         dir 1]
    ; (display-map m pos)
    (let [to-left (step pos (left dir))
          forward (step pos dir)
          left-tile (get m to-left 1)
          forward-tile (get m forward 1)
          final-dir (cond (> left-tile 0) (left dir)
                          (> forward-tile 0) dir
                          :else (right dir))
          [m' state' pos' reply] (step-robot m state pos final-dir)]
      (case reply
        0 (recur state' pos' m' (right dir))
        1 (recur state' pos' m' final-dir)
        2 [pos' m']))))

(defn expore-full [state]
  (loop [state state
         pos {:x 0 :y 0}
         m {pos 1}
         dir 1]
    ;(display-map m pos)
    (let [to-left (step pos (left dir))
          forward (step pos dir)
          left-tile (get m to-left 1)
          forward-tile (get m forward 1)
          final-dir (cond (> left-tile 0) (left dir)
                          (> forward-tile 0) dir
                          :else (right dir))
          [m' state' pos' reply] (step-robot m state pos final-dir)]
      (if (and (not= pos pos') (= pos' {:x 0 :y 0}))
        m'
        (case reply
          0 (recur state' pos' m' (right dir))
          1 (recur state' pos' m' final-dir)
          2 (recur state' pos' m' final-dir))))))

(defn find-path-length [from [target m]]
  (loop [queue [from]
         discovered #{from}
         path {}]
    ; (display-map m (first queue))
    (if (empty? queue)
      nil
      (let [pos (first queue)
            neighbours (->> (range 1 5) (map #(step pos %)) (filter #(not (contains? discovered %))) (filter #(< 0 (get m % 0))))]
        (if (= pos target)
          (loop [p [target]]
            (if (= from (get path (last p)))
              (count p)
              (recur (conj p (get path (last p))))))
          (recur (into (rest queue) neighbours)
                 (into discovered neighbours)
                 (into path (map (fn [n] [n pos]) neighbours))))))))

(defn find-longest-path [m]
  (let [target (first (first (filter #(= 2 (second %)) m)))]
    (->> m
         (filter #(= 1 (second %)))
         (map #(find-path-length target [(first %) m]))
         (reduce max))))

(defn a [input]
  (->> (->intcode input)
       expore
       (find-path-length {:x 0 :y 0})))

(defn b [input]
  (->> (->intcode input)
       expore-full
       find-longest-path))
