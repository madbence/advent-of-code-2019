(ns aoc.d11
  (:require [aoc.intcode :refer [->intcode run-intcode-while]]))

(defn step [robot dir]
  (let [dir' (case dir
               0 (case (:dir robot)
                   :u :l
                   :l :d
                   :d :r
                   :r :u)
               1 (case (:dir robot)
                   :u :r
                   :r :d
                   :d :l
                   :l :u))
        pos' (case dir'
               :u (update (:pos robot) :y inc)
               :l (update (:pos robot) :x dec)
               :d (update (:pos robot) :y dec)
               :r (update (:pos robot) :x inc))]
    {:pos pos' :dir dir'}))

(defn a [input]
  (loop [robot {:pos {:x 0 :y 0} :dir :u}
         m {}
         state (->intcode input)]
    (let [state' (-> state
                     (update :input conj (get m (:pos robot) 0))
                     (assoc :output '())
                     (run-intcode-while #(and (some? %) (< (count (:output %)) 2))))
          d (first (:output state'))
          c (second (:output state'))]
      ;(println robot (count m) (:output state') (step robot d) m)
      (if (nil? state')
        (count m)
        (recur (step robot d) (assoc m (:pos robot) c) state')))))

(defn dump-identifier [m]
  (let [minx (->> m (map (comp :x first)) (reduce min))
        maxx (->> m (map (comp :x first)) (reduce max))
        miny (->> m (map (comp :y first)) (reduce min))
        maxy (->> m (map (comp :y first)) (reduce max))
        w (- maxx minx -1)
        h (- maxy miny -1)]
    (->> (for [y (range 0 h)
               x (range 0 w)] {:x (+ minx x) :y (+ miny y)})
         (map #(get m % 0))
         (map #(if (zero? %) \ \X))
         reverse
         (partition w)
         (map reverse)
         (map vec)
         (map #(conj % \newline))
         (map #(apply str %)))))

(defn b [input]
  (loop [robot {:pos {:x 0 :y 0} :dir :u}
         m {{:x 0 :y 0} 1}
         state (->intcode input)]
    (let [state' (-> state
                     (update :input conj (get m (:pos robot) 0))
                     (assoc :output '())
                     (run-intcode-while #(and (some? %) (< (count (:output %)) 2))))
          d (first (:output state'))
          c (second (:output state'))]
      ;(println robot (count m) (:output state') (step robot d) m)
      (if (nil? state')
        (dump-identifier m)
        (recur (step robot d) (assoc m (:pos robot) c) state')))))
