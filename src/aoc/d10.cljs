(ns aoc.d10)

(defn get-dir [a b]
  (map - a b))

(defn directions [asteroids]
  (for [a asteroids]
    (for [b asteroids :when (not= a b)]
      (let [[x y] (get-dir a b)]
        (.atan2 js/Math x y)))))

(defn a [input]
  (->> (seq input)
       (filter #(or (= % \#) (= % \.)))
       (map vector (for [x (range 0 31) y (range 0 31)] [y x]))
       (filter #(= (second %) \#))
       (map first)
       directions
       (map #(into #{} %))
       (map count)
       (reduce max)))
