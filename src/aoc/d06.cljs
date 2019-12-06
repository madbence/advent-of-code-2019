(ns aoc.d06)

(defn add-to-map [m [a b]]
  (assoc m b a))

(defn get-length [m n]
  (if (= n "COM")
    0
    (inc (get-length m (get m n)))))

(defn get-orbit-lengths [m]
  (map #(get-length m (first %)) m))

(defn a [input]
  (->> (.split input #"\n")
       (drop-last)
       (map #(.split % ")"))
       (map vec)
       (reduce add-to-map {})
       get-orbit-lengths
       (reduce +)))
