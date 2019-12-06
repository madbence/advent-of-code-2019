(ns aoc.d06)

(defn add-to-map [m [a b]]
  (assoc m b a))

(defn get-length [m n]
  (if (= n "COM")
    0
    (-> (get-length m (get m n)) inc)))

(defn get-orbit-lengths [m]
  (map #(get-length m (first %)) m))

(defn get-neighbours [graph node]
  (into (if (= node "COM") [] [(get graph node)])
        (->> graph
             (filter (fn [[a b]] (= b node)))
             (map first))))

(defn bfs [from to graph]
  (loop [visited #{}
         queue [[from]]]
    (let [path (first queue)
          curr (last path)]
      (if (= curr to)
        (- (count path) 3)
        (recur (conj visited curr)
               (into (rest queue) (->> (get-neighbours graph curr)
                                       (filter #(not (contains? visited %)))
                                       (map #(conj path %)))))))))

(defn a [input]
  (->> (.split input #"\n")
       (drop-last)
       (map #(.split % ")"))
       (map vec)
       (reduce add-to-map {})
       get-orbit-lengths
       (reduce +)))

(defn b [input]
  (->> (.split input #"\n")
       (drop-last)
       (map #(.split % ")"))
       (map vec)
       (reduce add-to-map {})
       (bfs "YOU" "SAN")))
