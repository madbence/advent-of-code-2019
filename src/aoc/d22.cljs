(ns aoc.d22)

(def l 10007)

(defn deal-into-new-stack [line stack]
  (if (= line "deal into new stack")
    (vec (reverse stack))
    stack))

(defn cut-at [line stack]
  (if-let [m (.match line #"cut (-?\d+)")]
    (let [n (js/parseInt (second m) 10)]
      (if (neg? n)
        (vec (concat (drop (+ l n) stack) (take (+ l n) stack)))
        (vec (concat (drop n stack) (take n stack)))))
    stack))

(defn deal-with-increment [line stack]
  (if-let [m (.match line #"deal with increment (\d+)")]
    (let [n (js/parseInt (second m))]
      (->> (->> (iterate (partial + n) 0)
                (take l)
                (map #(mod % l))
                (map vector (iterate inc 0))
                (sort-by second)
                (map first))
           (mapv #(nth stack %))))
    stack))

(defn a [input]
  (->> (.split input "\n")
       drop-last
       (reduce #(->> %1
                     (deal-into-new-stack %2)
                     (cut-at %2)
                     (deal-with-increment %2)) (range 0 l))
       (mapv vector (iterate inc 0))
       (some #(when (= (second %) 2019) (first %)))))
