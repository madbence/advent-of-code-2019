(ns aoc.core
  (:require ["fs" :as fs]))

(defn read-input [day]
  (fs/readFileSync (str "./" day ".txt") "utf-8"))

(defn ->lines [input]
  (filter js/Boolean (-> input (.split "\n"))))

(defn ->int [n]
  (js/parseInt n 10))

(defn ->fuel [n]
 (- (.floor js/Math (/ n 3)) 2))

(defn ->fuel-all [n]
  (loop [fuel (->fuel n)
         sum 0]
    (if (neg? fuel)
      sum
      (recur
        (->fuel fuel)
        (+ sum fuel)))))

(defn d01a [input]
  (->> input
       (->lines)
       (map ->int)
       (map ->fuel)
       (reduce #(+ %1 %2) 0)))

(defn d01b [input]
  (->> input
       (->lines)
       (map ->int)
       (map ->fuel-all)
       (reduce #(+ %1 %2) 0)))

(defn step-intcode [pos code]
  (case (get code pos)
    99 nil
    1 (assoc code (get code (+ pos 3))
                  (+ (get code (get code (+ pos 1)))
                     (get code (get code (+ pos 2)))))
    2 (assoc code (get code (+ pos 3))
                  (* (get code (get code (+ pos 1)))
                     (get code (get code (+ pos 2)))))))

(defn run-intcode [noun verb code]
  (loop [code (-> code (assoc 1 noun) (assoc 2 verb))
         pos 0]
    (let [next (step-intcode pos code)]
      (if (nil? next)
        code
        (recur next (+ pos 4))))))

(defn d02a [input]
  (->> (-> input (.split ","))
       (map ->int)
       (into [])
       (run-intcode 12 2)
       first))

(defn find-intcode [target code]
  (->> (for [noun (range 0 100)
             verb (range 0 100)] [noun verb])
       (some (fn [[noun verb]]
               (if (= target (-> (run-intcode noun verb code) first))
                 [noun verb]
                 nil)))))

(defn d02b [input]
  (->> (-> input (.split ","))
       (map ->int)
       (into [])
       (find-intcode 19690720)
       ((fn [[noun verb]] (+ (* noun 100) verb)))))

(defn -main [& args]
  (let [days {:d01a d01a
              :d01b d01b
              :d02a d02a
              :d02b d02b}
        day (keyword (str (first args) (second args)))
        input (read-input (first args))]
    (println ((day days) input))))
